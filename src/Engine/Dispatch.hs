{-# LANGUAGE OverloadedStrings #-}
-- | The event dispatcher (docs/handlers.md section 4), after Showdown's @sim/battle.ts@: @runEvent@,
-- @singleEvent@, @fieldEvent@ and @eachEvent@, with the handler collection (@findEventHandlers@), the sort keys
-- (@resolvePriority@) and the call-time gates.
--
-- Handlers are the status, volatile, ability and item effects of a Pokemon, and the side conditions and field
-- conditions (weather, terrain, pseudo-weather); the hooks where species, slot-condition and format handlers are
-- collected are marked and empty. The functions the natives and the turn loop share (damage, healing, fainting,
-- stat stages, side conditions) live here too, because @fieldEvent@ resolves faints after every handler.
module Engine.Dispatch
  ( Eng
  , runEvent
  , runEventCrit
  , singleEvent
  , fieldEvent
  , eachEvent
  , getAllActive
  , getActionSpeed
  , updateSpeedAll
  , Owner (..)
  , addVolatile
  , useItem
  , effectiveTerrain
  , addSideCondition
  , removeSideCondition
  , addPseudoWeather
  , setWeather
  , setTerrain
  , runStatusImmunity
  , effectiveWeather
  , boostMon
  , clampStage
  , trySetStatus
  , clearStatus
  , resolveFaints
  , endAbilityAndItem
  , checkWin
  , win
  , applyDamage
  , healMon
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify')
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine.Chance
import Engine.Damage (hasItemNamed, hasTypeNamed, isGrounded, isSemiInvulnerable, statusKey, typeIdNamed, typeImmuneTo, typeIsNamed, typeMod)
import Engine.Dex
import Engine.Event
import Engine.Modifier (baseMod, chainModify, modify)
import Engine.Rule (BoostStat (..), MoveRule (..), StatusId (..))
import Engine.State
import Engine.Stats (actionSpeed, boostedStat, parSpe)

-- | The engine monad: battle state over the chance monad.
type Eng m = StateT Battle m

-- ---------------------------------------------------------------------------
-- Effects on a Pokemon

-- | The condition of a status, if it has one ('StatusNone' and the @fnt@ marker have no handlers).
statusCond :: MonStatus -> Maybe CondId
statusCond s = case s of
  StatusBrn -> Just CBrn
  StatusPar -> Just CPar
  StatusPsn -> Just CPsn
  StatusTox -> Just CTox
  StatusFrz -> Just CFrz
  StatusNone -> Nothing
  StatusFainted -> Nothing

-- | Where an effect's state lives: on a Pokemon, in a side's @sideConditions@, or in the @field@ (weather, terrain,
-- pseudo-weather: the 'EffectKind' of the key says which).
data Owner = OwnMon !Actor | OwnSide !Player | OwnField
  deriving (Eq, Show)

-- | The state of an effect a Pokemon currently has, or 'Nothing' when it does not (the status is another one, the
-- volatile is gone).
effectState :: Mon -> EffectKey -> Maybe EffectState
effectState m (EffectKey kind i) = case kind of
  EffStatus
    | fmap fromEnum (statusCond (monStatus m)) == Just i -> Just (monStatusState m)
    | otherwise -> Nothing
  EffVolatile -> case [st | Volatile j st <- SV.toList (monVolatiles m), j == i] of
    (st : _) -> Just st
    [] -> Nothing
  EffAbility -> if monAbility m == i then Just (monAbilityState m) else Nothing
  EffItem -> if monItem m == i then Just (monItemState m) else Nothing
  _ -> Nothing

-- | Replace the state of an effect a Pokemon has.
setEffectState :: EffectKey -> EffectState -> Mon -> Mon
setEffectState (EffectKey kind i) st m = case kind of
  EffStatus -> if fmap fromEnum (statusCond (monStatus m)) == Just i then m {monStatusState = st} else m
  EffVolatile -> m {monVolatiles = SV.map (\v@(Volatile j _) -> if j == i then Volatile j st else v) (monVolatiles m)}
  EffAbility -> if monAbility m == i then m {monAbilityState = st} else m
  EffItem -> if monItem m == i then m {monItemState = st} else m
  _ -> m

-- | The state of an effect in its owner, or 'Nothing' when it is not there (Showdown's @sideConditions[id]@,
-- @weatherState@, @terrainState@ and @pseudoWeather[id]@ lookups).
ownerState :: Battle -> Owner -> EffectKey -> Maybe EffectState
ownerState b owner key@(EffectKey kind i) = case owner of
  OwnMon a -> effectState (monOf b a) key
  OwnSide p -> entryState (sConds (sideOf p b))
  OwnField -> case kind of
    EffWeather -> if fWeather f == i then Just (fWeatherState f) else Nothing
    EffTerrain -> if fTerrain f == i then Just (fTerrainState f) else Nothing
    _ -> entryState (fPseudo f)
  where
    f = bField b
    entryState es = case [st | CondEntry j st <- SV.toList es, j == i] of
      (st : _) -> Just st
      [] -> Nothing

-- | Replace the state of an effect in its owner.
setOwnerState :: Owner -> EffectKey -> EffectState -> Battle -> Battle
setOwnerState owner key@(EffectKey kind i) st b = case owner of
  OwnMon a -> modifyMon a (setEffectState key st) b
  OwnSide p -> modifySide p (\s -> s {sConds = setEntry (sConds s)}) b
  OwnField -> case kind of
    EffWeather -> b {bField = (bField b) {fWeatherState = st}}
    EffTerrain -> b {bField = (bField b) {fTerrainState = st}}
    _ -> b {bField = (bField b) {fPseudo = setEntry (fPseudo (bField b))}}
  where
    setEntry = SV.map (\e@(CondEntry j _) -> if j == i then CondEntry j st else e)

-- | @pokemon.removeVolatile@: the @End@ event of the volatile has no handler here.
removeVolatile :: Int -> Mon -> Mon
removeVolatile i m = m {monVolatiles = SV.filter (\(Volatile j _) -> j /= i) (monVolatiles m)}

-- | @pokemon.setStatus('')@ through @clearStatus@ / @cureStatus@: the status goes with its state.
clearStatus :: Mon -> Mon
clearStatus m = m {monStatus = StatusNone, monStatusState = noEffectState}

-- | @pokemon.cureStatus()@: needs HP and a status.
cureStatus :: Actor -> Battle -> Battle
cureStatus a = modifyMon a (\x -> if monHp x > 0 && monStatus x /= StatusNone then clearStatus x else x)

-- | @pokemon.addVolatile(id)@ (pokemon.ts 1995-2040) of a condition with no @onRestart@: needs HP (no @affectsFainted@)
-- and fails when the volatile is already there. Then @runStatusImmunity(id)@ (no type is immune to a volatile, but
-- the @Immunity@ event runs, and Showdown asks it first) and @runEvent('TryAddVolatile', pokemon, source, effect,
-- status)@ (Misty Terrain's confusion block, Electric Terrain's and Leaf Guard's yawn block; a secondary's volatile
-- reaches it too), either falsy failing it. The state gets the condition's duration; the @Start@ event runs (and,
-- returning false, cancels it).
addVolatile :: MonadChance m => Dex -> Actor -> CondId -> Eng m Bool
addVolatile dx a c = addVolatileFx dx a c Nothing

-- | 'addVolatile' with the effect that adds it (@sourceEffect@: `effect?.name === 'Booster Energy'` in the Paradox
-- volatiles' @onStart@).
addVolatileFx :: MonadChance m => Dex -> Actor -> CondId -> Maybe EffectKey -> Eng m Bool
addVolatileFx dx a c eff = do
  m <- gets (`monOf` a)
  if monHp m <= 0 || isJust (effectState m key)
    then pure False
    else do
      immune <- runStatusImmunity dx a (condName c)
      allowed <- if immune then relayTruthy <$> runEvent dx EvTryAddVolatile a Nothing Nothing (RText (condName c)) else pure False
      if not allowed
        then pure False
        else do
          -- this.volatiles[status.id] = this.battle.initEffectState({ id, name, target: this }) (pokemon.ts 2021)
          order <- stateOrderFor a
          modify' (modifyMon a (\x -> x {monVolatiles = SV.snoc (monVolatiles x) (Volatile (fromEnum c) noEffectState {esOrder = order, esDuration = cnDuration (conditionOf dx c)})}))
          r <- singleEventFx dx EvStart key (OwnMon a) Nothing Nothing eff RNone
          if relayTruthy r
            then pure True
            else modify' (modifyMon a (removeVolatile (fromEnum c))) >> pure False
  where
    key = EffectKey EffVolatile (fromEnum c)

-- | @pokemon.trySetStatus(status)@ -> @setStatus(this.status || status)@ (pokemon.ts) of a Pokemon, with no
-- ability or field effect: fails with no HP, off the field, with any status already (including the same
-- one) or when the type is immune; otherwise sets the status and its fresh @statusState@ (toxic's @onStart@
-- sets the stage to 0). @SetStatus@ and @AfterSetStatus@ have no handlers (draw nothing).
trySetStatus :: MonadChance m => Dex -> Actor -> StatusId -> Eng m Bool
trySetStatus dx a st = do
  b <- get
  let m = monOf b a
      has = monStatus m `elem` [StatusBrn, StatusPar, StatusPsn, StatusTox, StatusFrz]
  -- setStatus: `this.runStatusImmunity(status.id === 'tox' ? 'psn' : status.id)`: the type table, then the Immunity event
  -- (Sunny Day's frz, Sand Rush's sandstorm), which only runs when the type does not already block it
  ok0 <- if monHp m <= 0 || not (monActive m) || has || typeImmuneTo dx (statusKey st) m
    then pure False
    else runStatusImmunity dx a (statusKey st)
  -- `runEvent('SetStatus', this, source, sourceEffect, status)`: a falsy result fails (Misty Terrain, Leaf Guard, Electric
  -- Terrain's sleep). It runs before the state is created, and `AfterSetStatus` (no handler) after it.
  ok <- if ok0 then relayTruthy <$> runEvent dx EvSetStatus a Nothing Nothing (RText (statusId st)) else pure False
  if not ok
    then pure False
    else do
      -- this.status = status.id; this.statusState = initEffectState({id, target: this}) (pokemon.ts 1754);
      -- `if (status.duration) duration`
      order <- stateOrderFor a
      modify' (modifyMon a (\x -> x {monStatus = monStatusOf st, monStatusState = noEffectState {esOrder = order, esDuration = cnDuration (conditionOf dx (condOf st))}}))
      -- singleEvent('Start', status, statusState, this, source, sourceEffect): toxic's onStart sets the stage to 0
      relayTruthy <$> singleEvent dx EvStart (EffectKey EffStatus (fromEnum (condOf st))) (OwnMon a) Nothing Nothing RNone
  where
    statusId s = case s of
      Brn -> "brn"
      Par -> "par"
      Psn -> "psn"
      Tox -> "tox"
      Frz -> "frz"
    monStatusOf s = case s of
      Brn -> StatusBrn
      Par -> StatusPar
      Psn -> StatusPsn
      Tox -> StatusTox
      Frz -> StatusFrz
    condOf s = case s of
      Brn -> CBrn
      Par -> CPar
      Psn -> CPsn
      Tox -> CTox
      Frz -> CFrz

-- | @pokemon.runStatusImmunity(type)@ (pokemon.ts 2299): false for a fainted Pokemon, when a type of it is immune to the
-- key (@dex.getImmunity@: types.json @otherDamageTaken@), or when the @Immunity@ event returns false (the
-- handlers: Sunny Day's @frz@, Sand Rush's @sandstorm@). The relay value is the key.
runStatusImmunity :: MonadChance m => Dex -> Actor -> T.Text -> Eng m Bool
runStatusImmunity dx a key = do
  b <- get
  let m = monOf b a
  if monFainted m
    then pure False
    else if typeImmuneTo dx key m
      then pure False
      else relayTruthy <$> runEvent dx EvImmunity a Nothing Nothing (RText key)

-- | @pokemon.ignoringAbility()@ (pokemon.ts 879): Gastro Acid and Neutralizing Gas, or a Pokemon not in play.
-- LATENT: no supported effect suppresses an ability, and the callers check the Pokemon is active themselves, so this
-- is always false.
ignoringAbility :: Battle -> Actor -> Bool
ignoringAbility _ _ = False

-- | @pokemon.getBestStat(false, true)@ (pokemon.ts 677): the index (0 atk, 1 def, 2 spa, 3 spd, 4 spe) of the first stat
-- with the greatest value after its stage (@getStat@ with @unboosted@ false and @unmodified@ true: no
-- @ModifyBoost@ and no stat modifier events), compared with @>@ from 0, so a tie goes to the earlier stat.
-- LATENT: with Wonder Room, @getStat(..., unmodified)@ swaps the defensive stats; Wonder Room is not supported.
bestStatIx :: Mon -> Int
bestStatIx m = fst (foldl pick (0, 0) (zip [0 ..] values))
  where
    st = monStats m
    bs = monBoosts m
    values = [boostedStat (stAtk st) (bsAtk bs), boostedStat (stDef st) (bsDef bs), boostedStat (stSpa st) (bsSpa bs), boostedStat (stSpd st) (bsSpd bs), boostedStat (stSpe st) (bsSpe bs)]
    pick (bi, bv) (i, v) = if v > bv then (i, v) else (bi, bv)

-- | @field.effectiveTerrain()@ (field.ts 177): the terrain after @runEvent('TryTerrain', target)@, which no supported
-- handler listens to. 'Nothing' is @''@.
-- LATENT: @TryTerrain@ (Gravity-like effects, abilities) has no handler, so it is not run.
effectiveTerrain :: Battle -> Maybe Int
effectiveTerrain b = if fTerrain (bField b) == noCond then Nothing else Just (fTerrain (bField b))

-- | @pokemon.useItem(source, sourceEffect)@ (pokemon.ts 1833-1871) as far as the supported items reach. A fainted or
-- inactive Pokemon, or one with no item, cannot; nor one an item tells to eat a different item than it holds.
-- @runEvent('UseItem')@ (no handler) can refuse it. Then the item's @boosts@ go through @battle.boost@ (source the
-- Pokemon, effect the item), @singleEvent('Use')@ runs (no handler), the item and its state are cleared, and
-- @runEvent('AfterUseItem')@ runs (Unburden). Returns whether the item was used.
-- LATENT: gems (an item that works at 0 HP), @lastItem@ (Recycle), @usedItemThisTurn@ and @usedItemThisBattle@ are not
-- in the state: nothing supported reads them and the snapshot does not record them. @red card@'s log differs, and no
-- berry is supported.
useItem :: MonadChance m => Dex -> Actor -> Maybe EffectKey -> Eng m Bool
useItem dx a sourceEffect = do
  b <- get
  let m = monOf b a
      itemKey = EffectKey EffItem (monItem m)
  if monHp m <= 0 || not (monActive m) || monItem m == noItem
    then pure False
    else case sourceEffect of
      -- `sourceEffect?.effectType === 'Item' && this.item !== sourceEffect.id && source === this`: the source is the event's
      -- target, the Pokemon itself, in every supported caller
      Just (EffectKey EffItem i) | i /= monItem m -> pure False
      _ -> do
        ok <- relayTruthy <$> runEvent dx EvUseItem a Nothing Nothing (RText (itemId dx (monItem m)))
        if not ok
          then pure False
          else do
            -- `if (item.boosts) this.battle.boost(item.boosts, this, source, item)`
            let boosts = maybe [] cnItemBoosts (effectOf dx itemKey)
            unless (null boosts) (void (boostMon a boosts))
            -- singleEvent('Use', item, itemState, ...): no item has an onUse; then the item and its state go
            modify' (modifyMon a (\x -> x {monItem = noItem, monItemState = noEffectState}))
            void (runEvent dx EvAfterUseItem a Nothing Nothing (RText (itemId dx (monItem m))))
            pure True

-- | @field.suppressingWeather()@ (field.ts 101): an active Pokemon with an ability that suppresses weather (Air Lock,
-- Cloud Nine). The gates of @singleEvent@ (battle.ts 624-627) and @runEvent@ (897-900) and 'effectiveWeather' read it.
-- LATENT: no supported ability has @suppressWeather@, so this is always false.
suppressingWeather :: Battle -> Bool
suppressingWeather _ = False

-- | @field.effectiveWeather()@: the weather, unless it is suppressed. 'Nothing' is @''@.
effectiveWeather :: Battle -> Maybe Int
effectiveWeather b
  | suppressingWeather b = Nothing
  | fWeather (bField b) == noCond = Nothing
  | otherwise = Just (fWeather (bField b))

-- | @pokemon.effectiveWeather()@ (pokemon.ts 2212): the field's, except that a Pokemon holding Utility Umbrella ignores
-- sun and rain.
-- LATENT: Utility Umbrella (an unsupported item) and Mega Sol (an unsupported ability) are not consulted.
effectiveWeatherOf :: Battle -> Actor -> Maybe Int
effectiveWeatherOf b _ = effectiveWeather b

-- | @field.setWeather(status, source, sourceEffect)@ (field.ts 39-100) by a source Pokemon, from an ability (@isAbility@)
-- or a move. The same weather already in place fails (gen > 5 for an ability, gen > 2 otherwise). @SetWeather@ runs on
-- the source (no handler) and a falsy result fails. The state is created with no target (so no @effectOrder@: field.ts
-- 70) and the duration of the condition, then the @durationCallback@ (the rock); @FieldStart@ runs and a falsy result
-- restores the previous weather. @eachEvent('WeatherChange')@ follows (no handler yet, but it sorts).
setWeather :: MonadChance m => Dex -> Int -> Actor -> Bool -> Eng m Bool
setWeather dx ci source isAbility = do
  b <- get
  let f = bField b
      key = EffectKey EffWeather ci
  -- `if (this.weather === status.id)`: an Ability source returns false when `gen > 5 || duration === 0`, any other source
  -- when `gen > 2 || status.id === 'sandstorm'`; the format is gen 9
  let gen = 9 :: Int
      sameRefused = if isAbility then gen > 5 else gen > 2
  if fWeather f == ci && sameRefused
    then pure False
    else do
      r <- runEvent dx EvSetWeather source (Just source) Nothing RNone
      if not (relayTruthy r)
        then pure False
        else do
          modify' (\bb -> bb {bField = (bField bb) {fWeather = ci, fWeatherState = noEffectState {esDuration = maybe 0 cnDuration (effectOf dx key)}}})
          setDurationFromCallback dx key OwnField source
          started <- singleEvent dx EvStart key OwnField (Just source) Nothing RNone
          if relayTruthy started
            then eachEvent dx EvWeatherChange >> pure True
            else do
              modify' (\bb -> bb {bField = (bField bb) {fWeather = fWeather f, fWeatherState = fWeatherState f}})
              pure False

-- | @battle.initEffectState@ for a state with an id whose target is a Pokemon (battle.ts 3381-3392): the next
-- @effectOrder@ when the Pokemon is active, else 0 and no increment.
stateOrderFor :: Monad m => Actor -> Eng m Int
stateOrderFor a = do
  b <- get
  if monActive (monOf b a)
    then do
      let (o, b') = takeEffectOrder b
      modify' (const b')
      pure o
    else pure 0

-- | @side.addSideCondition@ (side.ts 420-445) of a field-condition id by a source Pokemon. A condition the side
-- already has restarts through its @onSideRestart@ (no handler: the move fails); otherwise its state is created
-- (@initEffectState@ with the side as target, so the counter advances) with the condition's duration, and
-- @SideStart@ runs; a falsy result deletes it again.
-- LATENT: @runEvent('SideConditionStart')@ has no supported handler, so it is not run.
addSideCondition :: MonadChance m => Dex -> Player -> Int -> Actor -> Eng m Bool
addSideCondition dx p ci source = do
  b <- get
  let key = EffectKey EffSideCond ci
      owner = OwnSide p
  case ownerState b owner key of
    Just _ -> case effectOf dx key >>= \cn -> condSpec cn PSide EvRestart >>= hsBody of
      Nothing -> pure False -- `if (!status.onSideRestart) return false`
      Just body -> relayTruthy <$> singleBody dx EvRestart body key owner (activeActor p b) (Just source) Nothing RNone
    Nothing -> do
      let (order, b') = takeEffectOrder b
          duration = maybe 0 cnDuration (effectOf dx key)
      modify' (const (modifySide p (\s -> s {sConds = SV.snoc (sConds s) (CondEntry ci noEffectState {esOrder = order, esDuration = duration})}) b'))
      -- this.sideConditions[status.id].duration = status.durationCallback.call(this.battle, this.active[0], source, sourceEffect)
      setDurationFromCallback dx key owner source
      r <- singleEvent dx EvStart key owner (Just source) Nothing RNone
      if relayTruthy r
        then pure True
        else do
          modify' (modifySide p (\s -> s {sConds = SV.filter (\(CondEntry j _) -> j /= ci) (sConds s)}))
          pure False

-- | @side.removeSideCondition@ (side.ts 460): a condition the side has ends (@singleEvent('SideEnd')@, with the side's
-- state) and goes.
removeSideCondition :: MonadChance m => Dex -> Player -> Int -> Eng m ()
removeSideCondition dx p ci = do
  b <- get
  let key = EffectKey EffSideCond ci
  case ownerState b (OwnSide p) key of
    Nothing -> pure () -- `if (!this.sideConditions[status.id]) return false`
    Just _ -> do
      void (singleEvent dx EvEnd key (OwnSide p) Nothing Nothing RNone)
      modify' (modifySide p (\s -> s {sConds = SV.filter (\(CondEntry j _) -> j /= ci) (sConds s)}))

-- | @field.removePseudoWeather@ (field.ts 223): a pseudo-weather the field has ends (@singleEvent('FieldEnd')@) and goes.
-- It removes by id, and does nothing when it is absent.
removePseudoWeather :: MonadChance m => Dex -> EffectKey -> Eng m ()
removePseudoWeather dx key@(EffectKey _ i) = do
  b <- get
  case ownerState b OwnField key of
    Nothing -> pure ()
    Just _ -> do
      void (singleEvent dx EvEnd key OwnField Nothing Nothing RNone)
      modify' (\bb -> bb {bField = (bField bb) {fPseudo = SV.filter (\(CondEntry j _) -> j /= i) (fPseudo (bField bb))}})

-- | @field.setTerrain(status, source, sourceEffect)@ (field.ts 130-165) by a source Pokemon: the same terrain already in
-- place fails (any source); the state is created with no target (no @effectOrder@) and the condition's duration, then
-- the @durationCallback@ (Terrain Extender); @FieldStart@ runs and a falsy result restores the previous terrain.
-- @eachEvent('TerrainChange')@ follows (no handler until phase 3e, but it sorts). There is no @SetTerrain@ event.
setTerrain :: MonadChance m => Dex -> Int -> Actor -> Eng m Bool
setTerrain dx ci source = do
  b <- get
  let f = bField b
      key = EffectKey EffTerrain ci
  if fTerrain f == ci
    then pure False
    else do
      modify' (\bb -> bb {bField = (bField bb) {fTerrain = ci, fTerrainState = noEffectState {esDuration = maybe 0 cnDuration (effectOf dx key)}}})
      setDurationFromCallback dx key OwnField source
      started <- singleEvent dx EvStart key OwnField (Just source) Nothing RNone
      if relayTruthy started
        then eachEvent dx EvTerrainChange >> pure True
        else do
          modify' (\bb -> bb {bField = (bField bb) {fTerrain = fTerrain f, fTerrainState = fTerrainState f}})
          pure False

-- | @field.clearTerrain@ (field.ts 167): like 'clearWeather', whatever terrain is in place ends, its state is reset
-- keeping its target, and @eachEvent('TerrainChange')@ runs.
clearTerrain :: MonadChance m => Dex -> Eng m ()
clearTerrain dx = do
  b <- get
  let f = bField b
  when (fTerrain f /= noCond) $ do
    void (singleEvent dx EvEnd (EffectKey EffTerrain (fTerrain f)) OwnField Nothing Nothing RNone)
    modify' (\bb -> bb {bField = (bField bb) {fTerrain = noCond, fTerrainState = noEffectState {esHasTarget = esHasTarget (fTerrainState (bField bb))}}})
    eachEvent dx EvTerrainChange

-- | @field.clearWeather@ (field.ts 90): it ignores which weather asked for it (the residual @end@ is called with the
-- holder and the effect id, but @clearWeather@ takes no argument): whatever weather is in place ends
-- (@singleEvent('FieldEnd')@), the weather and its state are reset (@clearEffectState@: no id, no duration, effectOrder 0,
-- but the target stays) and @eachEvent('WeatherChange')@ runs. Nothing happens with no weather.
clearWeather :: MonadChance m => Dex -> Eng m ()
clearWeather dx = do
  b <- get
  let f = bField b
  when (fWeather f /= noCond) $ do
    void (singleEvent dx EvEnd (EffectKey EffWeather (fWeather f)) OwnField Nothing Nothing RNone)
    modify' (\bb -> bb {bField = (bField bb) {fWeather = noCond, fWeatherState = noEffectState {esHasTarget = esHasTarget (fWeatherState (bField bb))}}})
    eachEvent dx EvWeatherChange

-- | @field.addPseudoWeather@ (field.ts 187-212) of a field-condition id by a source Pokemon. A pseudo-weather the field
-- already has restarts through its @onFieldRestart@ (none: the move fails); otherwise its state is created (no
-- target, so no @effectOrder@ and no counter), the @durationCallback@ sets its duration, and @FieldStart@ runs; a
-- falsy result deletes it again.
-- LATENT: @runEvent('PseudoWeatherChange')@ has no supported handler, so it is not run.
addPseudoWeather :: MonadChance m => Dex -> Int -> Actor -> Eng m Bool
addPseudoWeather dx ci source = do
  b <- get
  let key = EffectKey EffPseudoWeather ci
  case ownerState b OwnField key of
    Just _ -> case effectOf dx key >>= \cn -> condSpec cn PField EvRestart >>= hsBody of
      Nothing -> pure False -- `if (!status.onFieldRestart) return false`
      Just body -> relayTruthy <$> singleBody dx EvRestart body key OwnField (holderOf b OwnField) (Just source) Nothing RNone
    Nothing -> do
      modify' (\bb -> bb {bField = (bField bb) {fPseudo = SV.snoc (fPseudo (bField bb)) (CondEntry ci noEffectState {esDuration = maybe 0 cnDuration (effectOf dx key)})}})
      -- state.duration = status.durationCallback.call(this.battle, source, source, sourceEffect)
      setDurationFromCallback dx key OwnField source
      r <- singleEvent dx EvStart key OwnField (Just source) Nothing RNone
      if relayTruthy r
        then pure True
        else do
          modify' (\bb -> bb {bField = (bField bb) {fPseudo = SV.filter (\(CondEntry j _) -> j /= ci) (fPseudo (bField bb))}})
          pure False

-- | The condition's @durationCallback@ (the handler @(POwn, EvDurationCallback)@), if it has one: its result is the
-- state's duration. @source@ is the Pokemon that sets the condition.
-- LATENT: Tailwind's and Trick Room's callbacks read @source.hasAbility('persistent')@ (6 and 7 turns); Persistent is
-- not a supported ability.
setDurationFromCallback :: MonadChance m => Dex -> EffectKey -> Owner -> Actor -> Eng m ()
setDurationFromCallback dx key owner source = do
  b <- get
  case effectOf dx key >>= \cn -> condSpec cn POwn EvDurationCallback >>= hsBody of
    Nothing -> pure ()
    Just body -> do
      Res out _ <- runBody dx body (Ctx (holderOf b owner) owner key (Just source) Nothing baseMod Nothing False) RNone
      case out of
        Replace (RInt d) -> modify' (\bb -> maybe bb (\st -> setOwnerState owner key st {esDuration = d} bb) (ownerState bb owner key))
        _ -> pure ()

-- ---------------------------------------------------------------------------
-- Collection and keys

-- | A collected handler (battle.ts @EventListener@).
data Handler = Handler
  { hKey :: !EffectKey
  , hOwner :: !Owner
  -- ^ Where the effect's state lives (Showdown's @state.target@).
  , hHolder :: !(Maybe Actor)
  -- ^ @effectHolder@ when it is a Pokemon: the Pokemon an effect belongs to, or the @customHolder@ a side or field
  -- condition is collected for. 'Nothing' when the holder is the side or the field.
  , hSpec :: !HandlerSpec
  , hSortKey :: !SortKey
  , hDuration :: !Int
  -- ^ The state's duration when collected (Showdown's @handler.state@ is that object: while it is in place the live
  -- duration is its own, once it is stale only this copy counts down).
  , hHasTarget :: !Bool
  -- ^ A field state's 'esHasTarget' when collected.
  , hStateOrder :: !Int
  -- ^ The @effectOrder@ of the state when collected: the identity of the state object (battle.ts 535-560 compares
  -- the object itself).
  , hEnd :: !Bool
  -- ^ Whether the handler has an @end@ (@customHolder@ handlers of a side or field condition have none).
  }

-- | @battle.getAllActive@: the Pokemon in play, p1 first.
getAllActive :: Bool -> Battle -> [Actor]
getAllActive includeFainted b =
  [ a
  | p <- [P1, P2]
  , let s = sideOf p b
  , sHasActive s
  , let a = activeActor p b
  , includeFainted || not (monFainted (monOf b a))
  ]

-- | @findPokemonEventHandlers@ (battle.ts 1111): the status, then the volatiles in insertion order. With
-- @byDuration@ (@getKey = 'duration'@, @Residual@) an effect whose state has a duration is collected even when it has
-- no callback.
-- Then the ability and the item. Hook: the species and slot-condition handlers are collected here once the state
-- holds them.
-- Keys are those of @resolvePriority@ (957-1032): order, priority and subOrder from the spec, the holder's cached
-- speed (scaled), and for @SwitchIn@ the holder's @effectOrder@ and the half-speed step by its position in
-- @battle.speedOrder@ (an absent position is -1, as @indexOf@).
-- An ability's or item's @onStart@ is also its @onSwitchIn@ when it has none (battle.ts getCallback 1025-1036): the
-- dex table gives it the handler @(POwn, EvSwitchIn)@ ('Engine.Dex.effectFor'), so it is collected here and sorted with
-- the holder's speed and its ability state's @effectOrder@ (the weather setters).
pokemonHandlers :: Dex -> Battle -> Bool -> Prefix -> EventId -> Actor -> [Handler]
pokemonHandlers dx b byDuration pfx ev a =
  statusPart ++ volatilePart ++ one (EffectKey EffAbility (monAbility m)) (monAbilityState m) ++ itemPart
  where
    m = monOf b a
    statusPart = case statusCond (monStatus m) of
      Just c -> one (EffectKey EffStatus (fromEnum c)) (monStatusState m)
      Nothing -> []
    volatilePart = concat [one (EffectKey EffVolatile i) st | Volatile i st <- SV.toList (monVolatiles m)]
    itemPart = if monItem m == noItem then [] else one (EffectKey EffItem (monItem m)) (monItemState m)
    one key st = case effectOf dx key >>= \cn -> condSpec cn pfx ev of
      Just spec | isJust (hsBody spec) || (byDuration && esDuration st /= 0) ->
        [Handler key (OwnMon a) (Just a) spec (sortKey ev spec (monSpeedKey b ev a) st) (esDuration st) False (esOrder st) True]
      _ -> []

-- | @resolvePriority@ (battle.ts 957-1032): order, priority and subOrder from the spec, the speed given, and for
-- @SwitchIn@ the state's @effectOrder@ (the callback names ending in @SwitchIn@ and @RedirectTarget@; @RedirectTarget@
-- is not an event here yet).
sortKey :: EventId -> HandlerSpec -> Int -> EffectState -> SortKey
sortKey ev spec speed st = SortKey
  { skOrder = hsOrder spec
  , skPriority = hsPriority spec
  , skSpeed = speed
  , skSubOrder = hsSubOrder spec
  , skEffectOrder = if ev == EvSwitchIn then esOrder st else 0
  }

-- | The speed of a Pokemon's handlers: its cached @speed@ (scaled), and for @SwitchIn@ the half-speed step by its
-- position in @battle.speedOrder@ (an absent position is -1, as @indexOf@; battle.ts 1027-1030).
monSpeedKey :: Battle -> EventId -> Actor -> Int
monSpeedKey b ev a@(Actor p _) =
  monSpeed (monOf b a) * speedScale - (if ev == EvSwitchIn then fromMaybe (-1) (U.elemIndex (playerIx p) (bSpeedOrder b)) else 0)

-- | How a side's or the field's conditions are collected (battle.ts @findSideEventHandlers@ and
-- @findFieldEventHandlers@, 1155-1200).
data Collect
  = -- | The condition's own @onSide<Event>@ / @onField<Event>@ callback (@fieldEvent@): the holder is the side or the
    -- field, the speed 0, and the handler has an @end@. With the duration key, for @Residual@.
    Owned
  | -- | The plain @on<Event>@ callback for one Pokemon in play (@fieldEvent@'s @customHolder@): the Pokemon is the
    -- holder, so the speed is its speed (with the @SwitchIn@ step); no duration key and no @end@.
    Custom !Actor
  | -- | The plain callback with a prefix (@on@, @onFoe@, @onAny@) as @runEvent@ collects it: the holder is the side or
    -- the field, the speed 0.
    Run !Prefix

-- | The callback prefix, speed, holder and @end@ of a 'Collect'.
collectParts :: Battle -> EventId -> Prefix -> Collect -> (Prefix, Int, Maybe Actor, Bool)
collectParts b ev ownPfx how = case how of
  Owned -> (ownPfx, 0, Nothing, True)
  Custom a -> (POwn, monSpeedKey b ev a, Just a, False)
  Run pfx -> (pfx, 0, Nothing, True)

-- | @findSideEventHandlers@ (battle.ts 1177): the side's conditions in insertion order. A condition with a duration
-- is collected for @Residual@ even without a callback (@getKey = 'duration'@). The key is @resolvePriority@'s
-- (957-1032): the side condition's subOrder (4 unless the export gives one), @effectOrder@ for @SwitchIn@.
sideHandlers :: Dex -> Battle -> Bool -> Collect -> EventId -> Player -> [Handler]
sideHandlers dx b byDuration how ev p =
  [ Handler key (OwnSide p) holder spec (sortKey ev spec speed st) (esDuration st) False (esOrder st) end
  | CondEntry i st <- SV.toList (sConds (sideOf p b))
  , let key = EffectKey EffSideCond i
  , Just cn <- [effectOf dx key]
  , let (pfx, speed, holder, end) = collectParts b ev PSide how
  , Just spec <- [condSpec cn pfx ev]
  , isJust (hsBody spec) || (byDuration && esDuration st /= 0)
  ]

-- | @findFieldEventHandlers@ (battle.ts 1150): the pseudo-weathers in insertion order, then the weather, then the
-- terrain. Their states have no target, so their @effectOrder@ is 0.
fieldHandlers :: Dex -> Battle -> Bool -> Collect -> EventId -> [Handler]
fieldHandlers dx b byDuration how ev =
  concat [one (EffectKey EffPseudoWeather i) st | CondEntry i st <- SV.toList (fPseudo f)]
    ++ [h | fWeather f /= noCond, h <- one (EffectKey EffWeather (fWeather f)) (fWeatherState f)]
    ++ [h | fTerrain f /= noCond, h <- one (EffectKey EffTerrain (fTerrain f)) (fTerrainState f)]
  where
    f = bField b
    one key st = case effectOf dx key of
      Just cn ->
        let (pfx, speed, holder, end) = collectParts b ev PField how
         in case condSpec cn pfx ev of
              Just spec | isJust (hsBody spec) || (byDuration && esDuration st /= 0) ->
                [Handler key OwnField holder spec (sortKey ev spec speed st) (esDuration st) (esHasTarget st) (esOrder st) end]
              _ -> []
      Nothing -> []

-- | @findBattleEventHandlers@ (the format and its rules): none exist yet.
battleHandlers :: Battle -> EventId -> [Handler]
battleHandlers _ _ = []

-- | @battle.findEventHandlers@ (battle.ts 1049-1116) for a Pokemon target: its own handlers first, then, when the
-- event takes prefixed handlers, those of allies and foes, then the source's @onSource@ handlers, the sides' (the
-- target's own side @on@, the other side @onFoe@, both @onAny@, p1 first), the field's @on@ and the format's. An
-- inactive target with no active source contributes nothing, and the side handlers are reached only through an
-- active target (1070).
findEventHandlers :: Dex -> Battle -> EventId -> Actor -> Maybe Actor -> [Handler]
findEventHandlers dx b ev target source =
  own ++ prefixedAroundTarget ++ sourceHandlers ++ sidePart ++ fieldHandlers dx b False (Run POwn) ev ++ battleHandlers b ev
  where
    active a = monActive (monOf b a)
    bubbles = active target || maybe False active source
    prefixed = eventPrefixed ev
    own = if bubbles then pokemonHandlers dx b False POwn ev target else []
    Actor tp _ = target
    -- alliesAndSelf() and foes() are the active Pokemon with HP
    side p = [a | sHasActive (sideOf p b), let a = activeActor p b, monHp (monOf b a) > 0]
    prefixedAroundTarget
      | bubbles && prefixed =
          concat [pokemonHandlers dx b False PAlly ev a ++ pokemonHandlers dx b False PAny ev a | a <- side tp]
            ++ concat [pokemonHandlers dx b False PFoe ev a ++ pokemonHandlers dx b False PAny ev a | a <- side (other tp)]
      | otherwise = []
    sourceHandlers = case source of
      Just s | prefixed -> pokemonHandlers dx b False PSource ev s
      _ -> []
    -- for (const side of this.sides): `on` for the target's side, `onFoe` for the other (prefixed events), `onAny`
    sidePart
      | bubbles =
          concat
            [ (if p == tp then sideHandlers dx b False (Run POwn) ev p else if prefixed then sideHandlers dx b False (Run PFoe) ev p else [])
                ++ (if prefixed then sideHandlers dx b False (Run PAny) ev p else [])
            | p <- [P1, P2]
            ]
      | otherwise = []

-- | Sort handlers by the event's sort kind (battle.ts 797-803). A @speedSort@ draws only for tie runs.
sortHandlers :: MonadChance m => EventId -> [Handler] -> m [Handler]
sortHandlers ev hs = case eventSort ev of
  SpeedSort -> speedSortWith (\x y -> compareKey (hSortKey x) (hSortKey y)) hs
  LeftToRight -> pure (sortBy (\x y -> compareLeftToRight (hSortKey x) (hSortKey y)) hs)
  RedirectSort -> pure (sortBy (\x y -> compareRedirect (hSortKey x) (hSortKey y)) hs)

-- ---------------------------------------------------------------------------
-- The events

-- | What a handler did: returned @undefined@ (keep going), or returned a value (the new relay value).
data Outcome = Keep | Replace !Relay

-- | The return @false@ of a handler.
stop :: Outcome
stop = Replace (RBool False)

-- | A handler's result and the event's modifier after it (@chainModify@ / @finalModify@ change it).
data Res = Res !Outcome !Int

-- | The context a handler runs in: its holder, the event's source Pokemon, its source effect (the move) and the
-- event's modifier. (The event's target is the holder, except for the @onSource@ handlers of @ModifyDamage@,
-- whose holder is the event's source; the natives that care know which.) For a side or field condition the holder
-- is the Pokemon it is collected for (@customHolder@, or the event's target in @runEvent@); an owned callback
-- (@onSideStart@, @onSideResidual@) has no Pokemon, Showdown passes the side or field, and 'cHolder' is then a
-- placeholder (the side's active Pokemon) that natives must not read: they use 'cOwner'.
data Ctx = Ctx
  { cHolder :: !Actor
  , cOwner :: !Owner
  -- ^ Where the running effect's state lives.
  , cKey :: !EffectKey
  , cSource :: !(Maybe Actor)
  , cMove :: !(Maybe MoveData)
  , cMod :: !Int
  , cEffect :: !(Maybe EffectKey)
  -- ^ The effect that caused the event (Showdown's @sourceEffect@ argument of @singleEvent@) when it is not a move: the
  -- Booster Energy that adds a volatile (`effect?.name === 'Booster Energy'`).
  , cCrit :: !Bool
  -- ^ The target's @getMoveHitData(move).crit@ of the hit the event belongs to (@ModifyDamage@: Reflect and Light Screen
  -- do not weaken a critical hit); false for every other event.
  }

-- | @battle.runEvent@ (battle.ts 765-925) on a Pokemon: collect the handlers, sort them, run them with the relay
-- value threaded through (a handler's value replaces it, a falsy one stops the event), and apply the accumulated
-- modifier to an integer result.
--
-- Arguments: the event, the target, the source Pokemon and the source effect (the move, for @BeforeMove@,
-- @ModifyMove@, @DamagingHit@, @AfterMoveSecondary@) and the relay value.
-- Call-time gates skip a handler (battle.ts 870-914): its status is no longer the holder's; a @breakable@
-- ability while the holder's ability is suppressed (Mold Breaker; nothing can suppress yet); an item of a
-- holder that is not active, except for @Start@ and @SwitchIn@ (@ignoringItem@: Klutz, Embargo and Magic Room
-- cannot happen yet); an ability of a holder that is not active (@ignoringAbility@: Gastro Acid and
-- Neutralizing Gas cannot happen yet). The Air Lock gate joins with weather.
-- LATENT: the move's own callbacks (@onEffect@, 787-795), array targets (@AfterMoveSecondary@ and @DamagingHit@
-- pass the target as a one-element array; in singles that is the same as the single target) and @fastExit@ are not
-- modelled.
runEvent :: MonadChance m => Dex -> EventId -> Actor -> Maybe Actor -> Maybe MoveData -> Relay -> Eng m Relay
runEvent dx = runEventCrit dx False

-- | 'runEvent' for an event of a hit whose critical-hit result is known (@ModifyDamage@): the handlers can read it as
-- 'cCrit'.
runEventCrit :: MonadChance m => Dex -> Bool -> EventId -> Actor -> Maybe Actor -> Maybe MoveData -> Relay -> Eng m Relay
runEventCrit dx crit ev target source mv relay0 = do
  b <- get
  let fmt = bFormat b
  sorted <- sortHandlers ev (findEventHandlers dx b ev target source)
  go fmt sorted relay0 baseMod
  where
    -- after the handlers: `typeof relayVar === 'number' && relayVar === Math.abs(Math.floor(relayVar))`
    finish fmt r md = case r of
      RInt v | v >= 0 -> RInt (modify fmt v md)
      _ -> r
    go fmt [] r md = pure (finish fmt r md)
    go fmt (h : hs) r md = do
      b <- get
      if skipped dx ev b h
        then go fmt hs r md
        else case hsBody (hSpec h) of
          Nothing -> go fmt hs r md
          Just body -> do
            -- `this.effectState.target = effectHolder` (battle.ts 908-909) when the callback is a function: a field state
            -- gets its target (the Field) the first time one of its callbacks runs through runEvent
            case (hOwner h, body) of
              (OwnField, BConst _) -> pure ()
              (OwnField, _) -> modify' (\bb -> maybe bb (\st -> setOwnerState OwnField (hKey h) st {esHasTarget = True} bb) (ownerState bb OwnField (hKey h)))
              _ -> pure ()
            -- a side or field handler is called with the event's own arguments: the Pokemon is the event's target
            Res out md' <- runBody dx body (Ctx (fromMaybe target (hHolder h)) (hOwner h) (hKey h) source mv md Nothing crit) r
            case out of
              Keep -> go fmt hs r md'
              Replace r'
                -- priorityEvent (fastExit): `if (!relayVar || fastExit) break` on the first returned value (927-934)
                | relayTruthy r' && not (eventFastExit ev) -> go fmt hs r' md'
                | otherwise -> pure (finish fmt r' md')

-- | The call-time gates of @runEvent@ (battle.ts 870-914), as documented there. They concern a Pokemon's own
-- effects; a side or field condition has none (the weather gate joins with weather).
skipped :: Dex -> EventId -> Battle -> Handler -> Bool
skipped dx ev b h = case (hKey h, hOwner h) of
  (EffectKey EffStatus _, OwnMon a) -> not (isJust (effectState (monOf b a) (hKey h))) -- it's changed
  (EffectKey EffAbility _, OwnMon a) ->
    (maybe False cnBreakable (effectOf dx (hKey h)) && suppressingAbility b a)
      || not (monActive (monOf b a)) -- ignoringAbility, gen >= 5
  (EffectKey EffItem _, OwnMon a) -> ev /= EvStart && ev /= EvSwitchIn && not (monActive (monOf b a)) -- ignoringItem, gen >= 5
  -- battle.ts 897-900: `(effect.effectType === 'Weather' || eventid === 'Weather') && eventid !== 'Residual' && eventid !== 'End'
  -- && this.field.suppressingWeather()` (Air Lock, Cloud Nine: 'suppressingWeather' is always false here)
  (EffectKey kind _, _) | (kind == EffWeather || ev == EvWeather) && ev /= EvResidual && ev /= EvEnd -> suppressingWeather b
  _ -> False

-- | @battle.suppressingAbility(target)@: Mold Breaker and its kin. No ability that suppresses exists yet.
suppressingAbility :: Battle -> Actor -> Bool
suppressingAbility _ _ = False

-- | @battle.singleEvent@ (battle.ts 578-656): run one effect's handler for an event on its owner (a Pokemon, a side or
-- the field; the callback is the effect's own: @onStart@ of a status, @onSideStart@ of a side condition). Returns
-- the relay value, which a handler may replace. Gates (601-628): the status is no longer the holder's; an item of a
-- holder that is not active, except for @Start@ (@TakeItem@ is not an event here); an ability of an inactive holder,
-- except for @End@. The weather gate joins with weather.
singleEvent :: MonadChance m => Dex -> EventId -> EffectKey -> Owner -> Maybe Actor -> Maybe MoveData -> Relay -> Eng m Relay
singleEvent dx ev key owner source mv = singleEventFx dx ev key owner source mv Nothing

-- | 'singleEvent' with the effect that caused it (an item, when it is not a move).
singleEventFx :: MonadChance m => Dex -> EventId -> EffectKey -> Owner -> Maybe Actor -> Maybe MoveData -> Maybe EffectKey -> Relay -> Eng m Relay
singleEventFx dx ev key@(EffectKey kind _) owner source mv eff relay = case effectOf dx key >>= \cn -> condSpec cn (ownPrefix kind) ev >>= hsBody of
  Nothing -> pure relay
  Just body -> do
    b <- get
    singleBodyFx dx ev body key owner (holderOf b owner) source mv eff relay

-- | The Pokemon an owner's callback is called with: the Pokemon itself, or for a side or field a placeholder (see
-- 'Ctx').
holderOf :: Battle -> Owner -> Actor
holderOf b owner = case owner of
  OwnMon a -> a
  OwnSide p -> activeActor p b
  OwnField -> activeActor P1 b

-- | The part of @singleEvent@ after finding the callback. @holder@ is the target the callback is called with: the
-- owner for a Pokemon's effect, the Pokemon a side's condition is collected for in @fieldEvent@.
singleBody :: MonadChance m => Dex -> EventId -> Body -> EffectKey -> Owner -> Actor -> Maybe Actor -> Maybe MoveData -> Relay -> Eng m Relay
singleBody dx ev body key owner holder source mv = singleBodyFx dx ev body key owner holder source mv Nothing

singleBodyFx :: MonadChance m => Dex -> EventId -> Body -> EffectKey -> Owner -> Actor -> Maybe Actor -> Maybe MoveData -> Maybe EffectKey -> Relay -> Eng m Relay
singleBodyFx dx ev body key owner holder source mv eff relay = do
  b0 <- get
  let m = monOf b0 holder
      off = case (key, owner) of
        (EffectKey EffStatus _, OwnMon _) -> not (isJust (effectState m key)) -- it's changed; call it off
        (EffectKey EffItem _, OwnMon _) -> ev /= EvStart && not (monActive m)
        -- battle.ts 621: `eventid !== 'End' && effect.effectType === 'Ability' && target.ignoringAbility()`
        (EffectKey EffAbility _, OwnMon _) -> ev /= EvEnd && not (monActive m)
        -- battle.ts 624-627: a weather's callbacks other than FieldStart, FieldResidual and FieldEnd while the weather is
        -- suppressed (Air Lock, Cloud Nine; 'suppressingWeather' is always false here)
        (EffectKey EffWeather _, _) -> ev /= EvStart && ev /= EvResidual && ev /= EvEnd && suppressingWeather b0
        _ -> False
  if off
    then pure relay
    else do
      Res out _ <- runBody dx body (Ctx holder owner key source mv baseMod eff False) relay
      pure (case out of Keep -> relay; Replace r -> r)

-- | @battle.eachEvent@ (battle.ts 470), for @Update@, @BeforeTurn@, @Weather@ and @WeatherChange@: speed-sort the active Pokemon by their cached
-- @speed@ (ties are shuffled) and run the event on each.
eachEvent :: MonadChance m => Dex -> EventId -> Eng m ()
eachEvent dx ev = do
  b <- get
  let speedOf a = monSpeed (monOf b a)
  actives <- speedSortWith (\x y -> compare (speedOf y) (speedOf x)) (getAllActive False b)
  forM_ actives $ \a -> runEvent dx ev a Nothing Nothing RNone
  -- `if (eventid === 'Weather' && this.gen >= 7) this.eachEvent('Update')` (battle.ts 477)
  when (ev == EvWeather) (eachEvent dx EvUpdate)

-- | @battle.fieldEvent@ (battle.ts 489-577), for @Residual@ and @SwitchIn@. The handlers of the field, then of each
-- side and of each Pokemon in play (fainted ones too: they still take part in the sort, and are skipped when
-- run) are collected, sorted by @comparePriority@ and run one at a time, each followed by @resolveFaints()@.
--
-- Collected as Showdown does (489-512), field first, then side by side, p1 first: the field's own @onField<Event>@
-- callbacks (with the duration key for @Residual@), then for each side its own @onSide<Event>@ callbacks, then for
-- each Pokemon in play its @onAny@ (SwitchIn), and, for the Pokemon in @targets@, its own handlers (status before
-- volatiles), the side's, the field's and the format's plain @on<Event>@ with that Pokemon as @customHolder@ (so
-- they sort at its speed, and have no duration key and no @end@). That second collection is how hazards run: gen 9
-- has no @EntryHazard@ event, so each hazard is a side condition with @onSwitchIn@. @effectOrder@ enters a key only
-- for @SwitchIn@ (@resolvePriority@); a Pokemon's hazards tie on everything else, so the order they were set decides.
-- A handler of a fainted Pokemon is skipped. For @Residual@ a state with a duration and an @end@ counts it down
-- first and the effect ends at 0 (flinch, duration 1, is removed here), skipping the faint resolution of that
-- handler. A handler whose effect is no longer in place is skipped (a hazard absorbed by an earlier switch-in): for
-- a side condition the state is compared by its @effectOrder@, unique to each creation. When a faint ends the battle
-- the remaining handlers do not run.
-- LATENT: for a Pokemon's effect and for the field, "in place" is checked by presence of the effect id, where
-- Showdown compares the state object itself (battle.ts ~530-561). An effect removed and re-added within one
-- fieldEvent would run here and not in Showdown; unreachable while no handler removes and re-adds an effect during
-- Residual or SwitchIn.
-- @targets@ restricts the Pokemon whose own handlers run (@SwitchIn@: the switchers); @onAnySwitchIn@ handlers
-- of every Pokemon in play are collected regardless.
fieldEvent :: MonadChance m => Dex -> EventId -> Maybe [Actor] -> Eng m ()
fieldEvent dx ev targets = do
  b <- get
  let byDuration = ev == EvResidual
      inTargets a = maybe True (a `elem`) targets
      perActive p a =
        (if ev == EvSwitchIn then pokemonHandlers dx b False PAny ev a else [])
          ++ ( if inTargets a
                 then
                   pokemonHandlers dx b byDuration POwn ev a
                     ++ sideHandlers dx b False (Custom a) ev p
                     ++ fieldHandlers dx b False (Custom a) ev
                     ++ battleHandlers b ev
                 else []
             )
      handlers =
        fieldHandlers dx b byDuration Owned ev
          ++ concat
            [ sideHandlers dx b byDuration Owned ev p ++ [h | sHasActive s, h <- perActive p (activeActor p b)]
            | p <- [P1, P2]
            , let s = sideOf p b
            ]
  sorted <- sortHandlers ev handlers
  run sorted
  where
    run [] = pure ()
    run (h : rest) = do
      b <- get
      let key = hKey h
          owner = hOwner h
          -- `(handler.effectHolder as Pokemon).fainted`
          fainted = maybe False (monFainted . monOf b) (hHolder h)
          current = ownerState b owner key
          -- battle.ts 535-560: the state object is still where it was. A side condition is compared by its
          -- effectOrder, unique to each creation; a Pokemon's effect by id. A field state is checked only once it has a
          -- target (548-561; docs/handlers.md "A field state gains a target"): before that the callback runs
          -- whatever has become of the state. The bit is the live state's, or the collected one's when it is gone.
          -- LATENT: Showdown compares the state object itself for a Pokemon's effect (an effect removed and re-added
          -- in one fieldEvent would pass here) and for a field state with a target (a weather cleared and set again
          -- with the same id would pass here; clearWeather also resets the same object in place, so it would pass there
          -- too).
          hasTarget = maybe (hHasTarget h) esHasTarget current
          inPlace = case (current, owner) of
            (Just st, OwnSide _) -> esOrder st == hStateOrder h
            (_, OwnField) | not hasTarget -> True
            (Just _, _) -> True
            (Nothing, _) -> False
          -- `handler.state.duration`: the live state while it is in place, else the object as it was collected
          liveDuration = case current of
            Just st | inPlace -> esDuration st
            _ -> hDuration h
      if fainted
        then run rest
        else
          if ev == EvResidual && hEnd h && liveDuration /= 0
            then do
              -- handler.state.duration--: only an object still in place changes anything; a stale copy only counts
              let d = liveDuration - 1
              case current of
                Just st | inPlace -> modify' (setOwnerState owner key st {esDuration = d})
                _ -> pure ()
              if d == 0
                then do
                  -- `handler.end.call(effectHolder, effect.id)`: removes by id whatever is there now, then `continue`
                  endEffect owner key
                  ended <- gets ((/= Ongoing) . bResult)
                  unless ended (run rest)
                else afterCount h inPlace rest
            else afterCount h inPlace rest
    -- the in-place check (battle.ts 535-560), then the callback
    afterCount h inPlace rest
      | not inPlace = run rest
      | otherwise = callback h >> after rest
    -- `if (handler.callback) singleEvent(...)`: the effect holder is the target (the side's condition collected for a
    -- Pokemon is called with that Pokemon)
    callback h = forM_ (hsBody (hSpec h)) $ \body -> do
      b <- get
      void (singleBody dx ev body (hKey h) (hOwner h) (fromMaybe (holderOf b (hOwner h)) (hHolder h)) Nothing Nothing RNone)
    after rest = do
      resolveFaints dx False True -- this.resolveFaints() (checkWin defaults to true)
      ended <- gets ((/= Ongoing) . bResult)
      unless ended (run rest)
    endEffect owner key = case (owner, key) of
      -- pokemon.clearStatus: `if (!this.hp || !this.status) return false`, then clears whatever status it has now
      (OwnMon holder, EffectKey EffStatus _) -> modify' (modifyMon holder (\x -> if monHp x > 0 then clearStatus x else x))
      -- pokemon.removeVolatile: `if (!this.hp) return false`, then by id (false when it is not there)
      (OwnMon holder, EffectKey EffVolatile i) -> modify' (modifyMon holder (\x -> if monHp x > 0 then removeVolatile i x else x))
      (OwnSide p, EffectKey EffSideCond i) -> removeSideCondition dx p i
      (OwnField, EffectKey EffWeather _) -> clearWeather dx
      (OwnField, EffectKey EffTerrain _) -> clearTerrain dx
      (OwnField, _) -> removePseudoWeather dx key
      _ -> pure () -- abilities and items have no duration, so never end here

-- ---------------------------------------------------------------------------
-- Handler bodies

runBody :: MonadChance m => Dex -> Body -> Ctx -> Relay -> Eng m Res
runBody dx body c relay = case body of
  -- pokemon.useItem() when every condition holds (the seeds)
  BRule (RUseItem conds) -> do
    b <- get
    let m = monOf b (cHolder c)
        holds cond = case cond of
          CTerrain ids -> maybe False (`elem` ids) (effectiveTerrain b)
          CMoveType _ -> False
          CHolderHpAtMost x y -> monHp m * y <= monMaxHp m * x
          CWeather ids -> maybe False (`elem` ids) (effectiveWeatherOf b (cHolder c))
    when (all holds conds) (void (useItem dx (cHolder c) (Just (cKey c))))
    pure (Res Keep (cMod c))
  -- chainModify(n, d) of the event's modifier when every condition holds
  BRule (RChainModify n d conds) -> do
    b <- get
    let m = monOf b (cHolder c)
        holds cond = case cond of
          CMoveType t -> maybe False ((== t) . mvType) (cMove c)
          CHolderHpAtMost x y -> monHp m * y <= monMaxHp m * x
          -- `pokemon.effectiveWeather()` (swiftswim, chlorophyll, slushrush) or `field.isWeather(...)` (sandrush), which
          -- differ only for Utility Umbrella (unsupported)
          CWeather ids -> maybe False (`elem` ids) (effectiveWeatherOf b (cHolder c))
          CTerrain ids -> maybe False (`elem` ids) (effectiveTerrain b)
    pure (Res Keep (if all holds conds then chainModify (bFormat b) (cMod c) (n, d) else cMod c))
  BConst r -> pure (Res (Replace r) (cMod c))
  BNative n -> runNative dx n c relay

-- | The named handlers (data/conditions.ts). Damage is @battle.damage(n)@: @clampIntRange(n, 1)@ of the floored
-- amount, with the Pokemon as its own target and no source.
runNative :: MonadChance m => Dex -> NativeId -> Ctx -> Relay -> Eng m Res
runNative dx n c relay = do
  let holder = cHolder c
      keep = pure (Res Keep (cMod c))
      returning o = pure (Res o (cMod c))
  m <- gets (`monOf` holder)
  fmt <- gets bFormat
  let boots = hasItemNamed dx "heavydutyboots" m
      -- `isGrounded() && !isSemiInvulnerable()` of the holder (the event's target: the Pokemon being hit or statused, or
      -- the attacker of a BasePower event)
      groundedHolder = isGrounded dx m && not (isSemiInvulnerable m)
  case n of
    -- brn, par, psn, frz onStart: `this.add('-status', ...)` only
    NLogOnly -> keep
    -- brn.onResidual: this.damage(pokemon.baseMaxhp / 16)
    NBrnResidual -> modify' (applyDamage holder (max 1 (monMaxHp m `quot` 16))) >> keep
    -- psn.onResidual: this.damage(pokemon.baseMaxhp / 8)
    NPsnResidual -> modify' (applyDamage holder (max 1 (monMaxHp m `quot` 8))) >> keep
    -- tox.onStart and tox.onSwitchIn: this.effectState.stage = 0
    NToxStart -> setStage holder 0 >> keep
    NToxSwitchIn -> setStage holder 0 >> keep
    -- tox.onResidual: `if (stage < 15) stage++`; damage(clampIntRange(baseMaxhp / 16, 1) * stage)
    NToxResidual -> do
      let stage0 = esA (monStatusState m)
          stage = if stage0 < 15 then stage0 + 1 else stage0
      setStage holder stage
      modify' (applyDamage holder (max 1 (monMaxHp m `quot` 16) * stage))
      keep
    -- par.onModifySpe: spe = finalModify(spe); floor(spe * 50 / 100) (no Quick Feet: no abilities yet)
    NParModifySpe -> case relay of
      RInt spe -> let (v, md) = parSpe fmt (cMod c) spe in pure (Res (Replace (RInt v)) md)
      _ -> keep
    -- par.onBeforeMove: `if (this.randomChance(1, 4)) { cant; return false }`
    NParBeforeMove -> do
      full <- randomChance 1 4
      returning (if full then stop else Keep)
    -- frz.onBeforeMove: a defrost move returns (it thaws in ModifyMove; `burnup` has handlers and is not
    -- supported); else randomChance(1, 5) cures the status and lets the move go on, and failing it is `cant`
    NFrzBeforeMove
      | defrost -> keep
      | otherwise -> do
          thawed <- randomChance 1 5
          if thawed then modify' (cureStatus holder) >> keep else returning stop
    -- frz.onModifyMove: `if (move.flags['defrost']) pokemon.clearStatus()`
    NFrzModifyMove -> do
      when defrost (modify' (modifyMon holder (\x -> if monHp x > 0 then clearStatus x else x)))
      keep
    -- frz.onAfterMoveSecondary: `if (move.thawsTarget) target.cureStatus()`
    NFrzAfterMoveSecondary -> do
      when (maybe False (mrThawsTarget . mvRule) (cMove c)) (modify' (cureStatus holder))
      keep
    -- frz.onDamagingHit: a damaging Fire move cures the target (`move.id !== 'polarflare'`, which has handlers
    -- and is not supported)
    NFrzDamagingHit -> do
      when (maybe False (\mv -> typeIsNamed dx (mvType mv) "Fire") (cMove c)) (modify' (cureStatus holder))
      keep
    -- flinch.onBeforeMove: `cant`, runEvent('Flinch') (no handlers), return false
    NFlinchBeforeMove -> returning stop
    -- data/abilities.ts technician.onBasePower: `basePowerAfterMultiplier = this.modify(basePower, this.event.modifier)`;
    -- `if (basePowerAfterMultiplier <= 60) return this.chainModify(1.5)`
    NTechnician -> case relay of
      RInt bp | modify fmt bp (cMod c) <= 60 -> pure (Res Keep (chainModify fmt (cMod c) (3, 2)))
      _ -> keep
    -- data/items.ts wiseglasses.onBasePower: `move.category === 'Special'` -> chainModify([4505, 4096])
    NWiseGlasses
      | Just mv <- cMove c, not (mvPhysical mv), not (mvStatus mv) -> chained (4505, 4096)
      | otherwise -> keep
    -- expertbelt.onModifyDamage(damage, source, target, move): `move && target.getMoveHitData(move).typeMod > 0`
    -- -> chainModify([4915, 4096]); the target is the event's source (the defender), whose hit data holds the
    -- clamped effectiveness 'typeMod'
    NExpertBelt -> do
      b <- get
      case (cMove c, cSource c) of
        (Just mv, Just d) | typeMod dx (mvType mv) (monOf b d) > 0 -> chained (4915, 4096)
        _ -> keep
    -- filter, solidrock, prismarmor .onSourceModifyDamage(damage, source, target, move): the holder is the target
    -- (the event's source): `typeMod > 0` -> chainModify(0.75)
    NFilterLike -> case cMove c of
      Just mv | typeMod dx (mvType mv) m > 0 -> chained (3, 4)
      _ -> keep
    -- leftovers.onResidual: this.heal(pokemon.baseMaxhp / 16); battle.heal turns an amount in (0, 1] into 1 and
    -- truncates, so the amount is floor(maxhp / 16), at least 1
    NLeftovers -> modify' (healMon holder (max 1 (monMaxHp m `quot` 16))) >> keep
    -- blacksludge.onResidual: a Poison-type heals baseMaxhp / 16, anything else takes this.damage(baseMaxhp / 8)
    NBlackSludge
      | hasTypeNamed dx "Poison" m -> modify' (healMon holder (max 1 (monMaxHp m `quot` 16))) >> keep
      | otherwise -> modify' (applyDamage holder (max 1 (monMaxHp m `quot` 8))) >> keep
    -- lifeorb.onAfterMoveSecondarySelf(source, target, move): `source && source !== target && move &&
    -- move.category !== 'Status' && !source.forceSwitchFlag` -> this.damage(source.baseMaxhp / 10, source, source,
    -- lifeorb); the holder is the user, and the move category is never Status where the event is run (a
    -- damaging move); forceSwitchFlag cannot be set (no force-switch moves)
    NLifeOrbRecoil
      | Just mv <- cMove c, not (mvStatus mv), cSource c /= Just holder ->
          modify' (applyDamage holder (max 1 (monMaxHp m `quot` 10))) >> keep
      | otherwise -> keep
    -- rockyhelmet.onDamagingHit(damage, target, source, move): `this.checkMoveMakesContact(move, source, target)`
    -- (no Protective Pads) -> this.damage(source.baseMaxhp / 6, source, target); the source is the attacker
    NRockyHelmet -> do
      b <- get
      case (cMove c, cSource c) of
        (Just mv, Just src) | mrContact (mvRule mv) ->
          modify' (applyDamage src (max 1 (monMaxHp (monOf b src) `quot` 6))) >> keep
        _ -> keep
    -- flameorb, toxicorb .onResidual: pokemon.trySetStatus('brn' / 'tox', pokemon)
    NFlameOrb -> void (trySetStatus dx holder Brn) >> keep
    NToxicOrb -> void (trySetStatus dx holder Tox) >> keep
    -- assaultvest.onDisableMove: every Status move except Me First is disabled (pokemon.disableMove)
    NAssaultVestDisable -> do
      modify' (modifyMon holder (\x -> x {monMoves = SV.map disableStatus (monMoves x)}))
      keep
    -- stealthrock.onSwitchIn(pokemon): `if (pokemon.hasItem('heavydutyboots')) return;`, then
    -- typeMod = clampIntRange(pokemon.runEffectiveness(stealthrock), -6, 6) and this.damage(maxhp * 2 ** typeMod / 8),
    -- which spreadDamage floors and clamps to at least 1 (exact in integers: a power of two over 8)
    NStealthRockSwitchIn
      | boots -> keep
      | Just rock <- typeIdNamed dx "Rock" -> do
          let tm = typeMod dx rock m
              dmg = if tm >= 0 then monMaxHp m * 2 ^ tm `quot` 8 else monMaxHp m `quot` (8 * 2 ^ negate tm)
          modify' (applyDamage holder (max 1 dmg))
          keep
      | otherwise -> keep
    -- spikes, toxicspikes onSideStart: this.effectState.layers = 1
    NLayerStart -> setLayers 1 >> keep
    -- spikes.onSideRestart: `if (this.effectState.layers >= 3) return false; layers++`
    NSpikesRestart -> restartLayers 3
    -- toxicspikes.onSideRestart: the same, at 2 layers
    NToxicSpikesRestart -> restartLayers 2
    -- spikes.onSwitchIn(pokemon): `if (!pokemon.isGrounded() || pokemon.hasItem('heavydutyboots')) return;`, then
    -- this.damage(damageAmounts[layers] * maxhp / 24) with damageAmounts [0, 3, 4, 6] (floored, at least 1)
    NSpikesSwitchIn
      | not (isGrounded dx m) || boots -> keep
      | otherwise -> do
          layers <- getLayers
          let amount = case layers of 1 -> 3; 2 -> 4; 3 -> 6; _ -> 0
          modify' (applyDamage holder (max 1 (amount * monMaxHp m `quot` 24)))
          keep
    -- toxicspikes.onSwitchIn(pokemon): `if (!pokemon.isGrounded()) return;` a Poison type removes the condition
    -- (checked before Steel and the boots); Steel or Heavy-Duty Boots do nothing; else
    -- pokemon.trySetStatus('tox' at 2 layers or more, else 'psn', the foe's active Pokemon): a source with Corrosion
    -- is the only thing the source changes, and no supported ability is Corrosion
    NToxicSpikesSwitchIn
      | not (isGrounded dx m) -> keep
      | hasTypeNamed dx "Poison" m -> do
          let Actor p _ = holder
              EffectKey _ ci = cKey c
          removeSideCondition dx p ci
          keep
      | hasTypeNamed dx "Steel" m || boots -> keep
      | otherwise -> do
          layers <- getLayers
          void (trySetStatus dx holder (if layers >= 2 then Tox else Psn))
          keep
    -- stickyweb.onSwitchIn(pokemon): `if (!pokemon.isGrounded() || pokemon.hasItem('heavydutyboots')) return;`, then
    -- this.boost({spe: -1}, pokemon, foe.active[0], stickyweb)
    NStickyWebSwitchIn
      | not (isGrounded dx m) || boots -> keep
      | otherwise -> void (boostMon holder [(BSpe, -1)]) >> keep
    -- reflect, lightscreen durationCallback(target, source, effect): `source?.hasItem('lightclay')` -> 8, else 5
    NLightClayDuration -> do
      b <- get
      duration (if maybe False (hasItemNamed dx "lightclay" . monOf b) (cSource c) then 8 else 5)
    -- tailwind durationCallback: 4 (6 with Persistent, which is not supported); trickroom durationCallback: 5 (7)
    NTailwindDuration -> duration 4
    NTrickRoomDuration -> duration 5
    -- reflect.onAnyModifyDamage(damage, source, target, move): `target !== source && this.effectState.target.hasAlly(target)
    -- && this.getCategory(move) === 'Physical'`, then `!target.getMoveHitData(move).crit && !move.infiltrates` ->
    -- chainModify(0.5) (activePerHalf is 1 in singles). The event is run with the attacker as its target and the defender
    -- as its source, so the handler's `source` is the attacker (the holder here) and its `target` the defender (the
    -- event's source). A move with `infiltrates` is not supported (the translation rejects the key).
    NReflectDamage -> screenDamage True
    -- lightscreen.onAnyModifyDamage: the same for a Special move
    NLightScreenDamage -> screenDamage False
    -- trickroom.onFieldRestart: this.field.removePseudoWeather('trickroom'); it returns undefined, so the move succeeds
    NTrickRoomRestart -> removePseudoWeather dx (cKey c) >> keep
    -- sunnyday, raindance, sandstorm, snowscape durationCallback(source, effect): 8 when the source holds the weather's rock
    -- (heatrock, damprock, smoothrock, icyrock), else 5
    NWeatherDuration -> do
      b <- get
      let rock = case maybe "" cnId (effectOf dx (cKey c)) of
            "sunnyday" -> "heatrock"
            "raindance" -> "damprock"
            "sandstorm" -> "smoothrock"
            _ -> "icyrock"
      duration (if maybe False (hasItemNamed dx rock . monOf b) (cSource c) then 8 else 5)
    -- onFieldResidual: `this.add('-weather', ..., '[upkeep]'); this.eachEvent('Weather')`. Sandstorm and snowscape do it
    -- `if (this.field.isWeather(own id))`
    NWeatherResidual -> eachEvent dx EvWeather >> keep
    NWeatherResidualIfActive -> do
      b <- get
      let EffectKey _ ci = cKey c
      when (effectiveWeather b == Just ci) (eachEvent dx EvWeather)
      keep
    -- sandstorm.onWeather(target): this.damage(target.baseMaxhp / 16). spreadDamage first asks
    -- `target.runStatusImmunity('sandstorm')` of a Weather effect (battle.ts 2133): Rock, Ground and Steel types, and
    -- the Immunity event (Sand Rush), skip it
    NSandstormWeather -> do
      ok <- runStatusImmunity dx holder "sandstorm"
      when ok (modify' (applyDamage holder (max 1 (monMaxHp m `quot` 16))))
      keep
    -- sandstorm.onModifySpD(spd, pokemon): `pokemon.hasType('Rock') && pokemon.effectiveWeather() === 'sandstorm'` ->
    -- this.modify(spd, 1.5) (a modify, not a chainModify: the stat itself changes)
    NSandstormSpD -> weatherStat "Rock"
    -- snowscape.onModifyDef(def, pokemon): the same for an Ice type
    NSnowscapeDef -> weatherStat "Ice"
    -- sunnyday.onWeatherModifyDamage(damage, attacker, defender, move): Hydro Steam by an attacker in sun, then
    -- `defender.effectiveWeather() !== 'sunnyday'` returns; Fire 1.5, Water 0.5
    NSunnyDayDamage -> do
      b <- get
      case (cMove c, cSource c) of
        (Just mv, Just defender)
          | mvId mv == "hydrosteam", ownWeather b holder -> chained (3, 2)
          | not (ownWeather b defender) -> keep
          | typeIsNamed dx (mvType mv) "Fire" -> chained (3, 2)
          | typeIsNamed dx (mvType mv) "Water" -> chained (1, 2)
        _ -> keep
    -- raindance.onWeatherModifyDamage: Water 1.5, Fire 0.5 in rain
    NRainDanceDamage -> do
      b <- get
      case (cMove c, cSource c) of
        (Just mv, Just defender)
          | not (ownWeather b defender) -> keep
          | typeIsNamed dx (mvType mv) "Water" -> chained (3, 2)
          | typeIsNamed dx (mvType mv) "Fire" -> chained (1, 2)
        _ -> keep
    -- sunnyday.onImmunity(type, pokemon): in sun, freezing is blocked
    NSunnyDayImmunity -> do
      b <- get
      if relay == RText "frz" && ownWeather b holder then returning stop else keep
    -- sandrush.onImmunity(type, pokemon): `type === 'sandstorm'` -> false
    NSandRushImmunity -> if relay == RText "sandstorm" then returning stop else keep
    -- hydration.onResidual(pokemon): a status under rain (or Primordial Sea) -> pokemon.cureStatus()
    NHydration -> do
      b <- get
      let rainy = [i | k <- ["raindance", "primordialsea" :: T.Text], Just i <- [M.lookup k (dxFieldIx dx)]]
      when (monStatus m /= StatusNone && maybe False (`elem` rainy) (effectiveWeatherOf b holder)) (modify' (cureStatus holder))
      keep
    -- electricterrain, grassyterrain, mistyterrain, psychicterrain durationCallback(source, effect): `source?.hasItem(
    -- 'terrainextender')` -> 8, else 5
    NTerrainDuration -> do
      b <- get
      duration (if maybe False (hasItemNamed dx "terrainextender" . monOf b) (cSource c) then 8 else 5)
    -- electricterrain.onSetStatus(status, target, source, effect): `status.id === 'slp' && target.isGrounded() &&
    -- !target.isSemiInvulnerable()` -> false (no supported move or ability inflicts sleep)
    NElectricTerrainSetStatus -> if relay == RText "slp" && groundedHolder then returning stop else keep
    -- electricterrain.onTryAddVolatile(status, target): a grounded target is protected from yawn (return null)
    NElectricTerrainTryAddVolatile -> if relay == RText "yawn" && groundedHolder then returning stop else keep
    -- electricterrain.onBasePower(basePower, attacker, defender, move): an Electric move by a grounded attacker,
    -- chainModify([5325, 4096]); the event's target is the attacker
    NElectricTerrainPower -> moveTypePower "Electric" groundedHolder (5325, 4096)
    -- grassyterrain.onBasePower: earthquake, bulldoze and magnitude against a grounded defender (the event's source),
    -- chainModify(0.5) and return; else a Grass move by a grounded attacker, chainModify([5325, 4096])
    NGrassyTerrainPower -> do
      b <- get
      case (cMove c, cSource c) of
        (Just mv, Just def)
          | mvId mv `elem` ["earthquake", "bulldoze", "magnitude"], isGrounded dx (monOf b def), not (isSemiInvulnerable (monOf b def)) -> chained (1, 2)
        _ -> moveTypePower "Grass" (isGrounded dx m) (5325, 4096)
    -- grassyterrain.onResidual(pokemon): a grounded, not semi-invulnerable Pokemon heals baseMaxhp / 16 (this.heal: at
    -- least 1)
    NGrassyTerrainHeal -> do
      when groundedHolder (modify' (healMon holder (max 1 (monMaxHp m `quot` 16))))
      keep
    -- mistyterrain.onSetStatus(status, target, source, effect): a grounded target takes no status (return false)
    NMistyTerrainSetStatus -> if groundedHolder then returning stop else keep
    -- mistyterrain.onTryAddVolatile(status, target, source, effect): a grounded target takes no confusion (return null)
    NMistyTerrainTryAddVolatile -> if relay == RText "confusion" && groundedHolder then returning stop else keep
    -- mistyterrain.onBasePower: a Dragon move against a grounded defender, chainModify(0.5)
    NMistyTerrainPower -> do
      b <- get
      case (cMove c, cSource c) of
        (Just mv, Just def)
          | typeIsNamed dx (mvType mv) "Dragon", isGrounded dx (monOf b def), not (isSemiInvulnerable (monOf b def)) -> chained (1, 2)
        _ -> keep
    -- psychicterrain.onTryHit(target, source, effect): `effect.priority <= 0.1 || effect.target === 'self'` returns; so
    -- does a semi-invulnerable or allied target and an ungrounded one; else the move is blocked (return null). The
    -- holder is the target and the source the user
    NPsychicTerrainTryHit -> case (cMove c, cSource c) of
      (Just mv, Just src)
        | mvPriority mv <= 0 -> keep
        | isSemiInvulnerable m || src == holder -> keep
        | not (isGrounded dx m) -> keep
        | otherwise -> returning stop
      _ -> keep
    -- psychicterrain.onBasePower: a Psychic move by a grounded attacker, chainModify([5325, 4096])
    NPsychicTerrainPower -> moveTypePower "Psychic" groundedHolder (5325, 4096)
    -- overcoat.onImmunity(type, pokemon): sandstorm, hail and powder are false
    NOvercoatImmunity -> if relay `elem` [RText "sandstorm", RText "hail", RText "powder"] then returning stop else keep
    -- overcoat.onTryHit(target, source, move): `move.flags['powder'] && target !== source &&
    -- this.dex.getImmunity('powder', target)` (the target is not naturally immune) -> return null
    NOvercoatTryHit -> case (cMove c, cSource c) of
      (Just mv, Just src) | mrPowder (mvRule mv), src /= holder, not (typeImmuneTo dx "powder" m) -> returning stop
      _ -> keep
    -- leafguard.onSetStatus(status, target, source, effect): in sun (or Desolate Land) any status fails
    NLeafGuardSetStatus -> do
      b <- get
      if inSun b holder then returning stop else keep
    -- leafguard.onTryAddVolatile(status, target): yawn fails in sun
    NLeafGuardTryAddVolatile -> do
      b <- get
      if relay == RText "yawn" && inSun b holder then returning stop else keep
    -- protosynthesis, quarkdrive condition.onStart(pokemon, source, effect): `fromBooster` when the effect is Booster
    -- Energy, and `bestStat = pokemon.getBestStat(false, true)`: the first of atk, def, spa, spd, spe with the greatest
    -- stat after its stage (no stat modifier events), a tie going to the earlier
    NParadoxStart -> do
      b <- get
      let fromBooster = case cEffect c of
            Just (EffectKey EffItem i) -> itemId dx i == "boosterenergy"
            _ -> False
      modify' (\bb -> maybe bb (\st -> setOwnerState (cOwner c) (cKey c) st {esA = bestStatIx (monOf b holder), esB = if fromBooster then 1 else 0} bb) (ownerState bb (cOwner c) (cKey c)))
      keep
    -- the five modifiers: `this.effectState.bestStat !== stat || pokemon.ignoringAbility()` returns; else
    -- chainModify([5325, 4096]), and 1.5 for Speed
    NParadoxAtk -> paradoxBoost 0 (5325, 4096)
    NParadoxDef -> paradoxBoost 1 (5325, 4096)
    NParadoxSpA -> paradoxBoost 2 (5325, 4096)
    NParadoxSpD -> paradoxBoost 3 (5325, 4096)
    NParadoxSpe -> paradoxBoost 4 (3, 2)
    -- unburden condition.onModifySpe(spe, pokemon): `!pokemon.item && !pokemon.ignoringAbility()` -> chainModify(2)
    NUnburdenSpe -> if monItem m == noItem then chained (2, 1) else keep
    -- protosynthesis.onStart(pokemon): this.singleEvent('WeatherChange', this.effect, this.effectState, pokemon); the
    -- handler runs as the SwitchIn at priority -2
    NProtosynthesisStart -> singleEvent dx EvWeatherChange (cKey c) (cOwner c) Nothing Nothing RNone >> keep
    NQuarkDriveStart -> singleEvent dx EvTerrainChange (cKey c) (cOwner c) Nothing Nothing RNone >> keep
    -- protosynthesis.onWeatherChange(pokemon): `this.field.isWeather('sunnyday')` (not Utility Umbrella's) ->
    -- pokemon.addVolatile('protosynthesis') (the effect is the ability: no Booster Energy); else, a volatile that
    -- is not fromBooster -> pokemon.removeVolatile('protosynthesis')
    NProtosynthesisWeatherChange -> do
      b <- get
      let sun = M.lookup "sunnyday" (dxFieldIx dx)
      if effectiveWeather b /= Nothing && effectiveWeather b == sun
        then void (addVolatileFx dx holder CProtosynthesis (Just (cKey c)))
        else unless (fromBoosterOf b holder CProtosynthesis) (modify' (modifyMon holder (\x -> if monHp x > 0 then removeVolatile (fromEnum CProtosynthesis) x else x)))
      keep
    -- quarkdrive.onTerrainChange(pokemon): the same with `this.field.isTerrain('electricterrain')`
    NQuarkDriveTerrainChange -> do
      b <- get
      let electric = M.lookup "electricterrain" (dxFieldIx dx)
      if effectiveTerrain b /= Nothing && effectiveTerrain b == electric
        then void (addVolatileFx dx holder CQuarkDrive (Just (cKey c)))
        else unless (fromBoosterOf b holder CQuarkDrive) (modify' (modifyMon holder (\x -> if monHp x > 0 then removeVolatile (fromEnum CQuarkDrive) x else x)))
      keep
    -- protosynthesis.onEnd, quarkdrive.onEnd(pokemon): `delete pokemon.volatiles[...]` (no End event for the volatile)
    NProtosynthesisEnd -> modify' (modifyMon holder (removeVolatile (fromEnum CProtosynthesis))) >> keep
    NQuarkDriveEnd -> modify' (modifyMon holder (removeVolatile (fromEnum CQuarkDrive))) >> keep
    -- unburden.onAfterUseItem(item, pokemon): `pokemon !== this.effectState.target` returns (the event's holder is its
    -- target); else pokemon.addVolatile('unburden')
    NUnburdenAfterUseItem -> void (addVolatileFx dx holder CUnburden (Just (cKey c))) >> keep
    -- unburden.onEnd(pokemon): pokemon.removeVolatile('unburden') (a Pokemon with no HP keeps it: clearVolatile follows)
    NUnburdenEnd -> modify' (modifyMon holder (\x -> if monHp x > 0 then removeVolatile (fromEnum CUnburden) x else x)) >> keep
    -- boosterenergy.onTakeItem, unburden.onTakeItem: no supported move takes an item (runEvent('TakeItem') is never run)
    -- LATENT: Booster Energy returns false for a Paradox species, Unburden adds its volatile.
    NInertTakeItem -> keep
    -- boosterenergy.onStart(pokemon): `this.effectState.started = true`, then its onUpdate
    NBoosterEnergyStart -> do
      modify' (\bb -> maybe bb (\st -> setOwnerState (cOwner c) (cKey c) st {esA = 1} bb) (ownerState bb (cOwner c) (cKey c)))
      boosterUpdate
    NBoosterEnergyUpdate -> boosterUpdate
    -- grassysurge, psychicsurge, mistysurge, electricsurge.onStart(source): this.field.setTerrain(...)
    NGrassySurge -> setTerrainNamed "grassyterrain"
    NPsychicSurge -> setTerrainNamed "psychicterrain"
    NMistySurge -> setTerrainNamed "mistyterrain"
    NElectricSurge -> setTerrainNamed "electricterrain"
    -- drought.onStart(source): `if (source.species.id === 'groudon' && source.item === 'redorb') return;`, then
    -- this.field.setWeather('sunnyday'); drizzle the same for Kyogre and a Blue Orb; neither orb is a supported item
    NDrought -> setWeatherNamed "sunnyday"
    NDrizzle -> setWeatherNamed "raindance"
    NSandStream -> setWeatherNamed "sandstorm"
    NSnowWarning -> setWeatherNamed "snowscape"
    -- toxicdebris.onDamagingHit(damage, target, source, move): `side = source.isAlly(target) ? source.side.foe :
    -- source.side` (singles: the attacker's side); a Physical move with no Toxic Spikes there, or fewer than 2
    -- layers, calls side.addSideCondition('toxicspikes', target)
    NToxicDebris -> do
      b <- get
      case (cMove c, cSource c, M.lookup "toxicspikes" (dxFieldIx dx)) of
        (Just mv, Just (Actor sp _), Just ts)
          | mvPhysical mv
          , maybe True ((< 2) . esA) (ownerState b (OwnSide sp) (EffectKey EffSideCond ts)) ->
              void (addSideCondition dx sp ts holder) >> keep
        _ -> keep
  where
    duration d = pure (Res (Replace (RInt d)) (cMod c))
    -- the weather this handler belongs to is the effective weather as the Pokemon sees it
    ownWeather b a = let EffectKey _ ci = cKey c in effectiveWeatherOf b a == Just ci
    paradoxBoost ix nd = do
      b <- get
      case ownerState b (cOwner c) (cKey c) of
        Just st | esA st == ix -> chained nd -- (pokemon.ignoringAbility() is never true: see 'ignoringAbility')
        _ -> pure (Res Keep (cMod c))
    fromBoosterOf b a cid = maybe False ((== 1) . esB) (effectState (monOf b a) (EffectKey EffVolatile (fromEnum cid)))
    -- boosterenergy.onUpdate(pokemon): once started, a Protosynthesis holder with no sun uses the item and adds the
    -- volatile (the effect is Booster Energy: fromBooster), then a Quark Drive holder with no Electric Terrain
    boosterUpdate = do
      b0 <- get
      case ownerState b0 (cOwner c) (cKey c) of
        Just st | esA st == 1 -> do
          paradox "protosynthesis" CProtosynthesis (\b -> effectiveWeather b /= Nothing && effectiveWeather b == M.lookup "sunnyday" (dxFieldIx dx))
          paradox "quarkdrive" CQuarkDrive (\b -> effectiveTerrain b /= Nothing && effectiveTerrain b == M.lookup "electricterrain" (dxFieldIx dx))
        _ -> pure ()
      pure (Res Keep (cMod c))
    paradox name cid active = do
      b <- get
      let mm = monOf b (cHolder c)
      when (abilityId dx (monAbility mm) == name && not (ignoringAbility b (cHolder c)) && not (active b)) $ do
        used <- useItem dx (cHolder c) (Just (cKey c))
        when used (void (addVolatileFx dx (cHolder c) cid (Just (cKey c))))
    moveTypePower ty cond nd = case cMove c of
      Just mv | typeIsNamed dx (mvType mv) ty, cond -> chained nd
      _ -> pure (Res Keep (cMod c))
    setTerrainNamed name = case M.lookup name (dxFieldIx dx) of
      Just ci -> setTerrain dx ci (cHolder c) >> pure (Res Keep (cMod c))
      Nothing -> pure (Res Keep (cMod c))
    inSun b a =
      let sun = [i | k <- ["sunnyday", "desolateland" :: T.Text], Just i <- [M.lookup k (dxFieldIx dx)]]
       in maybe False (`elem` sun) (effectiveWeatherOf b a)
    setWeatherNamed name = case M.lookup name (dxFieldIx dx) of
      Just ci -> setWeather dx ci (cHolder c) True >> pure (Res Keep (cMod c))
      Nothing -> pure (Res Keep (cMod c))
    weatherStat ty = do
      b <- get
      let mm = monOf b (cHolder c)
      case relay of
        RInt v | hasTypeNamed dx ty mm, ownWeather b (cHolder c) -> do
          fmt <- gets bFormat
          pure (Res (Replace (RInt (modify fmt v 6144))) (cMod c))
        _ -> pure (Res Keep (cMod c))
    screenDamage physical = case (cMove c, cSource c, cOwner c) of
      (Just mv, Just defender@(Actor dp _), OwnSide sp)
        | defender /= cHolder c
        , dp == sp
        , not (mvStatus mv)
        , mvPhysical mv == physical
        , not (cCrit c) -> chained (1, 2)
      _ -> pure (Res Keep (cMod c))
    getLayers = gets (\b -> maybe 0 esA (ownerState b (cOwner c) (cKey c)))
    setLayers v = modify' (\b -> maybe b (\st -> setOwnerState (cOwner c) (cKey c) st {esA = v} b) (ownerState b (cOwner c) (cKey c)))
    restartLayers limit = do
      layers <- getLayers
      if layers >= limit then pure (Res stop (cMod c)) else setLayers (layers + 1) >> pure (Res Keep (cMod c))
    defrost = maybe False (mrDefrost . mvRule) (cMove c)
    chained nd = do
      fmt <- gets bFormat
      pure (Res Keep (chainModify fmt (cMod c) nd))
    disableStatus ms =
      let mv = moveData dx (msMove ms)
       in if mvStatus mv && mvId mv /= "mefirst" then ms {msDisabled = True} else ms
    setStage holder v = modify' (modifyMon holder (\x -> x {monStatusState = (monStatusState x) {esA = v}}))

-- ---------------------------------------------------------------------------
-- Speed

-- | @pokemon.getActionSpeed()@ (pokemon.ts 662): the Spe stat with its stage, through the @ModifySpe@ event
-- (@getStat('spe')@ also runs @ModifyBoost@, which has no handlers), capped and truncated to 13 bits.
getActionSpeed :: MonadChance m => Dex -> Actor -> Eng m Int
getActionSpeed dx a = do
  b <- get
  let m = monOf b a
      boosted = boostedStat (stSpe (monStats m)) (bsSpe (monBoosts m))
  r <- runEvent dx EvModifySpe a Nothing Nothing (RInt boosted)
  -- `this.battle.field.getPseudoWeather('trickroom')` (the twisteddimensionmod rule, which inverts it, is not in gen9ou)
  let trickRoom = case M.lookup "trickroom" (dxFieldIx dx) of
        Just ci -> any (\(CondEntry j _) -> j == ci) (fPseudo (bField b))
        Nothing -> False
  pure (actionSpeed (bFormat b) trickRoom (case r of RInt v -> v; _ -> boosted))

-- | @battle.updateSpeed()@: refresh the cached speed of every Pokemon in play.
updateSpeedAll :: MonadChance m => Dex -> Eng m ()
updateSpeedAll dx = do
  b <- get
  forM_ (getAllActive False b) $ \a -> do
    s <- getActionSpeed dx a
    modify' (modifyMon a (\m -> m {monSpeed = s}))

-- ---------------------------------------------------------------------------
-- Damage, healing, fainting

clampStage :: Int -> Int
clampStage = max (-6) . min 6

-- | @battle.boost@ (battle.ts) of one target: each stage moves by its change clamped to [-6, 6]
-- (@getCappedBoost@). Returns whether any stage changed. It fails for a target with no HP or off the field,
-- and when the foe side has no Pokemon left. The order of the stats is unobservable (independent stages;
-- the boost events have no handlers).
-- LATENT: @battle.boost@ also sets @target.statsRaisedThisTurn@ / @statsLoweredThisTurn@ on success (battle.ts 2101); they
-- are not in the state. Lash Out (which reads @statsLoweredThisTurn@) and Eject Pack (@AfterBoost@) need them.
{-# INLINABLE boostMon #-}
boostMon :: Monad m => Actor -> [(BoostStat, Int)] -> Eng m Bool
boostMon a@(Actor p _) changes = do
  b <- get
  let m = monOf b a
  if monHp m <= 0 || not (monActive m) || sLeft (sideOf (other p) b) == 0
    then pure False
    else do
      let step' bs (s, d) = setBoostOf s (clampStage (boostOf s bs + d)) bs
          new = foldl step' (monBoosts m) changes
      modify' (modifyMon a (\x -> x {monBoosts = new}))
      pure (new /= monBoosts m)

-- | @singleEvent('End', pokemon.getAbility(), pokemon.abilityState, pokemon)@ and the same for the item: run when a
-- Pokemon switches out (battle-actions.ts 142-143) and when it faints (battle.ts 2605-2606), before @clearVolatile@.
-- Protosynthesis, Quark Drive and Unburden remove their volatile here (which @clearVolatile@ then removes anyway).
endAbilityAndItem :: MonadChance m => Dex -> Actor -> Eng m ()
endAbilityAndItem dx a = do
  m <- gets (`monOf` a)
  void (singleEvent dx EvEnd (EffectKey EffAbility (monAbility m)) (OwnMon a) Nothing Nothing RNone)
  when (monItem m /= noItem) (void (singleEvent dx EvEnd (EffectKey EffItem (monItem m)) (OwnMon a) Nothing Nothing RNone))

-- | @battle.heal@ + @Pokemon.heal@: restore up to max HP; nothing for 0, or at full HP, or when fainted.
healMon :: Actor -> Int -> Battle -> Battle
healMon a amount b
  | amount <= 0 || monHp m <= 0 || monHp m >= monMaxHp m = b
  | otherwise = modifyMon a (\x -> x {monHp = min (monMaxHp x) (monHp x + amount)}) b
  where
    m = monOf b a

-- | @Pokemon.damage@ / @faint@: reduce HP and queue the faint when it reaches zero.
applyDamage :: Actor -> Int -> Battle -> Battle
applyDamage a d b
  | monHp m <= 0 || d <= 0 = b
  | monHp m - d <= 0 =
      (modifyMon a (\x -> x {monHp = 0, monSwitchFlag = False, monFaintQueued = True}) b)
        {bFaintQueue = if monFainted m || monFaintQueued m then bFaintQueue b else SV.snoc (bFaintQueue b) a}
  | otherwise = modifyMon a (\x -> x {monHp = monHp x - d}) b
  where
    m = monOf b a

-- | @battle.resolveFaints@ (battle.ts), gen 5+ path without events: faint everything in the faint
-- queue, then optionally check for a winner. Arguments: @checkWinIfNoFaints@, @checkWin@.
resolveFaints :: MonadChance m => Dex -> Bool -> Bool -> Eng m ()
resolveFaints dx checkWinIfNoFaints checkWin' = do
  b <- get
  when (bResult b == Ongoing) $
    case SV.toList (bFaintQueue b) of
      [] -> when checkWinIfNoFaints (void (checkWin Nothing))
      q -> do
        -- LATENT: faints queued while this loop runs (battle.ts ~2626, which forces checkWin) need a Faint
        -- event handler (Destiny Bond, ...); nothing queues a faint here.
        forM_ q $ \a@(Actor p _) -> do
          m <- gets (\bb -> monOf bb a)
          unless (monFainted m) $ do
            modify' (modifySide p (\s -> if sLeft s > 0 then s {sLeft = sLeft s - 1} else s))
            -- singleEvent('End', ability) and singleEvent('End', item) (battle.ts 2605-2606), before the volatiles go
            endAbilityAndItem dx a
            -- pokemon.clearVolatile(false) resets the stat stages, the volatiles and the cached speed (setSpecies)
            modify' (modifyMon a (\mm -> mm
              { monSpeed = stSpe (monStats mm)
              , monBoosts = noBoosts
              , monVolatiles = SV.empty
              , monFainted = True
              , monActive = False
              }))
        modify' (\bb -> bb {bFaintQueue = SV.empty})
        case reverse q of
          (lastFaint : _) | checkWin' -> void (checkWin (Just lastFaint))
          _ -> pure ()

-- | @battle.checkWin@: end the battle if a side has no Pokemon left. Returns whether it ended.
checkWin :: Monad m => Maybe Actor -> Eng m Bool
checkWin faint = do
  b <- get
  let left p = sLeft (sideOf p b)
  if left P1 == 0 && left P2 == 0
    then win (fmap (\(Actor p _) -> p) faint) >> pure True
    else case [p | p <- [P1, P2], left (other p) == 0] of
      (p : _) -> win (Just p) >> pure True
      [] -> pure False

-- | @battle.win@: end the battle with a winner, or a tie when 'Nothing'.
win :: Monad m => Maybe Player -> Eng m ()
win w = do
  b <- get
  when (bResult b == Ongoing) $
    modify' (\bb -> bb {bResult = maybe Tied Won w, bRequest = ReqNone})
