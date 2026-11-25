{-# LANGUAGE OverloadedStrings #-}
-- | The event dispatcher (docs/handlers.md section 4), after Showdown's @sim/battle.ts@: @runEvent@,
-- @singleEvent@, @fieldEvent@ and @eachEvent@, with the handler collection (@findEventHandlers@), the sort keys
-- (@resolvePriority@) and the call-time gates.
--
-- Handlers are the status and volatile conditions of a Pokemon so far; the hooks where ability, item, species,
-- slot-condition, side, field and format handlers are collected are marked and empty. The functions the
-- natives and the turn loop share (damage, healing, fainting) live here too, because @fieldEvent@ resolves
-- faints after every handler.
module Engine.Dispatch
  ( Eng
  , runEvent
  , singleEvent
  , fieldEvent
  , eachEvent
  , getAllActive
  , getActionSpeed
  , updateSpeedAll
  , addVolatile
  , clearStatus
  , resolveFaints
  , checkWin
  , win
  , applyDamage
  , healMon
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify')
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine.Chance
import Engine.Damage (typeIsNamed)
import Engine.Dex
import Engine.Event
import Engine.Modifier (baseMod, modify)
import Engine.Rule (MoveRule (..))
import Engine.State
import Engine.Stats (boostedStat, finalSpeed, parSpe)

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

-- | The condition with an id of the state.
condIdOf :: Int -> Maybe CondId
condIdOf i = case [c | c <- [minBound .. maxBound], fromEnum c == i] of
  (c : _) -> Just c
  [] -> Nothing

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

-- | Replace the state of an effect a Pokemon has.
setEffectState :: EffectKey -> EffectState -> Mon -> Mon
setEffectState (EffectKey kind i) st m = case kind of
  EffStatus -> if fmap fromEnum (statusCond (monStatus m)) == Just i then m {monStatusState = st} else m
  EffVolatile -> m {monVolatiles = SV.map (\v@(Volatile j _) -> if j == i then Volatile j st else v) (monVolatiles m)}

-- | @pokemon.removeVolatile@: the @End@ event of the volatile has no handler here.
removeVolatile :: Int -> Mon -> Mon
removeVolatile i m = m {monVolatiles = SV.filter (\(Volatile j _) -> j /= i) (monVolatiles m)}

-- | @pokemon.setStatus('')@ through @clearStatus@ / @cureStatus@: the status goes with its state.
clearStatus :: Mon -> Mon
clearStatus m = m {monStatus = StatusNone, monStatusState = noEffectState}

-- | @pokemon.cureStatus()@: needs HP and a status.
cureStatus :: Actor -> Battle -> Battle
cureStatus a = modifyMon a (\x -> if monHp x > 0 && monStatus x /= StatusNone then clearStatus x else x)

-- | @pokemon.addVolatile(id)@ (pokemon.ts) of a condition with no @onRestart@, @TryAddVolatile@ handler or
-- immunity: needs HP (no @affectsFainted@) and fails when the volatile is already there; the state gets the
-- condition's duration; the @Start@ event runs (and, returning false, cancels it).
addVolatile :: MonadChance m => Dex -> Actor -> CondId -> Eng m Bool
addVolatile dx a c = do
  m <- gets (`monOf` a)
  if monHp m <= 0 || isJust (effectState m key)
    then pure False
    else do
      modify' (modifyMon a (\x -> x {monVolatiles = SV.snoc (monVolatiles x) (Volatile (fromEnum c) noEffectState {esDuration = cnDuration (conditionOf dx c)})}))
      r <- singleEvent dx EvStart key a Nothing Nothing RNone
      if relayTruthy r
        then pure True
        else modify' (modifyMon a (removeVolatile (fromEnum c))) >> pure False
  where
    key = EffectKey EffVolatile (fromEnum c)

-- ---------------------------------------------------------------------------
-- Collection and keys

-- | A collected handler (battle.ts @EventListener@).
data Handler = Handler
  { hKey :: !EffectKey
  , hHolder :: !Actor
  , hSpec :: !HandlerSpec
  , hSortKey :: !SortKey
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
-- Hook: the ability, item, species and slot-condition handlers of the Pokemon are collected here, in that order,
-- once the state holds them.
-- Keys are those of @resolvePriority@ (957-1032): order, priority and subOrder from the spec, the holder's cached
-- speed (scaled), and for @SwitchIn@ the holder's @effectOrder@ and the half-speed step by its position in
-- @battle.speedOrder@ (an absent position is -1, as @indexOf@).
pokemonHandlers :: Dex -> Battle -> Bool -> Prefix -> EventId -> Actor -> [Handler]
pokemonHandlers dx b byDuration pfx ev a@(Actor p _)
  | null (monVolatiles m) && monStatus m == StatusNone = []
  | otherwise = statusPart ++ concat [one (EffectKey EffVolatile i) c st | Volatile i st <- SV.toList (monVolatiles m), Just c <- [condIdOf i]]
  where
    m = monOf b a
    statusPart = case statusCond (monStatus m) of
      Just c -> one (EffectKey EffStatus (fromEnum c)) c (monStatusState m)
      Nothing -> []
    one key c st = case condSpec (conditionOf dx c) pfx ev of
      Just spec | isJust (hsBody spec) || (byDuration && esDuration st /= 0) -> [Handler key a spec (sortKey spec st)]
      _ -> []
    switchIn = ev == EvSwitchIn
    sortKey spec st = SortKey
      { skOrder = hsOrder spec
      , skPriority = hsPriority spec
      , skSpeed = monSpeed m * speedScale - (if switchIn then fromMaybe (-1) (U.elemIndex (playerIx p) (bSpeedOrder b)) else 0)
      , skSubOrder = hsSubOrder spec
      , skEffectOrder = if switchIn then esOrder st else 0
      }

-- | @findSideEventHandlers@ (battle.ts, side conditions and slot conditions), @findFieldEventHandlers@ (pseudo-weather,
-- weather, terrain) and @findBattleEventHandlers@ (the format and its rules): none exist yet.
sideHandlers, fieldHandlers, battleHandlers :: Battle -> EventId -> [Handler]
sideHandlers _ _ = []
fieldHandlers _ _ = []
battleHandlers _ _ = []

-- | @battle.findEventHandlers@ (battle.ts 1049-1116) for a Pokemon target: its own handlers first, then, when the
-- event takes prefixed handlers, those of allies and foes, then the source's @onSource@ handlers, the side's, the
-- field's and the format's. An inactive target with no active source contributes nothing, and the side handlers
-- are reached only through an active target (1070).
findEventHandlers :: Dex -> Battle -> EventId -> Actor -> Maybe Actor -> [Handler]
findEventHandlers dx b ev target source =
  own ++ prefixedAroundTarget ++ sourceHandlers ++ (if bubbles then sideHandlers b ev else []) ++ fieldHandlers b ev ++ battleHandlers b ev
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

-- | Sort handlers by the event's sort kind (battle.ts 797-803). A @speedSort@ draws only for tie runs.
sortHandlers :: MonadChance m => EventId -> [Handler] -> m [Handler]
sortHandlers ev hs = case eventSort ev of
  SpeedSort -> speedSortWith (\x y -> compareKey (hSortKey x) (hSortKey y)) hs
  LeftToRight -> pure (sortBy (\x y -> compareLeftToRight (hSortKey x) (hSortKey y)) hs)

-- ---------------------------------------------------------------------------
-- The events

-- | What a handler did: returned @undefined@ (keep going), or returned a value (the new relay value).
data Outcome = Keep | Replace !Relay

-- | The return @false@ of a handler.
stop :: Outcome
stop = Replace (RBool False)

-- | A handler's result and the event's modifier after it (@chainModify@ / @finalModify@ change it).
data Res = Res !Outcome !Int

-- | The context a handler runs in: its holder, the event's source effect (the move) and the event's modifier.
data Ctx = Ctx
  { cHolder :: !Actor
  , cMove :: !(Maybe MoveData)
  , cMod :: !Int
  }

-- | @battle.runEvent@ (battle.ts 765-925) on a Pokemon: collect the handlers, sort them, run them with the relay
-- value threaded through (a handler's value replaces it, a falsy one stops the event), and apply the accumulated
-- modifier to an integer result.
--
-- Arguments: the event, the target, the source Pokemon and the source effect (the move, for @BeforeMove@,
-- @ModifyMove@, @DamagingHit@, @AfterMoveSecondary@) and the relay value.
-- A call-time gate skips a handler: its status is no longer the holder's (battle.ts 872). The @breakable@ ability
-- (Mold Breaker), @ignoringItem@, @ignoringAbility@ and Air Lock gates (881-914) join with abilities, items and
-- weather.
-- LATENT: the move's own callbacks (@onEffect@, 787-795), array targets (@AfterMoveSecondary@ and @DamagingHit@
-- pass the target as a one-element array; in singles that is the same as the single target) and @fastExit@ are not
-- modelled.
runEvent :: MonadChance m => Dex -> EventId -> Actor -> Maybe Actor -> Maybe MoveData -> Relay -> Eng m Relay
runEvent dx ev target source mv relay0 = do
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
      let m = monOf b (hHolder h)
          gone = case hKey h of
            EffectKey EffStatus _ -> not (isJust (effectState m (hKey h)))
            EffectKey EffVolatile _ -> False
      if gone
        then go fmt hs r md -- `effect.effectType === 'Status' && effectHolder.status !== effect.id`: it's changed
        else case hsBody (hSpec h) of
          Nothing -> go fmt hs r md
          Just body -> do
            Res out md' <- runBody dx body (Ctx (hHolder h) mv md) r
            case out of
              Keep -> go fmt hs r md'
              Replace r'
                | relayTruthy r' -> go fmt hs r' md'
                | otherwise -> pure (finish fmt r' md')

-- | @battle.singleEvent@ (battle.ts 578-656): run one effect's handler for an event on its holder. Returns the relay
-- value, which a handler may replace. Gated on the status still being the holder's (601); the ability, item and
-- weather gates (606-628) join with those effects.
singleEvent :: MonadChance m => Dex -> EventId -> EffectKey -> Actor -> Maybe Actor -> Maybe MoveData -> Relay -> Eng m Relay
singleEvent dx ev key holder _source mv relay = case effectBody dx key ev of
  Nothing -> pure relay
  Just body -> singleBody dx body key holder mv relay

-- | The body of an effect's own handler for an event.
effectBody :: Dex -> EffectKey -> EventId -> Maybe Body
effectBody dx (EffectKey _ i) ev = do
  c <- condIdOf i
  spec <- condSpec (conditionOf dx c) POwn ev
  hsBody spec

-- | The part of @singleEvent@ after finding the callback.
singleBody :: MonadChance m => Dex -> Body -> EffectKey -> Actor -> Maybe MoveData -> Relay -> Eng m Relay
singleBody dx body key holder mv relay = do
  m <- gets (`monOf` holder)
  case key of
    EffectKey EffStatus _ | not (isJust (effectState m key)) -> pure relay -- it's changed; call it off
    _ -> do
      Res out _ <- runBody dx body (Ctx holder mv baseMod) relay
      pure (case out of Keep -> relay; Replace r -> r)

-- | @battle.eachEvent@ (battle.ts 470), for @Update@ and @BeforeTurn@: speed-sort the active Pokemon by their cached
-- @speed@ (ties are shuffled) and run the event on each.
eachEvent :: MonadChance m => Dex -> EventId -> Eng m ()
eachEvent dx ev = do
  b <- get
  let speedOf a = monSpeed (monOf b a)
  actives <- speedSortWith (\x y -> compare (speedOf y) (speedOf x)) (getAllActive False b)
  forM_ actives $ \a -> runEvent dx ev a Nothing Nothing RNone

-- | @battle.fieldEvent@ (battle.ts 489-577), for @Residual@ and @SwitchIn@. The handlers of the field, then of each
-- side and of each Pokemon in play (fainted ones too: they still take part in the sort, and are skipped when
-- run) are collected, sorted by @comparePriority@ and run one at a time, each followed by @resolveFaints()@.
--
-- Collected as Showdown does, side by side, p1 first, status before volatiles, so a speed tie is shuffled from
-- the same starting order. @effectOrder@ enters a key only for @SwitchIn@ (@resolvePriority@).
-- A handler of a fainted Pokemon is skipped. For @Residual@ a state with a duration counts it down first and the
-- effect ends at 0 (flinch, duration 1, is removed here), skipping the faint resolution of that handler. A handler
-- whose effect is no longer in place is skipped. When a faint ends the battle the remaining handlers do not run.
-- LATENT: "in place" is checked by presence of the effect id, where Showdown compares the state object itself
-- (battle.ts ~530-561). An effect removed and re-added within one fieldEvent would run here and not in Showdown;
-- unreachable while no handler removes and re-adds an effect during Residual or SwitchIn.
-- @targets@ restricts the Pokemon whose own handlers run (@SwitchIn@: the switchers); @onAnySwitchIn@ handlers
-- of every Pokemon in play are collected regardless.
fieldEvent :: MonadChance m => Dex -> EventId -> Maybe [Actor] -> Eng m ()
fieldEvent dx ev targets = do
  b <- get
  let byDuration = ev == EvResidual
      inTargets a = maybe True (a `elem`) targets
      perActive a =
        (if ev == EvSwitchIn then pokemonHandlers dx b False PAny ev a else [])
          ++ ( if inTargets a
                 then pokemonHandlers dx b byDuration POwn ev a ++ sideHandlers b ev ++ fieldHandlers b ev ++ battleHandlers b ev
                 else []
             )
      handlers =
        fieldHandlers b ev
          ++ concat
            [ sideHandlers b ev ++ [h | sHasActive s, h <- perActive (activeActor p b)]
            | p <- [P1, P2]
            , let s = sideOf p b
            ]
  sorted <- sortHandlers ev handlers
  run sorted
  where
    run [] = pure ()
    run (h : rest) = do
      let holder = hHolder h
          key = hKey h
      m <- gets (`monOf` holder)
      case effectState m key of
        _ | monFainted m -> run rest
        Nothing -> run rest -- the effect was removed by a prior handler
        Just st
          | ev == EvResidual && esDuration st /= 0 -> do
              -- handler.state.duration--; at 0 `end` (clearStatus / removeVolatile) and `continue`
              let d = esDuration st - 1
              modify' (modifyMon holder (setEffectState key st {esDuration = d}))
              if d == 0
                then do
                  modify' (modifyMon holder (endEffect key))
                  ended <- gets ((/= Ongoing) . bResult)
                  unless ended (run rest)
                else callback h >> after rest
          | otherwise -> callback h >> after rest
    -- `if (handler.callback) singleEvent(...)`
    callback h = forM_ (hsBody (hSpec h)) $ \body ->
      void (singleBody dx body (hKey h) (hHolder h) Nothing RNone)
    after rest = do
      resolveFaints False True -- this.resolveFaints() (checkWin defaults to true)
      ended <- gets ((/= Ongoing) . bResult)
      unless ended (run rest)
    endEffect key = case key of
      EffectKey EffStatus _ -> \x -> if monHp x > 0 then clearStatus x else x
      EffectKey EffVolatile i -> removeVolatile i

-- ---------------------------------------------------------------------------
-- Handler bodies

runBody :: MonadChance m => Dex -> Body -> Ctx -> Relay -> Eng m Res
runBody dx body c relay = case body of
  -- BRule: no rule kind exists yet ('HandlerRule' has no constructor)
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
  where
    defrost = maybe False (mrDefrost . mvRule) (cMove c)
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
  pure (finalSpeed (bFormat b) (case r of RInt v -> v; _ -> boosted))

-- | @battle.updateSpeed()@: refresh the cached speed of every Pokemon in play.
updateSpeedAll :: MonadChance m => Dex -> Eng m ()
updateSpeedAll dx = do
  b <- get
  forM_ (getAllActive False b) $ \a -> do
    s <- getActionSpeed dx a
    modify' (modifyMon a (\m -> m {monSpeed = s}))

-- ---------------------------------------------------------------------------
-- Damage, healing, fainting

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
resolveFaints :: Monad m => Bool -> Bool -> Eng m ()
resolveFaints checkWinIfNoFaints checkWin' = do
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
