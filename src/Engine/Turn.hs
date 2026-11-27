{-# OPTIONS_GHC -fexpose-all-unfoldings -fspecialise-aggressively #-}
-- The SPECIALIZE pragmas of 'newBattle' and 'step' below make this module compile the whole engine once for each concrete
-- chance monad; -fspecialise-aggressively lets that reach the functions of "Engine.Dispatch" and "Engine.Queue".
{-# LANGUAGE OverloadedStrings #-}
-- | The turn loop, structured after Showdown's sim/battle.ts and sim/battle-actions.ts.
--
-- Showdown's control flow is reproduced function by function: @start@ ('newBattle'),
-- @commitChoices@, @turnLoop@, @runAction@ (with its post-action tail), @runMove@ /
-- @useMove@ / @trySpreadMoveHit@, @switchIn@ / @runSwitch@, @resolveFaints@,
-- @checkFainted@, @checkWin@ and @endTurn@. The event points (@eachEvent@, @fieldEvent@, @runEvent@) are
-- "Engine.Dispatch"'s; they perform Showdown's speed sorts, which consume random draws on speed ties, whatever
-- is attached to them.
module Engine.Turn
  ( newBattle
  , step
  , stagedAccuracy
  , roundDiv
    -- slice 7d
  , runMove
    -- slice 8b
  , getDamage
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.State.Strict (execStateT, get, gets, modify')
import Data.Bits (setBit)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Vector.Strict as SV
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Engine.Chance
import Engine.Choice
import Engine.Damage
import Engine.Dex
import Engine.Dispatch
import Engine.Event
import Engine.Format (Format, trunc)
import Engine.Queue
import Engine.Rule
import Engine.State
import Engine.Stats (boostedStat)
import Engine.Team

-- ---------------------------------------------------------------------------
-- Construction and the public step

-- | Showdown @new Battle@ + @setPlayer@ x2 + @start()@: build both sides (drawing genders in
-- team order, p1 first), queue the @start@ action and stop at the team-preview request.
{-# INLINABLE newBattle #-}
{-# SPECIALIZE newBattle :: Dex -> Format -> Team -> Team -> Sample Battle #-}
{-# SPECIALIZE newBattle :: Dex -> Format -> Team -> Team -> Replay Battle #-}
newBattle :: MonadChance m => Dex -> Format -> Team -> Team -> m Battle
newBattle dx fmt t1 t2 = do
  s1 <- buildSide fmt dx t1
  s2 <- buildSide fmt dx t2
  let start = Action
        { acKind = KStart
        , acOrder = orderOf KStart
        , acPriority = 0
        , acSpeed = 1 * speedScale
        , acActor = Nothing
        , acArg = 0
        , acPranksterBoosted = False
        }
  pure Battle
    { bFormat = fmt
    , bTurn = 0
    , bRequest = ReqTeamPreview
    , bResult = Ongoing
    , bMidTurn = True
    , bQueue = SV.singleton start
    , bFaintQueue = SV.empty
    , bActiveMove = Nothing
    , bSpeedOrder = U.empty
    , bField = noField
    , bEffectOrder = 0
    , bP1 = s1
    , bP2 = s2
    }

-- | Apply the choices of the sides that were asked and run to the next decision point.
-- Bad input is reported without drawing or changing anything: a finished battle, a missing choice,
-- an illegal choice (not in 'legalChoices'; at team preview any permutation of 1..n) or a choice
-- from a side that is not being asked.
step :: MonadChance m => Dex -> Battle -> (Maybe Choice, Maybe Choice) -> m (Either StepError Battle)
step dx b (c1, c2) = case request b of
  Finished _ -> pure (Left BattleFinished)
  Awaiting kind a1 a2 ->
    case (validate P1 a1 c1, validate P2 a2 c2) of
      (Left e, _) -> pure (Left e)
      (_, Left e) -> pure (Left e)
      (Right x1, Right x2) -> Right <$> execStateT (commitChoices dx (x1 ++ x2)) b
    where
      validate _ False Nothing = Right []
      validate p False (Just c) = Left (UnexpectedChoice p c)
      validate p True Nothing = Left (MissingChoice p)
      validate p True (Just c)
        | isLegal b p c, Just as <- choiceActions dx b kind p c = Right as
        | otherwise = Left (IllegalChoice p c)
{-# INLINABLE step #-}
{-# SPECIALIZE step :: Dex -> Battle -> (Maybe Choice, Maybe Choice) -> Sample (Either StepError Battle) #-}
{-# SPECIALIZE step :: Dex -> Battle -> (Maybe Choice, Maybe Choice) -> Replay (Either StepError Battle) #-}

-- | Build the actions of a legal choice (Showdown @side.choose@ -> @choice.actions@).
choiceActions :: Dex -> Battle -> RequestKind -> Player -> Choice -> Maybe [Action]
choiceActions dx b kind p c = case (kind, c) of
  (TeamPreview, Team xs) -> Just
    [ mk KTeam (Just (Actor p (x - 1))) (negate i) i
    | (i, x) <- zip [0 ..] xs
    ]
  (MoveRequest, Move n)
    | mustStruggle b p -> Just [mk KMove active 0 (-1)]
    -- battle-queue.ts 242-247: `unshift` of a 'priorityChargeMove' action when the move has a `priorityChargeCallback` (Chilly
    -- Reception), ahead of the move; its speed is the Pokemon's action speed ('Engine.Queue.resolveAction')
    | otherwise -> Just ([mk KPriorityChargeMove active 0 (n - 1) | charges n] ++ [mk KMove active 0 (n - 1)])
  -- battle-queue.ts 230-236: `unshift` of a 'terastallize' action (only when the Pokemon is not already terastallized),
  -- ahead of the move; its priority is unset (0) and its speed the Pokemon's action speed ('Engine.Queue.resolveAction').
  -- The priority charge is unshifted after it (242), so it comes first in the list: [priorityChargeMove, terastallize, move].
  (MoveRequest, MoveTera n) ->
    (\ms -> let (charge, rest) = span ((== KPriorityChargeMove) . acKind) ms
            in charge ++ [mk KTerastallize active 0 0 | not (monTerastallized (activeMon p b))] ++ rest)
      <$> choiceActions dx b kind p (Move n)
  (MoveRequest, Switch n) -> switchTo KSwitch n
  (SwitchRequest, Switch n) -> switchTo KInstaSwitch n
  _ -> Nothing
  where
    s = sideOf p b
    active = Just (activeActor p b)
    mk k actor prio arg = Action
      { acKind = k, acOrder = 0, acPriority = prio, acSpeed = 0, acActor = actor, acArg = arg, acPranksterBoosted = False }
    switchTo k n = (\o -> [mk k active 0 o]) <$> sSlots s U.!? (n - 1)
    charges n = case monMoves (activeMon p b) SV.!? (n - 1) of
      Just ms -> isJust (moveSpec (moveData dx (msMove ms)) POwn EvPriorityChargeCallback)
      Nothing -> False

-- ---------------------------------------------------------------------------
-- commitChoices / turnLoop

-- | @battle.commitChoices@ (battle.ts): refresh speeds, queue the sorted choice actions in front of any
-- saved mid-turn queue, then run the turn loop.
commitChoices :: MonadChance m => Dex -> [Action] -> Eng m ()
commitChoices dx choices = do
  updateSpeedAll dx
  old <- gets bQueue
  resolved <- mapM (resolveAction dx) choices
  sorted <- sortQueue (SV.fromList resolved)
  modify' (\b -> b {bQueue = sorted SV.++ old, bRequest = ReqNone})
  turnLoop dx
{-# INLINABLE commitChoices #-}

-- | @battle.turnLoop@ (battle.ts): run queued actions until a request is made, the battle ends, or the
-- queue empties (then @endTurn@).
turnLoop :: MonadChance m => Dex -> Eng m ()
turnLoop dx = do
  mid <- gets bMidTurn
  unless mid $ do
    -- queue.insertChoice({choice: 'beforeTurn'}); queue.addChoice({choice: 'residual'})
    before <- resolveAction dx (plain KBeforeTurn)
    residual <- resolveAction dx (plain KResidual)
    q <- gets bQueue
    q' <- insertChoice before q
    modify' (\b -> b {bQueue = SV.snoc q' residual, bMidTurn = True})
  loop
  where
    plain k = Action {acKind = k, acOrder = 0, acPriority = 0, acSpeed = 0, acActor = Nothing, acArg = 0, acPranksterBoosted = False}
    loop = do
      q <- gets bQueue
      case SV.uncons q of
        Nothing -> do
          endTurn dx
          modify' (\b -> b {bMidTurn = False, bQueue = SV.empty})
        Just (a, rest) -> do
          modify' (\b -> b {bQueue = rest})
          runAction dx a
          stop <- gets (\b -> bRequest b /= ReqNone || bResult b /= Ongoing)
          unless stop loop
{-# INLINABLE turnLoop #-}

-- | @battle.setActiveMove@ (battle.ts 375-379): the move in use and its user.
setActiveMove :: Monad m => Actor -> MoveData -> Eng m ()
setActiveMove user mv = modify' (\b -> b {bActiveMove = Just (MoveInUse user (mvIgnoreAbility mv))})

-- | @battle.clearActiveMove@ (battle.ts 381-389).
clearActiveMove :: Monad m => Eng m ()
clearActiveMove = modify' (\b -> case bActiveMove b of Nothing -> b; Just _ -> b {bActiveMove = Nothing})

-- | @battle.endTurn@ (battle.ts): advance the turn counter, apply the 1000-turn limit and request moves.
{-# INLINABLE endTurn #-}
endTurn :: MonadChance m => Dex -> Eng m ()
endTurn dx = do
  modify' (\b -> b {bTurn = bTurn b + 1})
  -- for (const side of this.sides) for (const pokemon of side.active): every move slot is enabled again, then
  -- runEvent('DisableMove', pokemon) lets handlers (Assault Vest) disable some. This is the only place
  -- DisableMove runs (battle.ts 1702-1708); the move's own onDisableMove (singleEvent, 1711) has no supported user.
  forM_ [P1, P2] $ \p -> do
    b <- get
    when (sHasActive (sideOf p b)) $ do
      let a = activeActor p b
      modify' (modifyMon a (\m -> m {monMoves = SV.map (\ms -> ms {msDisabled = False}) (monMoves m)}))
      void (runEvent dx EvDisableMove a Nothing Nothing RNone)
      -- slice 7c: `pokemon.activeTurns++` (battle.ts 1782), in the same loop, after the trapping checks, for a Pokemon that has not fainted
      modify' (modifyMon a (\m -> if monFainted m then m else m {monActiveTurns = monActiveTurns m + 1}))
  t <- gets bTurn
  -- maybeTriggerEndlessBattleClause: turn limit (staleness cannot arise in slice 0)
  if t > 1000
    then win Nothing
    else modify' (\b -> b {bRequest = ReqMove})

-- ---------------------------------------------------------------------------
-- runAction

-- | @battle.runAction@ (battle.ts): perform one queued action, then the shared tail that resolves
-- faints, runs the @Update@ event, raises switch requests and re-sorts the remaining queue.
runAction :: MonadChance m => Dex -> Action -> Eng m ()
runAction dx ac = do
  fmt <- gets bFormat
  case acKind ac of
    KTeam -> teamAction ac
    KStart -> do
      startAction dx
      afterAction dx ac
    KMove -> do
      b <- get
      case acActor ac of
        Just a | monActive (monOf b a), not (monFainted (monOf b a)) -> do
          runMovePrio fmt dx a (acArg ac) (Just (acPriority ac `quot` priorityScale, acPranksterBoosted ac))
          afterAction dx ac
        _ -> pure () -- the Pokemon left the field or fainted: no tail (battle.ts 'move' case returns false)
    KInstaSwitch -> switchAction dx ac >> afterAction dx ac
    KSwitch -> switchAction dx ac >> afterAction dx ac
    -- battle.ts 2793-2798: `case 'priorityChargeMove'`: `if (!pokemon.isActive) return false; if (pokemon.fainted) return false;`
    -- (no tail), then `action.move.priorityChargeCallback.call(this, action.pokemon)`, then the shared tail
    KPriorityChargeMove -> do
      b <- get
      case acActor ac of
        Just a | monActive (monOf b a), not (monFainted (monOf b a)), Just ms <- monMoves (monOf b a) SV.!? acArg ac -> do
          movePriorityCharge dx a (moveData dx (msMove ms))
          afterAction dx ac
        _ -> pure ()
    KRunSwitch -> do
      forM_ (acActor ac) (runSwitch dx)
      afterAction dx ac
    -- battle.ts 2781-2782: `case 'terastallize': this.actions.terastallize(action.pokemon)`, then the shared tail
    KTerastallize -> do
      forM_ (acActor ac) (terastallize dx)
      afterAction dx ac
    KBeforeTurn -> do
      eachEvent dx EvBeforeTurn -- eachEvent('BeforeTurn')
      afterAction dx ac
    KResidual -> do
      clearActiveMove -- battle.ts 2854: `this.clearActiveMove(true)` before `updateSpeed`
      updateSpeedAll dx
      fieldEvent dx EvResidual Nothing -- fieldEvent('Residual')
      afterAction dx ac
{-# INLINABLE runAction #-}

-- | @actions.terastallize@ (battle-actions.ts 1990-2026), which draws nothing. An Ogerpon whose Tera type is not Fire, Grass,
-- Rock or Water returns at once (no illusion is supported): nothing is set, the side keeps its Tera. Otherwise the Pokemon is
-- terastallized and the side's @canTerastallize@ goes ('sTeraUsed'); an Ogerpon changes forme permanently
-- (@formeChange@: Ogerpon-Teal-Tera with Embody Aspect (Teal)); then @AfterTerastallization@ runs.
-- LATENT: Morpeko's branch (2018-2023) needs Hunger Switch's forme change, which is not supported; base Terapagos
-- terastallizes like any other Pokemon (no Terapagos forme is supported).
{-# INLINABLE terastallize #-}
terastallize :: MonadChance m => Dex -> Actor -> Eng m ()
terastallize dx a@(Actor p _) = do
  m <- gets (`monOf` a)
  let sp = speciesData dx (monSpecies m)
      ogerpon = spBaseSpecies sp == "Ogerpon"
      teraName = dxTypeNames dx V.! monTeraType m
  unless (ogerpon && teraName `notElem` ["Fire", "Grass", "Rock", "Water"]) $ do
    modify' (modifySide p (\s -> s {sTeraUsed = True}) . modifyMon a (\x -> x {monTerastallized = True}))
    when ogerpon (formeChangeTera dx a)
    void (runEvent dx EvAfterTerastallization a Nothing Nothing RNone)

-- | @pokemon.formeChange(ogerponSpecies, null, true)@ (pokemon.ts 1449-1518) of Ogerpon's Terastallization: @setSpecies@
-- (species, stored types, stats and speed), @baseSpecies@ (the species: transform is unsupported), then @setAbility(ability0,
-- null, null, true)@: the old ability's @End@, the new ability and a fresh state (the next effect order), the new ability's
-- @Start@ (Embody Aspect boosts a terastallized Ogerpon-Teal-Tera); @baseAbility@ is the new ability.
-- LATENT: every Ogerpon forme has the same base stats, so the stats are not recomputed; `source || !getAbility().flags
-- ['cantsuppress']` (1509) is always true as no supported ability has that flag.
formeChangeTera :: MonadChance m => Dex -> Actor -> Eng m ()
formeChangeTera dx a = do
  m <- gets (`monOf` a)
  let sp = speciesData dx (monSpecies m)
      base = if T.null (spBattleOnly sp) then spId sp else spBattleOnly sp
      newId = base <> (if base == "ogerpon" then "tealtera" else "tera")
  case (M.lookup newId (dxSpeciesIx dx), M.lookup (spAbility0 (speciesData dx (M.findWithDefault (monSpecies m) newId (dxSpeciesIx dx)))) (dxAbilityIx dx)) of
    (Just ns, Just ab) -> do
      let nsp = speciesData dx ns
      modify' (modifyMon a (\x -> x {monSpecies = ns, monType1 = spType1 nsp, monType2 = spType2 nsp, monSpeed = stSpe (monStats x)}))
      -- setAbility: singleEvent('End', oldAbility, this.abilityState, this), then the new state, then its Start
      void (singleEvent dx EvEnd (EffectKey EffAbility (monAbility m)) (OwnMon a) Nothing Nothing RNone)
      order <- nextEffectOrder True
      modify' (modifyMon a (\x -> x {monAbility = ab, monAbilityState = noEffectState {esOrder = order}}))
      void (singleEvent dx EvStart (EffectKey EffAbility ab) (OwnMon a) Nothing Nothing RNone)
      modify' (modifyMon a (\x -> x {monBaseAbility = ab}))
    _ -> pure ()

-- | @case 'team'@: rebuild the side's Pokemon order one action at a time. No tail.
teamAction :: Monad m => Action -> Eng m ()
teamAction ac = case acActor ac of
  Just (Actor p o) -> modify' $ modifySide p $ \s ->
    s {sSlots = if acArg ac == 0 then U.singleton o else U.snoc (sSlots s) o}
  Nothing -> pure ()

-- | @case 'start'@: set @pokemonLeft@ and switch in each side's lead.
{-# INLINABLE startAction #-}
startAction :: MonadChance m => Dex -> Eng m ()
startAction dx = do
  forM_ [P1, P2] $ \p ->
    modify' (modifySide p (\s -> if sLeft s > 0 then s {sLeft = SV.length (sMons s)} else s))
  forM_ [P1, P2] $ \p -> do
    s <- gets (sideOf p)
    switchIn dx HowSwitch p (activeOrig s)
  modify' (\b -> b {bMidTurn = True})

-- | @case 'switch'@ / @'instaswitch'@: switch the target in at the actor's position. A live Pokemon replaced by an instaswitch is
-- one whose switch flag was set, and the action tail's @runPursuitActivation@ already ran for it when the request was made
-- ('switchRequests'), setting @skipPursuit@: its @switchIn@ skips it (battle-actions.ts 97-98 and 62-63, battle.ts 2954). A plain @switch@ runs it
-- there.
{-# INLINABLE switchAction #-}
switchAction :: MonadChance m => Dex -> Action -> Eng m ()
switchAction dx ac = case acActor ac of
  Just (Actor p _) -> switchIn dx (if acKind ac == KSwitch then HowSwitch else HowInsta) p (acArg ac)
  Nothing -> pure ()

-- | The part of @runAction@ after the action's own work.
{-# INLINABLE afterAction #-}
afterAction :: MonadChance m => Dex -> Action -> Eng m ()
afterAction dx ac = do
  -- phazing (battle.ts 2866-2873): after the action and before the faints are resolved, whether or not the battle has ended
  phazing dx
  -- battle.ts 2875: `if (this.gen > 4) this.clearActiveMove()`, after the phazing and before the faints are resolved
  clearActiveMove
  -- fainting: resolveFaints({ checkWinIfNoFaints: true, checkWin: true })
  resolveFaints dx True True
  ended <- gets ((/= Ongoing) . bResult)
  unless ended $ do
    next <- gets (peekQueue . bQueue)
    -- switching (fainted pokemon): only when the queue is exhausted
    when (isNothing next) checkFainted
    -- gen >= 5: eachEvent('Update') unless the next action is an instaswitch
    when (acKind ac /= KStart && fmap acKind next /= Just KInstaSwitch) (eachEvent dx EvUpdate)
    flagged <- switchRequests dx
    if or flagged
      then modify' (\b -> b {bRequest = ReqSwitch})
      else do
        -- gen >= 8: speeds are refreshed and the queue re-sorted before the next move
        next' <- gets (peekQueue . bQueue)
        when (fmap acKind next' == Just KMove) $ do
          updateSpeedAll dx
          q <- gets bQueue
          refreshed <- SV.mapM (\a -> if isNothing (acActor a) then pure a else actionSpeedOf dx a) q
          sorted <- sortQueue refreshed
          modify' (\b' -> b' {bQueue = sorted})
  where
    peekQueue q = q SV.!? 0

-- | The action tail's phazing step (battle.ts 2866-2873): for each side in order, an active Pokemon with @forceSwitchFlag@ is dragged
-- out when it still has HP, and the flag is cleared either way. It follows the action's own work (the residual branch included) and
-- precedes @resolveFaints@, and Showdown does not test @ended@ first: a battle that a Rocky Helmet has just ended still drags.
{-# INLINABLE phazing #-}
phazing :: MonadChance m => Dex -> Eng m ()
phazing dx = forM_ [P1, P2] $ \p -> do
  b <- get
  let s = sideOf p b
  when (sHasActive s) $ do
    let a = activeActor p b
        m = monOf b a
    when (monForceSwitchFlag m) $ do
      when (monHp m > 0) (dragIn dx p)
      modify' (modifyMon a (\x -> x {monForceSwitchFlag = False}))

-- | @actions.dragIn@ (battle-actions.ts 216-227) for singles position 0. @getRandomSwitchable@ (battle.ts 1584-1587) draws first:
-- `sample` is @random(n)@ over 'switchCandidates', and it draws even for one candidate; no candidate means no draw and no drag.
-- Only then the HP check (`!oldActive.hp`; the phazing step has already tested it, so the order is not observable), the second
-- @DragOut@ event with no source or move, and the drag-mode 'switchIn'.
{-# INLINABLE dragIn #-}
dragIn :: MonadChance m => Dex -> Player -> Eng m ()
dragIn dx p = do
  b <- get
  let cands = switchCandidates b p
  unless (null cands) $ do
    i <- roll 0 (length cands)
    case drop i cands of
      target : _ -> do
        b' <- get
        let old = activeActor p b'
        when (monHp (monOf b' old) > 0) $ do
          ok <- relayTruthy <$> runEvent dx EvDragOut old Nothing Nothing RNone
          when ok (switchIn dx HowDrag p target)
      [] -> pure ()

-- | Which sides have a pending switch (battle.ts 2937-2967). A side that cannot switch has its flag cleared. For a side that can, each
-- flagged Pokemon with HP runs @runPursuitActivation@ (battle-actions.ts 69-80): `eachEvent('Update')` (gen >= 5), whose speed sort
-- draws on a tie, before the request is made; the @BeforeSwitchOut@ event has no handlers. It sets @skipPursuit@ so the switch-out
-- itself does not run it again ('switchAction'). A fainted flagged Pokemon (@checkFainted@) has no HP and skips it.
{-# INLINABLE switchRequests #-}
switchRequests :: MonadChance m => Dex -> Eng m [Bool]
switchRequests dx = mapM one [P1, P2]
  where
    one p = do
      b <- get
      let s = sideOf p b
          flagged = sHasActive s && maybe False ((/= NoSwitch) . monSwitchFlag) (slotMon s 0)
      if flagged && canSwitch b p == 0
        then do
          modify' (modifyMon (activeActor p b) (\m -> m {monSwitchFlag = NoSwitch}))
          pure False
        else do
          -- runPursuitActivation: `this.battle.clearActiveMove()` (battle-actions.ts 65), then the Update
          when (flagged && monHp (activeMon p b) > 0) (clearActiveMove >> eachEvent dx EvUpdate)
          pure flagged

-- ---------------------------------------------------------------------------
-- Faints and the winner

-- | @battle.checkFainted@: flag fainted active Pokemon for replacement when their side can switch.
{-# INLINABLE checkFainted #-}
checkFainted :: Monad m => Eng m ()
checkFainted = forM_ [P1, P2] $ \p -> do
  b <- get
  let s = sideOf p b
      a = activeActor p b
  when (canSwitch b p > 0 && sHasActive s && monFainted (monOf b a)) $
    -- pokemon.status = 'fnt'; pokemon.switchFlag = true
    modify' (modifyMon a (\m -> m {monSwitchFlag = SwitchForced, monStatus = StatusFainted}))

-- ---------------------------------------------------------------------------
-- Switching

-- | How @switchIn@ is called: a plain @switch@ choice (and the start's leads), an @instaswitch@, or a drag ('dragIn').
data SwitchHow = HowSwitch | HowInsta | HowDrag
  deriving (Eq)

-- | @actions.switchIn@ (battle-actions.ts) for singles position 0, without messages. @runPursuitActivation@ runs for a live Pokemon
-- being replaced only for a plain switch (battle-actions.ts 97-98: not after an instaswitch, see 'switchAction', and not for a
-- drag, @isDrag@). A drag runs @runSwitch@ at once (208-212) instead of inserting a @runSwitch@ action: no @updateSpeed@ of the
-- incoming Pokemon (that is @insertChoice@'s), so it keeps the speed cached when it last left the field.
--
-- LATENT: @isStarted@ (battle-actions.ts ~241) is not modelled; it matters once an effect reads it.
{-# INLINABLE switchIn #-}
switchIn :: MonadChance m => Dex -> SwitchHow -> Player -> Int -> Eng m ()
switchIn dx how p target = do
  b0 <- get
  let s0 = sideOf p b0
      tgt = Actor p target
  unless (monActive (monOf b0 tgt)) $ do
    let old = if sHasActive s0 then Just (activeActor p b0) else Nothing
    case old of
      Just o | monHp (monOf b0 o) > 0 -> do
        -- runPursuitActivation(oldActive): eachEvent('Update') (the BeforeSwitchOut event has no handlers). It returns at once for a
        -- Pokemon whose `skipPursuit` is set, which a flagged one has ('switchRequests'): not for an instaswitch
        when (how == HowSwitch) (clearActiveMove >> eachEvent dx EvUpdate)
        -- battle-actions.ts 100: `if (!this.battle.runEvent('SwitchOut', oldActive)) return false` (Regenerator and Natural Cure listen,
        -- neither refuses; custom formats are the only thing that can)
        void (runEvent dx EvSwitchOut o Nothing Nothing RNone)
        -- singleEvent('End', oldActive.getAbility(), ...) and singleEvent('End', oldActive.getItem(), ...) (battle-actions.ts
        -- 142-143: Protosynthesis, Quark Drive and Unburden remove their volatile)
        endAbilityAndItem dx o
        -- queue.cancelAction(oldActive) (battle-actions.ts 124-127: a Pokemon forced out or switching by its own move cannot use its
        -- chosen move; the actions it still has queued go, except a runSwitch); oldActive.clearVolatile() (resets the stat stages, the volatiles, both switch flags (`includeSwitchFlags`, pokemon.ts 1530, 1558-1561: a U-turn user dragged out by Red Card keeps none) and the cached speed; the status, and toxic's stage, stay)
        modify' (\b -> modifyMon o (\m -> m {monSpeed = stSpe (monStats m), monBoosts = noBoosts, monVolatiles = SV.empty, monLastMove = -1 {- slice 7c: clearVolatile, pokemon.ts 1563 -}, monAbility = monBaseAbility m,
          -- slice 8e: `setSpecies(baseSpecies)` (pokemon.ts 1581) gives back the species' types (Libero and Protean's setType is undone)
          monType1 = spType1 (speciesData dx (monSpecies m)), monType2 = spType2 (speciesData dx (monSpecies m)), monSwitchFlag = NoSwitch, monForceSwitchFlag = False}) b {bQueue = cancelAction o (bQueue b)})
      _ -> pure ()
    case old of
      Just o -> do
        -- oldActive.isActive = false; `if (oldActive.fainted) oldActive.status = ''`
        modify' (modifyMon o (\m -> m {monActive = False, monStatus = if monFainted m then StatusNone else monStatus m}))
        -- the old active and the incoming Pokemon trade slots
        modify' (modifySide p (\s -> s {sSlots = swapSlot0 target (sSlots s)}))
      Nothing -> pure ()
    -- (slice 7c: `pokemon.activeTurns = 0`, battle-actions.ts 157)
    modify' (modifyMon tgt (\m -> m {monActive = True, monActiveTurns = 0}) . modifySide p (\s -> s {sHasActive = True}))
    -- pokemon.abilityState = initEffectState({ id: pokemon.ability, target: pokemon }); pokemon.itemState =
    -- initEffectState({ id: pokemon.item, target: pokemon }) (battle-actions.ts 185-186). The Pokemon is active, so a
    -- state with an id takes the next effectOrder: the ability always (`noability` is an id), the item when held
    let monT = monOf b0 tgt
    abilityOrder <- nextEffectOrder (abilityId dx (monAbility monT) /= "")
    itemOrder <- nextEffectOrder (monItem monT /= noItem)
    modify' (modifyMon tgt (\m -> m {monAbilityState = noEffectState {esOrder = abilityOrder}, monItemState = noEffectState {esOrder = itemOrder}}))
    if how == HowDrag
      -- `if (isDrag) this.runSwitch(pokemon)` (battle-actions.ts 208-209), which also takes the runSwitch actions at the queue's head
      then runSwitch dx tgt
      else do
        -- queue.insertChoice({ choice: 'runSwitch', pokemon }): updateSpeed first, then a tie-aware insert
        getActionSpeed dx tgt >>= \sp -> modify' (modifyMon tgt (\m -> m {monSpeed = sp}))
        new <- resolveAction dx Action
          { acKind = KRunSwitch, acOrder = 0, acPriority = 0, acSpeed = 0, acActor = Just tgt, acArg = 0, acPranksterBoosted = False }
        q <- gets bQueue
        q' <- insertChoice new q
        modify' (\b -> b {bQueue = q'})
  where
    swapSlot0 o slots = case U.elemIndex o slots of
      Just i | i /= 0 -> slots U.// [(0, slots U.! i), (i, slots U.! 0)]
      _ -> slots

-- | @battle.initEffectState@'s counter (battle.ts 3381-3392) when the state has an id: its value, and the counter
-- advances; a state with no id gets 0.
nextEffectOrder :: Monad m => Bool -> Eng m Int
nextEffectOrder False = pure 0
nextEffectOrder True = do
  b <- get
  let (o, b') = takeEffectOrder b
  modify' (const b')
  pure o

-- | @actions.runSwitch@ (battle-actions.ts 225-250): gather consecutive @runSwitch@ actions, speed-sort every
-- Pokemon on the field (including fainted ones), record the result as @battle.speedOrder@ and run
-- @fieldEvent('SwitchIn', switchersIn)@ for the switchers.
--
-- The @SwitchIn@ handlers' keys use @speedOrder@: @resolvePriority@ gives each the speed
-- @pokemon.speed - speedOrder.indexOf(position) / 2@, so no two Pokemon tie and the sort draws nothing.
-- A Pokemon's own handlers and the hazards of its side tie on everything but @effectOrder@ (the order the states were
-- created, 'bEffectOrder'), which is therefore what orders two hazards on one side.
{-# INLINABLE runSwitch #-}
runSwitch :: MonadChance m => Dex -> Actor -> Eng m ()
runSwitch dx first = do
  -- while (queue.peek()?.choice === 'runSwitch') switchersIn.push(queue.shift().pokemon)
  q <- gets bQueue
  let following = SV.takeWhile ((== KRunSwitch) . acKind) q
      switchersIn = first : [a | Just a <- map acActor (SV.toList following)]
  modify' (\b -> b {bQueue = SV.drop (SV.length following) (bQueue b)})
  b <- get
  let speedOf a = monSpeed (monOf b a)
  -- battle.speedSort(allActive): comparePriority on Pokemon objects reduces to cached speed
  allActive <- speedSortWith (\x y -> compare (speedOf y) (speedOf x)) (getAllActive True b)
  -- this.battle.speedOrder = allActive.map(a => a.getFieldPositionValue()); singles: side.n + 2 * position
  modify' (\bb -> bb {bSpeedOrder = U.fromList [playerIx p | Actor p _ <- allActive]})
  fieldEvent dx EvSwitchIn (Just switchersIn)

-- ---------------------------------------------------------------------------
-- Moves

-- | @actions.runMove@ + @useMove@ + @trySpreadMoveHit@ (battle-actions.ts) for a supported move or
-- Struggle (slot -1: no PP is deducted and the move is typeless with recoil).
{-# INLINABLE runMove #-}
runMove :: MonadChance m => Format -> Dex -> Actor -> Int -> Eng m ()
runMove fmt dx user slot = runMovePrio fmt dx user slot Nothing

-- | 'runMove' with the priority of the queued action (slice 8e). Showdown's @getActionSpeed@ writes the priority that the
-- @ModifyPriority@ events gave into @action.move.priority@ (battle.ts 2702), and @runMove@ reads it back off the same ActiveMove
-- (@baseMove.priority@, 283): the move in use carries it, which Psychic Terrain's @onTryHit@ reads (a Prankster status move has priority
-- 1). @pranksterBoosted@ is set on the same object by Prankster's handler: 'Engine.Queue.actionSpeedOf' records it in the action
-- ('acPranksterBoosted') and it is carried here. 'Nothing' is the dex priority and no flag.
{-# INLINABLE runMovePrio #-}
runMovePrio :: MonadChance m => Format -> Dex -> Actor -> Int -> Maybe (Int, Bool) -> Eng m ()
runMovePrio fmt dx user@(Actor p _) slot qprio = do
  b <- get
  let target = activeActor (other p) b
      um = monOf b user
  case (slot < 0, monMoves um SV.!? slot) of
    -- Struggle: `baseMove.id !== 'struggle'` skips OverrideAction (battle-actions.ts 277); pokemon.deductPP finds no Struggle slot and
    -- returns 0, which runMove allows for struggle
    (True, _) -> go target (inUse b (dxStruggle dx)) (M.findWithDefault (-1) "struggle" (dxMoveIx dx)) Nothing
    (False, Just ms) -> do
      -- slice 7c. runEvent('OverrideAction', pokemon, target, baseMove) (battle-actions.ts 278-287): Encore returns the encored move's id;
      -- `baseMove = getActiveMove(changedMove); baseMove.priority = priority` keeps the chosen move's priority, on the move in use too
      -- (Psychic Terrain's TryHit and the Tera floor read `move.priority`; the queue is not re-sorted either), and the target is `getRandomTarget(pokemon, baseMove)` ('go' recomputes it from the
      -- move). The slot whose PP is deducted is the new move's (`deductPP(baseMove)` finds it by id).
      let chosen = inUse b (moveData dx (msMove ms))
      over <- runEvent dx EvOverrideAction user (Just target) (Just chosen) RNone
      case over of
        -- (`if (pranksterBoosted) baseMove.pranksterBoosted = pranksterBoosted`, 289)
        RText i | Just ix <- M.lookup i (dxMoveIx dx) -> go target ((moveData dx ix) {mvPriority = mvPriority chosen, mvPranksterBoosted = mvPranksterBoosted chosen}) ix (SV.findIndex ((== ix) . msMove) (monMoves um))
        _ -> go target chosen (msMove ms) (Just slot)
    _ -> pure ()
  where
    -- the move as the queue left it: its priority and its prankster flag
    inUse _ mv0 = mv0
      { mvPriority = maybe (mvPriority mv0) fst qprio
      , mvPranksterBoosted = maybe False snd qprio }
    -- @mix@: the Dex index of the move in use; @mslot@: the index of its move slot (Nothing: none, as for Struggle)
    go target mv mix mslot = do
      -- runEvent('BeforeMove'): before PP is deducted; a move stopped here costs no PP and returns from
      -- runMove without the closing resolveFaints / checkWin (there is nothing to resolve), and does not set lastMove
      -- (a move on the user targets the user: battle.getRandomTarget returns the Pokemon for 'self')
      -- (battle.getRandomTarget: `self`, `all`, `allySide` and `allyTeam` return the Pokemon; foeSide the foe)
      let eventTarget = if mrTarget (mvRule mv) `elem` [TargetSelf, TargetAllySide, TargetAll] then user else target
      -- runMove (battle-actions.ts 302): `this.battle.setActiveMove(move, pokemon, target)`, before BeforeMove
      setActiveMove user mv
      proceed <- relayTruthy <$> runEvent dx EvBeforeMove user (Just eventTarget) (Just mv) RNone
      -- a move stopped here: `clearActiveMove(true)` (309)
      unless proceed clearActiveMove
      when proceed $ do
        -- pokemon.deductPP (battle-actions.ts 339), after BeforeMove: `if (!pokemon.deductPP(baseMove, null, target) && move.id !== 'struggle')`
        -- the move fails (`cant nopp`) and returns. deductPP returns 0 for a move with no slot or no PP left. (A chosen move always has PP
        -- at the request, but BeforeMove can no longer hide that: Encore's override can name a move whose PP ran out this turn.)
        b <- get
        let um = monOf b user
            struggling = slot < 0
            hasPP = case mslot of
              Just i -> maybe False ((> 0) . msPP) (monMoves um SV.!? i)
              Nothing -> False
        -- the PP check fails: `cant nopp` and `clearActiveMove(true)` (341)
        unless (struggling || hasPP) clearActiveMove
        when (struggling || hasPP) $ do
          forM_ mslot $ \i ->
            modify' (modifyMon user (\m -> m {monMoves = SV.imap (\j x -> if j == i then x {msPP = msPP x - 1} else x) (monMoves m)}))
          -- pokemon.moveUsed(move, targetLoc) (battle-actions.ts 354; pokemon.ts 920-935): `this.lastMove = move`
          modify' (modifyMon user (\m -> m {monLastMove = mix}))
          useMove fmt dx user target mv
          -- runMove (battle-actions.ts 366-368), after useMove: `singleEvent('AfterMove', move, null, pokemon, target, move)` (no supported
          -- move has an onAfterMove) then `runEvent('AfterMove', pokemon, target, move)`: Eject Pack's onAnyAfterMove. The move is the
          -- one chosen: Showdown passes the ActiveMove after useMove's changes, which no handler of AfterMove reads.
          -- A move stopped before useMove (BeforeMove, the PP check) returns earlier and reaches neither.
          void (singleMoveEvent dx EvAfterMove user (Just target) mv)
          void (runEvent dx EvAfterMove user (Just target) (Just mv) RNone)
          -- resolveFaints(); checkWin()
          resolveFaints dx False True
          void (checkWin Nothing)

-- | The hit steps of @trySpreadMoveHit@ (battle-actions.ts): target check, type immunity, accuracy, then the
-- hit loop. Spread targets and @randomNormal@ reach the one foe in singles ('TargetFoe'); a move on the
-- user ('TargetSelf') skips immunity and accuracy (@hitStepAccuracy@: a self-targeted status move).
{-# INLINABLE useMove #-}
useMove :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m ()
useMove fmt dx user target mv0 = do
  -- useMoveInner (battle-actions.ts 494-497): singleEvent('ModifyType') and singleEvent('ModifyMove') on the move change
  -- the move in use (the copy made for this use: accuracy, type, power, ...), then runEvent('ModifyType') and
  -- runEvent('ModifyMove') run the other effects' handlers (a frozen user of a defrost move thaws:
  -- conditions.ts frz.onModifyMove; no other supported effect changes the move). The target they see is the one
  -- `getRandomTarget` gives (a move on the user targets the user). Every later step of the use reads the changed move.
  let eventTarget = if mrTarget (mvRule mv0) `elem` [TargetSelf, TargetAllySide, TargetAll] then user else target
  -- useMoveInner (battle-actions.ts 492): `this.battle.setActiveMove(move, pokemon, target)`, before the ModifyType and ModifyMove events.
  -- Showdown's ActiveMove is one object that those handlers mutate: 'mvIgnoreAbility' of the changed move is set again after them
  -- (Mold Breaker sets it in onModifyMove, 8f). There is no restore after a nested useMove in Gen 9 (battle-actions.ts 415-417 restores only for `gen <= 4`).
  setActiveMove user mv0
  mvT <- singleMoveEvent dx EvModifyType user (Just eventTarget) mv0
  mv1 <- singleMoveEvent dx EvModifyMove user (Just eventTarget) mvT
  -- the holders' handlers edit the move ('MoveEdit'): ModifyType's result is the move ModifyMove sees (battle-actions.ts 502-503)
  let edited r m = case r of
        RMoveEdits es@(_ : _) -> applyMoveEdits es m
        _ -> m
  mvTy <- (`edited` mv1) <$> runEvent dx EvModifyType user (Just eventTarget) (Just mv1) (RMoveEdits [])
  mv <- (`edited` mvTy) <$> runEvent dx EvModifyMove user (Just eventTarget) (Just mvTy) (RMoveEdits [])
  setActiveMove user mv
  -- useMoveInner (battle-actions.ts 540-551): the Pressure targets of the move pay for it, after the move is announced and before
  -- its `TryMove` and the hit steps. `runEvent('DeductPP', source, pokemon, move)` runs once per Pressure target and its numbers are
  -- summed; `pokemon.deductPP(moveOrMoveName, extraPP)` then takes them from the move's slot (pokemon.ts 909-923: nothing when the move
  -- has no slot, as Struggle, or its PP is 0 already; the PP stops at 0). `getMoveTargets` (pokemon.ts 812-873) gives the targets:
  -- the foe for a move on one foe (none when it has fainted: the move has no targets), the user alone for a move on itself or its
  -- side, the user and the foe (a fainted one too: `foes(true)`) for a move on the field, none for a move on the foe's side, and for
  -- a move with `mustpressure` (Spikes, Stealth Rock, Toxic Spikes) the foe that has not fainted, whatever its target.
  b <- get
  let foeAlive = not (monFainted (monOf b target))
      rule = mvRule mv
      pressureTargets
        | mrMustPressure rule = [target | foeAlive]
        | otherwise = case mrTarget rule of
            TargetFoe -> [target | foeAlive]
            TargetSelf -> [user]
            TargetAllySide -> [user]
            TargetFoeSide -> []
            TargetAll -> [user, target]
  drops <- mapM (\pt -> runEvent dx EvDeductPP pt (Just user) (Just mv) RNone) pressureTargets
  let extraPP = sum [n | RInt n <- drops]
      slotOf = SV.findIndex (\ms -> mvId (moveData dx (msMove ms)) == mvId mv) (monMoves (monOf b user))
  when (extraPP > 0) $ forM_ slotOf $ \i ->
    modify' (modifyMon user (\m -> m {monMoves = SV.imap (\j x -> if j == i && msPP x > 0 then x {msPP = max 0 (msPP x - extraPP)} else x) (monMoves m)}))
  useMoveWith fmt dx user target mv

-- | 'useMove' after the @ModifyType@ and @ModifyMove@ events: the rest of @useMoveInner@ and the hit steps, which read the
-- move in use.
{-# INLINABLE useMoveWith #-}
useMoveWith :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m ()
useMoveWith fmt dx user target mv = case mrTarget rule of
  -- trySpreadMoveHit (battle-actions.ts 656): the move's own onTry first, on the user (Teleport fails when the side cannot switch)
  -- (slice 7a: then `singleEvent('PrepareHit', move, {}, targets[0], pokemon, move) && runEvent('PrepareHit', pokemon, targets[0], move)`
  -- (656-658): Protect and its kin fail here, on the stall draw or when the user moves last; a falsy result ends the move)
  TargetSelf -> do
    tried <- moveTry dx user user mv
    prepared <- if tried then movePrepareHit dx user mv else pure False
    ran <- if prepared then relayTruthy <$> runEvent dx EvPrepareHit user (Just user) (Just mv) RNone else pure False
    -- LATENT: the hit steps of a self-targeted move (Invulnerability, TryHit, accuracy, BreakProtect) are skipped: no handler acts
    -- on one (a Protect user holds no protect volatile when it moves, and only the volatile's own TryHit would). Dancer, Instruct and
    -- the like, which let a Pokemon move twice in a turn, are not supported.
    when ran (statusMove dx user user mv)
  -- getMoveTargets (pokemon.ts 812): foeSide -> the foes (fainted ones too), allySide -> the allies and the user,
  -- all -> both, and tryMoveHit takes targets[0]: the foe for foeSide, the user for allySide and all
  TargetFoeSide -> sideMove dx user target mv
  TargetAllySide -> sideMove dx user user mv
  TargetAll -> sideMove dx user user mv
  TargetFoe -> do
    b <- get
    let tm = monOf b target
        um = monOf b user
    -- getMoveTargets (battle-actions.ts ~842-859, pokemon.ts getMoveTargets): a fainted target leaves no
    -- targets, so the move fails before any draw. getRandomTarget in singles returns foe.active[0] even when
    -- it has fainted; the spread targets (allAdjacentFoes, allAdjacent) use side.foes(), which skips it.
    unless (monFainted tm) $ do
     -- hitStepTryHitEvent (battle-actions.ts 709, step 1 of trySpreadMoveHit): runEvent('TryHit', targets, pokemon, move);
     -- a falsy result removes the target and the move fails before any draw (Psychic Terrain, Overcoat). It is sorted
     -- left to right (never a draw). Self-targeted moves skip it here: no handler acts on one (Psychic Terrain returns
     -- for `effect.target === 'self'`, Overcoat for `target === source`).
     -- LATENT: the Invulnerability event (step 0) and the move's @Try@ event have no handler.
     -- trySpreadMoveHit (battle-actions.ts 656-657): singleEvent('PrepareHit', move, {}, target, pokemon, move) (Ivy Cudgel
     -- logs), then runEvent('PrepareHit', pokemon, target, move), before the hit steps
     -- (656: `singleEvent('Try', move, null, pokemon, targets[0], move) && singleEvent('PrepareHit') && runEvent('PrepareHit')`:
     -- the move's own onTry first, whose falsy result fails the move: Poltergeist against a target with no item)
     tried <- moveTry dx user target mv
     when tried (void (singleMoveEvent dx EvPrepareHit target (Just user) mv))
     prepared <- if tried then relayTruthy <$> runEvent dx EvPrepareHit user (Just target) (Just mv) RNone else pure False
     hitRelay <- if prepared then runEvent dx EvTryHit target (Just user) (Just mv) RNone else pure (RBool False)
     -- ESCAPE HATCH: magicbounce (slice 8f). The handler stopped the event with `null` after calling `useMove(newMove, target, { target: source })`
     -- inside it; the nested use runs here, after the event returned, which changes nothing it could observe (the event's handlers after a
     -- falsy result do not run). The failed hit then ends this move (`!moveResult`, no `-fail` for null).
     when (hitRelay == RBounce) (bounceMove dx target user mv)
     let tryHit = relayTruthy hitRelay
     when tryHit $
      -- hitStepTypeImmunity (before accuracy: an immune target consumes no draws); a move with
      -- ignoreImmunity (every status move but a few) skips it
      unless (not (mrIgnoreImmunity rule) && isImmune dx b target (mvType mv)) $
       -- hitStepTryImmunity (battle-actions.ts 735): `gen >= 6 && move.flags['powder'] && target !== pokemon &&
       -- !this.dex.getImmunity('powder', target)`: Grass types are immune to a powder move (no draw)
       unless (mrPowder rule && typeImmuneTo dx "powder" tm) $
        -- hitStepTryImmunity, the move's own onTryImmunity (battle-actions.ts 739): Trick and Switcheroo against Sticky Hold
        -- hitStepTryImmunity (battle-actions.ts 742), after it: `gen >= 7 && move.pranksterBoosted && pokemon.hasAbility('prankster') &&
        -- !targets[i].isAlly(pokemon) && !this.dex.getImmunity('prankster', target)`: a Dark target is immune (the foe is never an
        -- ally here). A move on the user or a side never reaches this per-target step against a foe.
        moveTryImmunity dx target mv >>= \tryImm0 -> gets (\bP -> tryImm0 && not (mvPranksterBoosted mv && hasAbility dx bP user "prankster" && typeImmuneTo dx "prankster" tm)) >>= \tryImm -> when tryImm $ do
        -- hitStepAccuracy
        -- (the user as it is now: Libero and Protean's PrepareHit may have changed its types, which Toxic's check reads)
        umNow <- gets (`monOf` user)
        hits <- if mvAccuracy mv == neverMisses then pure True
          else do
            -- slice 8b. `accuracy !== true` (battle-actions.ts 779-791): the user's @ModifyBoost@ event, then (`if (!move.ignoreEvasion)`) the
            -- target's, each read for its one stage (Unaware zeroes the foe's evasion and the foe's accuracy). They run for Toxic by a Poison type
            -- too, which `accuracy = true` (797) then overrides: only an event's tie-break draws could tell.
            accStage <- modifyBoost dx user BAccuracy (bsAccuracy (monBoosts um))
            evaStage <- if mrIgnoreEvasion rule || mvIgnoreEvasion mv then pure 0 else modifyBoost dx target BEvasion (bsEvasion (monBoosts tm))
            -- the target's evasion stage joins the accuracy stage (battle-actions.ts 785-788)
            let staged = stagedAccuracy fmt (mvAccuracy mv) ((monBoosts um) {bsAccuracy = accStage}) ((monBoosts tm) {bsEvasion = evaStage})
            if isToxic && hasTypeNamed dx "Poison" umNow then pure True else randomChance staged 100
        when hits $
          if mrDamaging rule
            then do
              moveResult <- hitLoop fmt dx user target mv
              -- useMoveInner (battle-actions.ts 588): `if (move.selfBoost && moveResult) this.moveHit(pokemon, pokemon, move,
              -- move.selfBoost, false, true)`, once, after the whole hit loop (trySpreadMoveHit returned): the boosts go
              -- through battle.boost with isSelf (no draw, no Update). The move succeeded: the first hit always lands
              -- once accuracy passed. A user with no HP gets nothing.
              -- LATENT: a first hit that fails (a move TryHit that returns false, as Low Kick's against a Dynamaxed target) would
              -- make `moveResult` false and skip the boost; no supported selfBoost move has one.
              -- (slice 8c: `moveResult` is false when the first hit's damage event failed, 'hitLoop': the boost and the events below are skipped)
              when (moveResult && not (null (mrSelfBoost rule))) (void (boostMon dx (Just user) (Just mv) moveCause user (mrSelfBoost rule)))
              -- useMoveInner: the move succeeded (`moveResult`), the user is queued to faint if it has no HP, then
              -- runEvent('AfterMoveSecondarySelf', pokemon, target, move): Life Orb's recoil. A status move
              -- raises it too, but no supported handler acts on one.
              when moveResult (void (runEvent dx EvAfterMoveSecondarySelf user (Just target) (Just mv) RNone))
            else statusMove dx user target mv
  where
    rule = mvRule mv
    -- ID-SPECIAL: battle-actions.ts hitStepAccuracy ~797 (and hitStepInvulnerabilityEvent ~693): in gen >= 8 Toxic
    -- used by a Poison-type skips the accuracy check (`accuracy = true`), so it draws nothing
    isToxic = mrOnHit rule == [EStatus Tox] && mvId mv == "toxic"

-- | ESCAPE HATCH: magicbounce (slice 8f). The nested @useMove(newMove, target, { target: source })@ of Magic Bounce's @onTryHit@ and
-- @onAllyTryHitSide@ (data/abilities.ts 2444-2447, 2454-2457): the bouncer uses the move again on the Pokemon that used it.
--
-- - @newMove = this.dex.getActiveMove(move.id)@ is a fresh copy from the dex, so nothing Mold Breaker or another ModifyMove handler
--   did to the first use carries over; @hasBounced@ is set and @pranksterBoosted@ cleared. @useMoveInner@ then gives it the priority of
--   the move in use (battle-actions.ts 475: `move.priority = this.battle.activeMove.priority`), here the first use's.
-- - @sourceEffect@ is the running effect, Magic Bounce (@this.battle.effect@, 451): it has no @pp@, so there is no @DeductPP@ for
--   Pressure, and the move's @ignoreAbility@ is the ability's (none). The use costs the bouncer no PP either: this is @useMove@, not @runMove@.
--   LATENT(8e): Pressure's @DeductPP@ (@!sourceEffect || callerMoveForPressure@, 540-552) is skipped for the same reason.
-- - The nested use sets the active move and nothing restores it in Gen 9 (battle-actions.ts 414-416 is for @gen <= 4@), so the first
--   use finishes with the bounced move active ('runMove' reads @activeMove@ for @AfterMove@).
--   LATENT(8e): 'runMove' passes the first use's move to @AfterMove@; no handler reads a field that differs.
-- - The move can bounce only once (@hasBounced@), so a second Magic Bounce on the original user lets it through.
bounceMove :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
bounceMove dx bouncer source mv = do
  fmt <- gets bFormat
  forM_ (M.lookup (mvId mv) (dxMoveIx dx)) $ \ix ->
    useMove fmt dx bouncer source ((moveData dx ix) {mvPriority = mvPriority mv, mvHasBounced = True, mvPranksterBoosted = False})

-- | @tryMoveHit@ (battle-actions.ts 883-919) of a move on a side or the field (@foeSide@, @allySide@, @all@): no
-- type immunity, accuracy or crit draw. The event points are @singleEvent('Try')@ (Aurora Veil's @onTry@: it needs snow),
-- @singleEvent('PrepareHit')@, @runEvent('PrepareHit')@, then @runEvent('TryHitField')@ for @all@ or @runEvent('TryHitSide')@, then
-- @moveHit@. The @PrepareHit@ callback is the move's own (@onPrepareHit@), which no supported side or field move has; the
-- @runEvent@s sort the handlers of the Pokemon in play (none attached today: Magic Bounce's
-- @onAllyTryHitSide@ is unsupported), and a sort over fewer than two handlers draws nothing, but they stay so that
-- a handler that appears later is sorted where Showdown sorts it. A falsy result fails the move.
-- @useMoveInner@ then runs @AfterMoveSecondarySelf@ (battle-actions.ts ~607), which this path skips like the
-- Pokemon-targeted status path: its only listener, Life Orb, is gated on @move.category !== 'Status'@, so no
-- supported handler can act. Unlike a Pokemon-targeted status move there are no @Update@ events inside: @hitStepMoveHitLoop@ is not used.
{-# INLINABLE sideMove #-}
sideMove :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
sideMove dx user target mv = do
  -- tryMoveHit (battle-actions.ts 887): `this.battle.setActiveMove(move, pokemon, targets[0])`
  setActiveMove user mv
  -- tryMoveHit (battle-actions.ts 889-891): `singleEvent('Try') && singleEvent('PrepareHit') && runEvent('PrepareHit')`; the move's
  -- own onTry (Aurora Veil needs snow) comes first and a falsy result fails the move before anything else
  tried <- moveTry dx user target mv
  prepared <- if tried then relayTruthy <$> runEvent dx EvPrepareHit user (Just target) (Just mv) RNone else pure False
  when prepared $ do
    let tryHit = if mrTarget (mvRule mv) == TargetAll then EvTryHitField else EvTryHitSide
    hitRelay <- runEvent dx tryHit target (Just user) (Just mv) RNone
    -- ESCAPE HATCH: magicbounce (slice 8f). onAllyTryHitSide returned null after `useMove(newMove, this.effectState.target, { target: source })`;
    -- the Pokemon the hazard was aimed at is the bouncer in singles (the foe the target names), and the move's user is its target.
    when (hitRelay == RBounce) (bounceMove dx target user mv)
    when (relayTruthy hitRelay) (moveHitSide dx user target mv)

-- | @moveHit@ -> @spreadMoveHit@ -> @runMoveEffects@ (battle-actions.ts 1430, 1175, 1330) of a side-targeted move:
-- the damage is @undefined@ (basePower 0), so only the effect on the target's side runs.
-- The @TryHitSide@ and @TryHitField@ callbacks of the move itself (@onTryHitSide@, ...) are not supported; its @onHitField@
-- (Court Change, Haze) runs after its field effects, and its @onHitSide@ (none supported) would run beside it.
-- A failed effect (the condition is already there and cannot restart) fails the move; nothing else follows.
{-# INLINABLE moveHitSide #-}
moveHitSide :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
moveHitSide dx user (Actor tp _) mv = do
  -- `target.side.addSideCondition(moveData.sideCondition, source, move)`
  forM_ (mrSideCondition rule) $ \ci -> void (addSideCondition dx tp ci user)
  -- `this.battle.field.addPseudoWeather(moveData.pseudoWeather, source, move)`
  forM_ (mrPseudoWeather rule) $ \ci -> void (addPseudoWeather dx ci user)
  -- `this.battle.field.setWeather(moveData.weather, source, move)`
  forM_ (mrWeather rule) $ \ci -> void (setWeather dx ci user False)
  -- `this.battle.field.setTerrain(moveData.terrain, source, move)`
  forM_ (mrTerrain rule) $ \ci -> void (setTerrain dx ci user)
  -- `if (move.target === 'all' && !isSelf) { if (moveData.onHitField) singleEvent('HitField', ...) }` (battle-actions.ts 1328-1331).
  -- Its result only decides whether the move reports a failure: nothing follows a side or field move either way.
  -- (HitSide, 1332-1335, is for the moves with an onHitSide: none is supported.)
  when (mrOnHit rule == [EOwnHitField]) (void (moveFieldHit dx user mv))
  -- `if (moveData.selfSwitch) { if (canSwitch(source.side) ...) didSomething = true; else combineResults(didSomething, false) }`
  -- (1349-1353): with a switch target the move did something even if its weather was already up (Chilly Reception); then
  -- the flag step (1369-1372)
  when (mvSelfSwitch mv) (setSelfSwitchFlag user)
  where
    rule = mvRule mv

-- | @hitStepAccuracy@'s stage step (battle-actions.ts): the user's accuracy stage minus the target's evasion
-- stage, each clamped to [-6, 6], scales the accuracy by @(3 + s) / 3@ or @3 / (3 - s)@, truncated.
stagedAccuracy :: Format -> Int -> Boosts -> Boosts -> Int
stagedAccuracy fmt acc user tgt
  | stage > 0 = trunc fmt (acc * (3 + stage) `quot` 3)
  | stage < 0 = trunc fmt (acc * 3 `quot` (3 - stage))
  | otherwise = acc
  where
    stage = clampStage (clampStage (bsAccuracy user) - bsEvasion tgt)

-- | A status move's single hit (@trySpreadMoveHit@ -> @runMoveEffects@): a self-targeted heal, a stat
-- change or a status condition on the target. When the effect does nothing (heal at full HP, a stage already
-- at its limit, a status the target is immune to or already has) the move fails with no draw and no @Update@: @runMoveEffects@ marks the damage false, @spreadMoveHit@ reports it, and
-- @hitStepMoveHitLoop@ stops before its first @Update@. On success the one hit runs the same two @Update@
-- events as a damaging hit, with no damage dealt.
{-# INLINABLE statusMove #-}
statusMove :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
statusMove dx user tgt mv0 = do
  -- spreadMoveHit's first step (battle-actions.ts 1102): singleEvent('TryHit', moveData, {}, target, pokemon, move), the move's own
  -- onTryHit; a falsy result ends the move with no other effect (Substitute's fail cases, slice 7b)
  tryHit <- moveStatusTryHit dx user mv0
  when tryHit (statusMoveHit dx user tgt mv0)

-- | The rest of 'statusMove', once the move's own @onTryHit@ passed.
{-# INLINABLE statusMoveHit #-}
statusMoveHit :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
statusMoveHit dx user tgt mv0 = do
  b <- get
  let tm = monOf b tgt
  -- ESCAPE HATCH: substitute (slice 7b). spreadMoveHit step 0 (battle-actions.ts 1115-1119): runEvent('TryPrimaryHit') for a move on
  -- another Pokemon. A Substitute that does not let the move through makes `getDamage` return undefined for a move with no base
  -- power, so the Substitute fails it (`-fail`, null): the target is dropped from every later step of this hit (no effect, no
  -- force switch), but the hit loop still counts the hit (null is not false, battle-actions.ts 1019): it runs the Update events
  -- as a hit that did something, and the move succeeds. A move on the user skips the event (`target === source` returns).
  blocked <- if user == tgt then pure False else (== RBool False) <$> runEvent dx EvTryPrimaryHit tgt (Just user) (Just mv0) RNone
  (done0, mv) <- if blocked then pure (True, mv0) else case mrOnHit (mvRule mv0) of
    [EHeal n d]
      | monHp tm >= monMaxHp tm -> pure (False, mv0)
      -- amount = baseMaxhp * n / d; battle.heal(Math.round(amount), ...) (gen >= 5)
      | otherwise -> do
          modify' (healMon tgt (roundDiv (monMaxHp tm * n) d))
          -- selfDrops (battle-actions.ts 1157, 1383-1395), after runMoveEffects of the same hit and only when it did not
          -- fail: `moveHit(source, source, move, moveData.self, ..., true)` adds roost's volatile (no draw)
          forM_ (mrSelf (mvRule mv0)) (applyEffect dx user mv0 tgt)
          pure (True, mv0)
    [EBoost bs] -> (\r -> (r, mv0)) <$> boostMon dx (Just user) (Just mv0) moveCause tgt bs
    -- the move's own onHit (singleEvent('Hit'), battle-actions.ts 1338): its result decides whether the hit did
    -- something. A heal that fails with NOT_FAIL still counts as a hit (see 'HitResult'), a false return fails the move
    -- (the move in use comes back: Parting Shot's body deletes its selfSwitch)
    [EOwnHit] -> (\(r, m) -> (r /= HitFail, m)) <$> moveOwnHit dx user tgt mv0
    -- slice 7a, runMoveEffects (battle-actions.ts 1294-1296, 1338): `target.addVolatile(moveData.volatileStatus, source, move)` (the move's own
    -- volatile: protect, spikyshield, banefulbunker, endure), then the move's onHit (adds `stall`), each folded into didSomething by
    -- combineResults: one success is enough
    [EOwnVolatile, EOwnHit] -> do
      added <- case condByName (mvId mv0) of
        Just c -> addVolatile dx tgt c
        Nothing -> pure False
      (r, m') <- moveOwnHit dx user tgt mv0
      pure (added || r /= HitFail, m')
    -- slice 7c. encore, taunt: runMoveEffects (battle-actions.ts 1305-1308) `hitResult = target.addVolatile(moveData.volatileStatus, source, move)`,
    -- the move's only effect: it did something exactly when the volatile was added (already there, no HP or a failing onStart: the
    -- move fails, with no Update). A Substitute does not stop them (both are `bypasssub`: 'mrBypassSub'). LATENT: Oblivious (Taunt's
    -- onTryHit), Aroma Veil (onAllyTryAddVolatile) and Mental Herb (onUpdate) are not supported; nor are Magic Bounce and Magic Coat.
    [EOwnVolatile] -> (\r -> (r, mv0)) <$> maybe (pure False) (addVolatile dx tgt) (condByName (mvId mv0))
    -- `target.trySetStatus(moveData.status, ...)`; `!hitResult && move.status` fails the move
    -- (slice 8d: `target.trySetStatus(moveData.status, source, move)`: the user is the source and the move the effect, which Synchronize reads)
    [EStatus st] -> (\r -> (r, mv0)) <$> trySetStatusFx dx tgt st (Just user) moveCause
    -- runMoveEffects (battle-actions.ts 1319-1322): `if (moveData.forceSwitch) hitResult = !!canSwitch(target.side)`, the move's only
    -- effect (Roar, Whirlwind), so it did something only if the target's side can switch
    [] | mrForceSwitch (mvRule mv0) -> (\bb -> (canSwitch bb (playerOfActor tgt) > 0, mv0)) <$> get
    _ -> pure (False, mv0) -- not executable ('Engine.Rule.executable' admits one effect)
  -- runMoveEffects (battle-actions.ts 1349-1353, 1369-1372) for a selfSwitch move: `if (canSwitch(source.side)) didSomething = true;
  -- else didSomething = combineResults(didSomething, false)`. With a switch target the hit did something, whatever the effects
  -- before it returned; without one, `combineResults` keeps a `true` and turns an `undefined` into `false`. A move's own onHit
  -- that returns nothing still gives `true` (`singleEvent` returns its relay value, `true`, for an `undefined` result: battle.ts
  -- 658), so Parting Shot with no target drops the foe's stats and still succeeds, with all its Update events; only a move with
  -- no effect at all (Teleport) would turn `undefined` into `false`, and its onTry has failed it before this. Then the flag step,
  -- before the hit loop's first Update. A move whose selfSwitch was deleted (Parting Shot with no drop) is an ordinary one.
  done <- if mvSelfSwitch mv && not blocked
    then do
      setSelfSwitchFlag user
      can <- gets (\bb -> canSwitch bb (playerOfActor user) > 0)
      pure (can || done0)
    else pure done0
  when done $ do
    -- spreadMoveHit step 6 (battle-actions.ts 1165), before the hit loop's first Update
    unless blocked (forceSwitchStep dx user tgt mv)
    eachEvent dx EvUpdate -- eachEvent('Update') after the hit
    -- resolveFaints({ checkWin: !pokemon.hp }); the user is alive
    userDown <- gets (\bb -> monHp (monOf bb user) <= 0)
    resolveFaints dx False userDown
    eachEvent dx EvUpdate -- second eachEvent('Update')

-- | The end of @runMoveEffects@ (battle-actions.ts 1369-1372): `else if (move.selfSwitch && source.hp && (this.battle.canSwitch(source.side)
-- ...)) source.switchFlag = move.id`, reached when the move did something. The callers only reach it past the checks that make a hit
-- do something (a damaging move that got through immunity and accuracy; a status move 'statusMove' counted as done). It runs inside the
-- hit, before the self drops, secondaries and @DamagingHit@: a user that a Rocky Helmet or Rough Skin then faints has its flag
-- cleared by the faint (@faint@ sets `switchFlag = false`, pokemon.ts 1607: 'applyDamage'), and Life Orb's recoil does the same.
{-# INLINABLE setSelfSwitchFlag #-}
setSelfSwitchFlag :: Monad m => Actor -> Eng m ()
setSelfSwitchFlag user = modify' $ \b ->
  if monHp (monOf b user) > 0 && canSwitch b (playerOfActor user) > 0
    then modifyMon user (\m -> m {monSwitchFlag = SwitchMove}) b
    else b

playerOfActor :: Actor -> Player
playerOfActor (Actor p _) = p

-- | @hitStepMoveHitLoop@ (battle-actions.ts): the hit count, then one @spreadMoveHit@ per hit with an
-- @Update@ event after each, then the faint resolution, recoil and the closing @Update@.
{-# INLINABLE hitLoop #-}
hitLoop :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m Bool
hitLoop fmt dx user target mv = do
  let rule = mvRule mv
  -- targetHits: multihit n is fixed; [2, 5] is battle.sample([2 x7, 3 x7, 4 x3, 5 x3]), one random(20) draw
  targetHits <- case mrHits rule of
    OneHit -> pure 1
    FixedHits n -> pure n
    TwoToFive -> (\i -> if i < 7 then 2 else if i < 14 then 3 else if i < 17 then 4 else 5) <$> roll 0 20
  (total, lastSub, firstFailed) <- hits targetHits (1 :: Int) 0 False mv
  -- slice 8c. `if (hit === 1) return damage.fill(false)` (battle-actions.ts 1037): a first hit whose damage event failed ends the move
  -- here, with no faint resolution, recoil or Update, and the move has failed (`moveResult` false)
  if firstFailed then pure False else afterHits dx user target mv total lastSub >> pure True
  where
    -- the loop `for (hit = 1; hit <= targetHits; hit++)`; returns move.totalDamage
    -- the copy of the move threads through the hits: a Stellar boost spent on one hit stays on it ('mvStellarBoosted')
    -- (slice 7b: it also returns whether the last hit went to a Substitute: that hit's targetsCopy entry is null)
    -- (slice 8c: and whether the loop ended at its first hit because that hit's damage event failed)
    hits n hit acc sub m
      | hit > n = pure (acc, sub, False)
      | otherwise = do
          b <- get
          -- `if (targets.every(target => !target?.hp)) break;`
          if monHp (monOf b target) <= 0
            then pure (acc, sub, False)
            else do
              (mdealt, m', onSub) <- hitMove fmt dx user target m
              case mdealt of
                -- slice 8c. spreadMoveHit's spreadDamage gave `false` (`damage event failed`): the target is dropped from the later steps (no
                -- onHit, self drops, secondaries or DamagingHit) and `if (!moveDamage.some(val => val !== false)) break;` (battle-actions.ts
                -- 1019) ends the loop before this hit's Update. At the first hit the whole move failed ('hitLoop').
                -- From the second hit on, `[moveDamageThisHit, targetsCopy] = this.spreadMoveHit(targetsCopy, ...)` (1010) has put `false` in
                -- targetsCopy (1143), so the later steps that read it see no target, as after a hit on a Substitute: `gotAttacked` (1055) and
                -- the target's AfterMoveSecondary (1069) do not run for it. The earlier hit's flag does not carry over: it is 'True' here.
                -- LATENT: Showdown drops the target only for `false` (battle.ts 2138-2143 returns the handler's value); a `null` or `undefined`
                -- result is no `false`, so the loop goes on and that hit's Update runs (battle-actions.ts 1019-1029; 1150 then drops the target from targetsCopy). The engine's Damage
                -- event cannot tell the two apart ('stop' is `false`), and no supported handler returns `null` for a Move cause (Magic Guard and
                -- Poison Heal return `false`, Sturdy and Endure a damage), so only `false` is modelled. `gotAttacked` is not in the state.
                Nothing -> pure (acc, True, hit == 1)
                Just dealt -> do
                  hitEffects dx user target m' onSub
                  eachEvent dx EvUpdate -- eachEvent('Update') after each hit
                  -- `if (!pokemon.hp && targets.length === 1) break;`
                  userDown <- gets (\bb -> monHp (monOf bb user) <= 0)
                  if userDown then pure (acc + dealt, onSub, False) else hits n (hit + 1) (acc + dealt) onSub m'

-- | The part of 'hitLoop' after the hits (battle-actions.ts @hitStepMoveHitLoop@ 1018-1060): faint resolution, recoil, the closing @Update@
-- and @AfterMoveSecondary@. @total@ is @move.totalDamage@ and @lastSub@ whether the last hit went to a Substitute.
{-# INLINABLE afterHits #-}
afterHits :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Int -> Bool -> Eng m ()
afterHits dx user target mv total lastSub = do
  let rule = mvRule mv
  b <- get
  let userDown = monHp (monOf b user) <= 0
  -- gen >= 5: resolveFaints({ checkWin: !pokemon.hp })
  resolveFaints dx False userDown
  -- applyRecoilDamage (after the loop, from move.totalDamage; battle-actions.ts ~1439)
  let um = monOf b user
  case (mvStruggleRecoil mv, mrRecoil rule) of
    -- struggleRecoil: round(baseMaxhp / 4), at least 1, as directDamage (battle-actions.ts 1449): no Damage event
    -- (slice 7b: `move.totalDamage || (move.struggleRecoil && damage.some((val, i) => val === 0 && !!targetsCopy[i]))`: a last hit on a
    -- Substitute leaves no target in targetsCopy, and its recoil was taken inside the hit ('hitSubstitute'))
    (True, _) | not lastSub -> modify' (applyDamage user (Just user) (Just (EffectKey EffRecoil 0)) (max 1 (roundDiv (monMaxHp um) 4)))
    -- recoil: round(totalDamage * n / d), at least 1, as battle.damage(recoil, pokemon, pokemon, 'recoil'): the Damage event runs
    -- with the 'EffRecoil' effect (Magic Guard and Rock Head read it)
    (_, Just (n, d)) | total > 0 -> void (damageMon dx user (Just user) Nothing (Just (EffectKey EffRecoil 0)) (max 1 (roundDiv (total * n) d)))
    _ -> pure ()
  -- `if (!damage.some(val => !!val || val === 0)) return damage;` guards the next Update: a hit always
  -- yields a numeric damage here (0 counts), so the guard always passes
  eachEvent dx EvUpdate -- second eachEvent('Update')
  -- afterMoveSecondaryEvent: runEvent('AfterMoveSecondary'); frz.onAfterMoveSecondary cures a frozen target of
  -- a thawsTarget move (conditions.ts). The target is passed as a one-element array in Showdown.
  -- slice 7b: `afterMoveSecondaryEvent(targetsCopy.filter(val => !!val), ...)`, and targetsCopy is the last hit's: a last hit that went
  -- to a Substitute left a null there, so the array is empty and no handler (Eject Button, Red Card, frz) runs.
  -- slice 8d: the relay value is @move.totalDamage@ ('RInt'), which Berserk's @onAfterMoveSecondary@ reads from the move; Showdown's event has no
  -- relay value, and no other handler reads it.
  unless lastSub (void (runEvent dx EvAfterMoveSecondary target (Just user) (Just mv) (RInt total)))

-- | The steps of @spreadMoveHit@ after the damage, run once per hit: @selfDrops@ (4), @secondaries@ (5), then
-- the @DamagingHit@ event. They run even when the hit fainted the target (its faint is only queued), so the
-- secondary rolls are drawn but a status or flinch cannot be applied to it.
{-# INLINABLE hitEffects #-}
hitEffects :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Bool -> Eng m ()
hitEffects dx user target mv onSub = do
  let rule = mvRule mv
  -- selfDrops: `self.boosts` always draws random(100) (the chance, when absent, always passes) and applies
  -- to the user. move.selfDropped stops a repeat only for a single hit (`if (!move.multihit)`), so a
  -- multi-hit move draws every hit.
  forM_ (mrSelf rule) $ \e -> case e of
    EBoost bs -> roll 0 100 >> void (boostMon dx (Just user) (Just mv) moveCause user bs)
    _ -> pure ()
  -- secondaries: for each, random(100); below the chance it hits the target (moveHit with isSecondary), then
  -- its `self` effects (selfDrops with isSecondary draws nothing)
  -- slice 8d, @secondaries@ (battle-actions.ts 1401): `runEvent('ModifySecondaries', target, source, moveData, secondaries.slice())` once per hit,
  -- before the draws, and the loop runs over the list it returns (Shield Dust keeps only the secondaries with a `self` effect). A hit on a
  -- Substitute has a null target, whose own handlers are not collected: the list is whole.
  secs <- if null (mrSecondaries rule) || onSub
    then pure (mrSecondaries rule)
    else do
      res <- runEvent dx EvModifySecondaries target (Just user) (Just mv) (RSecondaries (mrSecondaries rule))
      pure (case res of RSecondaries xs -> xs; _ -> [])
  forM_ secs $ \sec -> do
    r <- roll 0 100
    when (r < secChance sec) $ do
      -- slice 7b: a hit that went to a Substitute has a null target: `secondaries` (1398-1411) still draws and still calls moveHit,
      -- whose runMoveEffects does nothing for a null target, and whose selfDrops still applies the secondary's `self` effects
      unless onSub (forM_ (secTarget sec) (applyEffect dx user mv target))
      forM_ (secSelf sec) (applyEffect dx user mv user)
  -- slice 7b: steps 6 and the events after it need a target that took the hit: `forceSwitch` tests `target &&`, and `damagedTargets`
  -- holds only targets with a numeric damage and a Pokemon (battle-actions.ts 1170-1190). A Substitute's hit gives neither.
  unless onSub $ do
   -- spreadMoveHit step 6 (battle-actions.ts 1165): after the secondaries, before DamagingHit
   forceSwitchStep dx user target mv
   -- runEvent('DamagingHit') (after the secondaries, per hit): frz.onDamagingHit cures a frozen target hit by
   -- a damaging Fire move. The target and the damage are one-element arrays in Showdown.
   void (runEvent dx EvDamagingHit target (Just user) (Just mv) RNone)
   -- spreadMoveHit (battle-actions.ts 1184-1186): `if (moveData.onAfterHit && pokemon.hp) singleEvent('AfterHit', moveData, {}, t,
   -- pokemon, move)` for each damaged target, after DamagingHit (gen >= 5) and once per hit. The user's HP is read now: a Rocky
   -- Helmet or Rough Skin that has just brought it to 0 stops the move's AfterHit (its faint is only queued).
   userAlive <- gets (\b -> monHp (monOf b user) > 0)
   when userAlive (moveAfterHit dx user target mv)

-- | @spreadMoveHit@ step 6, @forceSwitch@ (battle-actions.ts 1165, 1413-1428): when the target and the user have HP and the target's
-- side can switch, @runEvent('DragOut', target, source, move)@; a truthy result sets the target's @forceSwitchFlag@, which the
-- action tail's 'phazing' consumes. The @canSwitch@ test comes before the event, so a flag is never set for a side with no one to
-- bring in.
-- LATENT: a @false@ result fails a Status move (`-fail`, damage false); no supported effect listens to @DragOut@, so none returns it.
forceSwitchStep :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
forceSwitchStep dx user target mv = when (mrForceSwitch (mvRule mv)) $ do
  b <- get
  when (monHp (monOf b target) > 0 && monHp (monOf b user) > 0 && canSwitch b (playerOfActor target) > 0) $ do
    r <- runEvent dx EvDragOut target (Just user) (Just mv) RNone
    when (relayTruthy r) (modify' (modifyMon target (\m -> m {monForceSwitchFlag = True})))

-- | One effect of a secondary on a Pokemon (@moveHit@ with @isSecondary@ -> @runMoveEffects@), of the move @mv@ used by @user@.
applyEffect :: MonadChance m => Dex -> Actor -> MoveData -> Actor -> Effect -> Eng m ()
applyEffect dx user mv a e = case e of
  EBoost bs -> void (boostMon dx (Just user) (Just mv) moveCause a bs)
  -- (slice 8d: a secondary's status: `target.trySetStatus(moveData.status, source, move)`)
  EStatus st -> void (trySetStatusFx dx a st (Just user) moveCause)
  -- addVolatile('flinch'): needs HP (no affectsFainted), fails if it is already there; no immunity, no
  -- TryAddVolatile handler, duration 1
  EFlinch -> void (addVolatile dx a CFlinch)
  EHeal _ _ -> pure () -- not executable as a secondary
  -- addVolatile('roost', source, move): `self.volatileStatus` of Roost; needs HP, and Terastallization (not modelled) fails it
  ERoost -> void (addVolatile dx a CRoost)
  EOwnVolatile -> pure () -- run by 'statusMove', never as a secondary
  EOwnHit -> pure () -- run by 'moveOwnHit', never as a secondary
  EOwnHitField -> pure () -- run by 'moveFieldHit', never as a secondary

-- | One @spreadMoveHit@ of a damaging move: crit roll, random factor, damage, drain. Returns the damage
-- actually dealt (@move.totalDamage@ accumulates it).
{-# INLINABLE hitMove #-}
hitMove :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m (Maybe Int, MoveData, Bool)
hitMove fmt dx user target mv = do
  -- spreadMoveHit (battle-actions.ts 1101-1105): singleEvent('TryHit', moveData, {}, target, pokemon, move) opens every hit
  -- (lowkick, grassknot and heavyslam fail against a Dynamaxed target: never here). The hit steps' runEvent('TryHit') already ran.
  -- (Brick Break, Psychic Fangs and Raging Bull shatter the target's screens here, before getDamage: 'moveTryHit')
  moveTryHit dx user target mv
  -- ESCAPE HATCH: substitute (slice 7b). spreadMoveHit step 0 (battle-actions.ts 1115-1119, tryPrimaryHitEvent 1197-1205):
  -- `damage[i] = runEvent('TryPrimaryHit', target, pokemon, moveData)`. HIT_SUBSTITUTE (0) means the Substitute took the hit.
  primary <- runEvent dx EvTryPrimaryHit target (Just user) (Just mv) RNone
  case primary of
    RInt 0 -> hitSubstitute fmt dx user target mv
    _ -> hitTarget fmt dx user target mv

-- | The damage of one hit (@getDamage@, battle-actions.ts 1650-1779) of a move on a Pokemon: the damage callback or a fixed damage, which
-- draw nothing, or the full formula.
{-# INLINABLE hitDamage #-}
hitDamage :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m (Int, MoveData)
hitDamage fmt dx user target mv = do
  b <- get
  -- getDamage (battle-actions.ts 1665): `if (move.damageCallback) return move.damageCallback.call(...)`: the damage, with no
  -- crit roll, BasePower, random factor or modifier (and so no draw)
  callback <- damageCallback mv target
  -- `if (move.damage === 'level') return source.level; else if (move.damage) return move.damage` (1666-1670), right after the
  -- damageCallback line and before the crit roll too: the same early return, so no draw (Seismic Toss, Night Shade)
  let um = monOf b user
      fixed = case (callback, mrDamage (mvRule mv)) of
        (Just d, _) -> Just d
        (_, Just DamageLevel) -> Just (monLevel um)
        (_, Just (DamageFixed n)) -> Just n
        _ -> Nothing
  maybe (getDamage fmt dx user target mv) (\d -> pure (d, mv)) fixed

-- | ESCAPE HATCH: substitute (slice 7b). The hit of a damaging move that the Substitute takes: the body of @substitute.condition.
-- onTryPrimaryHit@ (data/moves.ts 18348-18381) after its test ('Engine.Dispatch', 'NSubstituteTryPrimaryHit'), and what
-- @spreadMoveHit@ does for the null target that @HIT_SUBSTITUTE@ leaves (1122-1126).
-- The damage is @getDamage@'s, with its crit roll and random factor, capped at the Substitute's HP. The Substitute loses it and goes at 0.
-- The user takes recoil from the damage to the Substitute (`applyRecoilDamage`: at least 1, and each hit on its own, because the
-- hit loop's total only counts hits on the Pokemon) and heals `ceil(damage * n / d)` of a drain move (spreadDamage rounds; this one
-- rounds up). Then the move's @onAfterSubDamage@ and the @AfterSubDamage@ event. The Pokemon takes nothing: the target is null, so
-- @getSpreadDamage@ and @spreadDamage@ skip it (damage 0), @runMoveEffects@ runs none of its target steps (no onHit, status, boost, ...), but
-- its @selfSwitch@ steps still set the user's flag (the hit counts as having done something: 0 is not falsy there). The self drops and
-- the secondaries still run ('hitEffects'); the force switch, @DamagingHit@, @AfterHit@ and @EmergencyExit@ do not.
-- Protect-like moves block before this (their volatile's `TryHit`, priority 3, runs in the hit steps before the first spreadMoveHit).
{-# INLINABLE hitSubstitute #-}
hitSubstitute :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m (Maybe Int, MoveData, Bool)
hitSubstitute fmt dx user target mv = do
  b <- get
  let rule = mvRule mv
      um = monOf b user
      subHp = fromMaybe 0 (monSubstituteHp (monOf b target))
  (dmg, mv') <- hitDamage fmt dx user target mv
  let dealt = min dmg subHp -- `if (damage > target.volatiles['substitute'].hp) damage = ...hp`
      left = subHp - dealt
  -- `target.volatiles['substitute'].hp -= damage; ... if (hp <= 0) target.removeVolatile('substitute')` (the End event only logs)
  if left <= 0
    then modify' (modifyMon target (removeVolatile (fromEnum CSubstitute)))
    else modify' (modifyMon target (\x -> x {monVolatiles = SV.map (\v@(Volatile j st) -> if j == fromEnum CSubstitute then Volatile j st {esA = left} else v) (monVolatiles x)}))
  -- `if (damage) this.actions.applyRecoilDamage(damage, move, source)`
  when (dealt > 0) $ case (mvStruggleRecoil mv, mrRecoil rule) of
    (True, _) -> modify' (applyDamage user (Just user) (Just (EffectKey EffRecoil 0)) (max 1 (roundDiv (monMaxHp um) 4)))
    (_, Just (n, d)) -> void (damageMon dx user (Just user) Nothing (Just (EffectKey EffRecoil 0)) (max 1 (roundDiv (dealt * n) d)))
    _ -> pure ()
  -- `if (move.drain) this.heal(Math.ceil(damage * move.drain[0] / move.drain[1]), source, target, 'drain')`
  forM_ (mrDrain rule) $ \(n, d) -> modify' (healMon user ((dealt * n + d - 1) `quot` d))
  moveAfterSubDamage dx user target mv' dealt
  -- runMoveEffects' selfSwitch steps (1349-1353, 1369-1372): see above
  when (mvSelfSwitch mv') (setSelfSwitchFlag user)
  pure (Just 0, mv', True)

-- | The rest of 'hitMove' for a hit on the Pokemon itself.
{-# INLINABLE hitTarget #-}
hitTarget :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m (Maybe Int, MoveData, Bool)
hitTarget fmt dx user target mv = do
  (dmg, mv') <- hitDamage fmt dx user target mv
  -- spreadDamage (battle.ts 2107-2154) through 'damageMon': the clamp to at least 1 (unless 0), `runEvent('Damage', target, source,
  -- effect, targetDamage, true)` with the move as the effect (Endure's onDamage leaves the target 1 HP), the second clamp and
  -- Pokemon.damage; `dealt` is the damage returned.
  -- Slice 8c. A falsy result of the event (`damage event failed`) makes the damage array's entry `false` (spreadDamage, battle.ts 2138-2143): `Nothing`,
  -- which drops the target from every later step of the hit (no drain, onHit, selfSwitch, self drops, secondaries or DamagingHit) and which
  -- 'hitLoop' reads as `false` too. No handler returns one for a Move cause: Magic Guard acts on every effect but a Move, Poison Heal on
  -- psn and tox, and Sturdy and Endure return a damage.
  -- A hit on a Substitute ('hitSubstitute') is no spreadDamage (its target is null there: the damage is
  -- `target.volatiles['substitute'].hp -= damage`), so no Damage event runs for it.
  mdealt <- damageMon dx target (Just user) (Just mv') moveCause dmg
  case mdealt of
    Nothing -> pure (Nothing, mv', False)
    Just dealt -> hitTargetRest dx user target mv mv' dealt

-- | The rest of 'hitTarget' once the damage was dealt (@dealt@: at most the target's HP; 0 counts).
{-# INLINABLE hitTargetRest #-}
hitTargetRest :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> MoveData -> Int -> Eng m (Maybe Int, MoveData, Bool)
hitTargetRest dx user target mv mv' dealt = do
  -- spreadDamage, effect.drain (battle.ts ~2187): heal the user round(dealt * n / d); battle.heal turns a
  -- positive amount below 1 into 1 and does nothing for 0, at full HP or when the user is down
  case mrDrain (mvRule mv) of
    Just (n, d) | dealt > 0 -> modify' (healMon user (roundDiv (dealt * n) d))
    _ -> pure ()
  -- runMoveEffects (battle-actions.ts 1338, step 3 of spreadMoveHit): the move's own onHit, after the damage and the drain and
  -- before self drops, secondaries and DamagingHit (Clear Smog clears the target's stages, a fainted target's too).
  -- LATENT: a damaging move's onHit that returns false (the target would be dropped from the steps after it) is not modelled;
  -- Clear Smog's never does.
  moved <- if mrOnHit (mvRule mv) == [EOwnHit] then snd <$> moveOwnHit dx user target mv else pure mv
  -- runMoveEffects' selfSwitch steps (1349-1353, 1369-1372): the hit landed, so the move did something (damage is a number)
  when (mvSelfSwitch moved) (setSelfSwitchFlag user)
  pure (Just dealt, mv', False)

-- | @getDamage@ (battle-actions.ts 1650-1779) of a move with no @damageCallback@ and no fixed @damage@: base power (its callback,
-- the crit roll, @BasePower@), the stats (with the overrides of Body Press, Psyshock, Foul Play and the ignored defensive stage of
-- Sacred Sword), the base damage, then @modifyDamage@.
{-# INLINABLE getDamage #-}
getDamage :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m (Int, MoveData)
getDamage fmt dx user target mv = do
  b <- get
  let um = monOf b user
      tm = monOf b target
  -- getDamage: `basePower = move.basePowerCallback.call(this.battle, source, target, move)` (1675-1677, before the crit
  -- roll), then `basePower = this.battle.clampIntRange(basePower, 1)`
  bp0 <- basePowerCallback dx user target mv
  -- getDamage: crit roll, then modifyDamage's random(16)
  crit0 <- case critDenominator (mvCritRatio mv) of
    Nothing -> pure False
    Just d -> randomChance 1 d
  -- slice 8c. getDamage (battle-actions.ts 1704-1706): `if (moveHit.crit) moveHit.crit = this.battle.runEvent('CriticalHit', target, null, move)`,
  -- after the roll and so after its draw: a falsy result (Shell Armor's `onCriticalHit: false`) cancels the critical hit
  crit <- if crit0 then relayTruthy <$> runEvent dx EvCriticalHit target Nothing (Just mv) RNone else pure False
  -- getDamage: after the crit roll, runEvent('BasePower', source, target, move, basePower, true) and the same clamp
  -- again. `onEffect` puts the move's own onBasePower first in the list, which is then sorted with the others
  -- ('runEventOnEffect'); Fickle Beam's randomChance is drawn when its handler runs, after the crit roll. A handler only ever
  -- raises the power here, so the `!basePower` early return (damage 0, no further draw) is unreachable
  power1 <- (\r -> case r of RInt p -> max 1 p; _ -> max 1 bp0) <$> runEventOnEffect dx EvBasePower user (Just target) mv (RInt (max 1 bp0))
  -- getDamage (1717-1726): the Terastallized user's 60 base-power floor, after the BasePower event and the clamp
  let power = teraFloor dx um mv power1
  -- getDamage (battle-actions.ts 1729-1756): the Pokemon and the stat each side of the formula reads.
  -- `attacker = move.overrideOffensivePokemon === 'target' ? target : source` (Foul Play: the target's Attack and stage),
  -- `attackStat = move.overrideOffensiveStat || (isPhysical ? 'atk' : 'spa')` (Body Press: Defense), `defenseStat =
  -- move.overrideDefensiveStat || (isPhysical ? 'def' : 'spd')` (Psyshock, Secret Sword: Defense). The defender is the
  -- target (`overrideDefensivePokemon` is not supported). The stage is the one of that Pokemon's own stat.
  let rule = mvRule mv
      attacker = if mrOverrideOffensivePokemon rule then tm else um
      atkSel = fromMaybe (if mvPhysical mv then BAtk else BSpa) (mrOverrideOffensiveStat rule)
      defSel = fromMaybe (if mvPhysical mv then BDef else BSpd) (mrOverrideDefensiveStat rule)
      atkStage0 = boostOf atkSel (monBoosts attacker)
      defStage0 = boostOf defSel (monBoosts tm)
      -- a critical hit sets ignoreNegativeOffensive / ignorePositiveDefensive: the attacker's negative stage of the stat read
      -- and the defender's positive one count as 0; `move.ignoreDefensive` (Sacred Sword) zeroes the defender's stage
      -- whatever its sign (`ignoreOffensive` has no user)
      atkStage = if crit && atkStage0 < 0 then 0 else atkStage0
      defStage = if mrIgnoreDefensive rule || (crit && defStage0 > 0) then 0 else defStage0
      atkStat = statOf atkSel (monStats attacker)
      defStat = statOf defSel (monStats tm)
      -- `attackStat = (category === 'Physical' ? 'atk' : 'spa')` is assigned again before the events (1763), so the
      -- attacker's event follows the move's category; the defender's follows `defenseStat`, which is not reassigned: Psyshock
      -- (Special, Defense) runs ModifyDef on the target
      atkEv = if mvPhysical mv then EvModifyAtk else EvModifySpA
      defEv = eventOfStat defSel
  -- slice 8b. getDamage (battle-actions.ts 1759-1760): `attacker.calculateStat(attackStat, atkBoosts, 1, source)` and `defender.calculateStat(defenseStat,
  -- defBoosts, 1, target)`. Each runs @ModifyBoost@ on its stat user (pokemon.ts 602: `statUser || this`, so the move's user for the attack and the target
  -- for the defense, even for Foul Play, whose boosts are the target's) with the one stage as the table; the stage then scales the stat.
  atkStageM <- modifyBoost dx user atkSel atkStage
  defStageM <- modifyBoost dx target defSel defStage
  let attack0 = boostedStat atkStat atkStageM
      defense0 = boostedStat defStat defStageM
  -- getDamage: attack = runEvent('ModifyAtk' | 'ModifySpA', source, target, move, attack) and the defender's
  -- runEvent('ModifyDef' | 'ModifySpD', target, source, move, defense)
  attack <- fromRelay attack0 <$> runEvent dx atkEv user (Just target) (Just mv) (RInt attack0)
  defense <- fromRelay defense0 <$> runEvent dx defEv target (Just user) (Just mv) (RInt defense0)
  let base = baseDamage fmt (monLevel um) power attack defense
  -- modifyDamage (battle-actions.ts 1783-1804): `baseDamage += 2`, then (the spread and Parental Bond modifiers are not
  -- here) the weather modifier, `priorityEvent('WeatherModifyDamage', pokemon, target, move, baseDamage)`, before the
  -- crit multiplier and the random factor. The event draws nothing.
  weathered <- fromRelay (base + 2) <$> runEvent dx EvWeatherModifyDamage user (Just target) (Just mv) (RInt (base + 2))
  r <- roll 0 16
  -- modifyDamage: ... runEvent('ModifyDamage', pokemon, target, move, baseDamage), then the minimum 1 and the 16-bit truncation
  -- (slice 8b: the burn's halving skips a Guts holder, `!pokemon.hasAbility('guts')`, battle-actions.ts 1874)
  let pre = modifyDamage fmt dx um tm mv crit (hasAbility dx b user "guts") r weathered
  dmg <- finalDamage fmt . fromRelay pre <$> runEventCrit dx crit EvModifyDamage user (Just target) (Just mv) (RInt pre)
  -- modifyDamage's STAB step (1835-1841): a Stellar user's boost marks the move in use and pushes its type onto
  -- @stellarBoostedTypes@ (never cleared: pokemon.ts 504 is the constructor). The marker is set where the state is written, the
  -- hit's result is the same either way.
  -- LATENT: Terapagos-Stellar does not push (1839); Terapagos has no supported forme.
  if stellarBoosts dx um mv
    then do
      modify' (modifyMon user (\x -> x {monStellarBoosted = setBit (monStellarBoosted x) (mvType mv)}))
      pure (dmg, mv {mvStellarBoosted = True})
    else pure (dmg, mv)

-- | The stat of a Pokemon a stat selector names (only the five stats the overrides can name).
statOf :: BoostStat -> Stats -> Int
statOf sel st = case sel of
  BAtk -> stAtk st
  BDef -> stDef st
  BSpa -> stSpa st
  BSpd -> stSpd st
  _ -> stSpe st

-- | The @Modify<Stat>@ event of @'Modify' + statTable[defenseStat]@ (battle-actions.ts 1767).
eventOfStat :: BoostStat -> EventId
eventOfStat sel = case sel of
  BAtk -> EvModifyAtk
  BDef -> EvModifyDef
  BSpa -> EvModifySpA
  BSpd -> EvModifySpD
  _ -> EvModifySpe

-- | The integer an event returned, or the one it started with.
fromRelay :: Int -> Relay -> Int
fromRelay dflt r = case r of
  RInt v -> v
  _ -> dflt

-- | @Math.round(n / d)@ for non-negative @n@ and positive @d@ (exact: ties round up, as in JavaScript).
roundDiv :: Int -> Int -> Int
roundDiv n d = (2 * n + d) `quot` (2 * d)
