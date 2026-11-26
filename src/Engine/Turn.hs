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
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.State.Strict (execStateT, get, gets, modify')
import Data.Maybe (isNothing)
import qualified Data.Vector.Strict as SV
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
        }
  pure Battle
    { bFormat = fmt
    , bTurn = 0
    , bRequest = ReqTeamPreview
    , bResult = Ongoing
    , bMidTurn = True
    , bQueue = SV.singleton start
    , bFaintQueue = SV.empty
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
        | isLegal b p c, Just as <- choiceActions b kind p c = Right as
        | otherwise = Left (IllegalChoice p c)
{-# INLINABLE step #-}

-- | Build the actions of a legal choice (Showdown @side.choose@ -> @choice.actions@).
choiceActions :: Battle -> RequestKind -> Player -> Choice -> Maybe [Action]
choiceActions b kind p c = case (kind, c) of
  (TeamPreview, Team xs) -> Just
    [ mk KTeam (Just (Actor p (x - 1))) (negate i) i
    | (i, x) <- zip [0 ..] xs
    ]
  (MoveRequest, Move n)
    | mustStruggle b p -> Just [mk KMove active 0 (-1)]
    | otherwise -> Just [mk KMove active 0 (n - 1)]
  (MoveRequest, Switch n) -> switchTo KSwitch n
  (SwitchRequest, Switch n) -> switchTo KInstaSwitch n
  _ -> Nothing
  where
    s = sideOf p b
    active = Just (activeActor p b)
    mk k actor prio arg = Action
      { acKind = k, acOrder = 0, acPriority = prio, acSpeed = 0, acActor = actor, acArg = arg }
    switchTo k n = (\o -> [mk k active 0 o]) <$> sSlots s U.!? (n - 1)

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
    plain k = Action {acKind = k, acOrder = 0, acPriority = 0, acSpeed = 0, acActor = Nothing, acArg = 0}
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
          runMove fmt dx a (acArg ac)
          afterAction dx ac
        _ -> pure () -- the Pokemon left the field or fainted: no tail (battle.ts 'move' case returns false)
    KInstaSwitch -> switchAction dx ac >> afterAction dx ac
    KSwitch -> switchAction dx ac >> afterAction dx ac
    KRunSwitch -> do
      forM_ (acActor ac) (runSwitch dx)
      afterAction dx ac
    KBeforeTurn -> do
      eachEvent dx EvBeforeTurn -- eachEvent('BeforeTurn')
      afterAction dx ac
    KResidual -> do
      updateSpeedAll dx
      fieldEvent dx EvResidual Nothing -- fieldEvent('Residual')
      afterAction dx ac
{-# INLINABLE runAction #-}

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
    switchIn dx p (activeOrig s)
  modify' (\b -> b {bMidTurn = True})

-- | @case 'switch'@ / @'instaswitch'@: switch the target in at the actor's position.
{-# INLINABLE switchAction #-}
switchAction :: MonadChance m => Dex -> Action -> Eng m ()
switchAction dx ac = case acActor ac of
  Just (Actor p _) -> switchIn dx p (acArg ac)
  Nothing -> pure ()

-- | The part of @runAction@ after the action's own work.
{-# INLINABLE afterAction #-}
afterAction :: MonadChance m => Dex -> Action -> Eng m ()
afterAction dx ac = do
  -- fainting: resolveFaints({ checkWinIfNoFaints: true, checkWin: true })
  resolveFaints dx True True
  ended <- gets ((/= Ongoing) . bResult)
  unless ended $ do
    next <- gets (peekQueue . bQueue)
    -- switching (fainted pokemon): only when the queue is exhausted
    when (isNothing next) checkFainted
    -- gen >= 5: eachEvent('Update') unless the next action is an instaswitch
    when (acKind ac /= KStart && fmap acKind next /= Just KInstaSwitch) (eachEvent dx EvUpdate)
    flagged <- switchRequests
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

-- | Which sides have a pending forced switch; sides that cannot switch have their flag cleared (battle.ts runAction).
{-# INLINABLE switchRequests #-}
switchRequests :: Monad m => Eng m [Bool]
switchRequests = do
  r <- mapM one [P1, P2]
  pure r
  where
    one p = do
      b <- get
      let s = sideOf p b
          flagged = sHasActive s && maybe False monSwitchFlag (slotMon s 0)
      if flagged && canSwitch b p == 0
        then do
          modify' (modifyMon (activeActor p b) (\m -> m {monSwitchFlag = False}))
          pure False
        else pure flagged

-- ---------------------------------------------------------------------------
-- Faints and the winner

-- | @battle.canSwitch(side)@: number of unfainted Pokemon in the back slots not already queued to switch in.
canSwitch :: Battle -> Player -> Int
canSwitch b p
  | sLeft s == 0 = 0
  | otherwise = length
      [ ()
      | i <- [1 .. U.length (sSlots s) - 1]
      , Just o <- [sSlots s U.!? i]
      , not (monFainted (monOf b (Actor p o)))
      , o `notElem` queued
      ]
  where
    s = sideOf p b
    queued =
      [ acArg a
      | a <- SV.toList (bQueue b)
      , acKind a `elem` [KSwitch, KInstaSwitch]
      , fmap (\(Actor q _) -> q) (acActor a) == Just p
      ]

-- | @battle.checkFainted@: flag fainted active Pokemon for replacement when their side can switch.
{-# INLINABLE checkFainted #-}
checkFainted :: Monad m => Eng m ()
checkFainted = forM_ [P1, P2] $ \p -> do
  b <- get
  let s = sideOf p b
      a = activeActor p b
  when (canSwitch b p > 0 && sHasActive s && monFainted (monOf b a)) $
    -- pokemon.status = 'fnt'; pokemon.switchFlag = true
    modify' (modifyMon a (\m -> m {monSwitchFlag = True, monStatus = StatusFainted}))

-- ---------------------------------------------------------------------------
-- Switching

-- | @actions.switchIn@ (battle-actions.ts) for singles position 0, without events or messages.
--
-- LATENT: a drag (Roar, Whirlwind, Dragon Tail) skips @runPursuitActivation@ and runs @runSwitch@ at once
-- (battle-actions.ts ~97, ~208), and @isStarted@ (battle-actions.ts ~241) is not modelled; both matter
-- once forced switches and switch-in effects exist.
{-# INLINABLE switchIn #-}
switchIn :: MonadChance m => Dex -> Player -> Int -> Eng m ()
switchIn dx p target = do
  b0 <- get
  let s0 = sideOf p b0
      tgt = Actor p target
  unless (monActive (monOf b0 tgt)) $ do
    let old = if sHasActive s0 then Just (activeActor p b0) else Nothing
    case old of
      Just o | monHp (monOf b0 o) > 0 -> do
        -- runPursuitActivation(oldActive): eachEvent('Update') (the BeforeSwitchOut event has no handlers)
        eachEvent dx EvUpdate
        -- singleEvent('End', oldActive.getAbility(), ...) and singleEvent('End', oldActive.getItem(), ...) (battle-actions.ts
        -- 142-143: Protosynthesis, Quark Drive and Unburden remove their volatile)
        endAbilityAndItem dx o
        -- queue.cancelAction(oldActive); oldActive.clearVolatile() (resets the stat stages, the volatiles and the cached speed; the status, and toxic's stage, stay)
        modify' (\b -> modifyMon o (\m -> m {monSpeed = stSpe (monStats m), monBoosts = noBoosts, monVolatiles = SV.empty}) b {bQueue = cancelAction o (bQueue b)})
      _ -> pure ()
    case old of
      Just o -> do
        -- oldActive.isActive = false; `if (oldActive.fainted) oldActive.status = ''`
        modify' (modifyMon o (\m -> m {monActive = False, monStatus = if monFainted m then StatusNone else monStatus m}))
        -- the old active and the incoming Pokemon trade slots
        modify' (modifySide p (\s -> s {sSlots = swapSlot0 target (sSlots s)}))
      Nothing -> pure ()
    modify' (modifyMon tgt (\m -> m {monActive = True}) . modifySide p (\s -> s {sHasActive = True}))
    -- pokemon.abilityState = initEffectState({ id: pokemon.ability, target: pokemon }); pokemon.itemState =
    -- initEffectState({ id: pokemon.item, target: pokemon }) (battle-actions.ts 185-186). The Pokemon is active, so a
    -- state with an id takes the next effectOrder: the ability always (`noability` is an id), the item when held
    let monT = monOf b0 tgt
    abilityOrder <- nextEffectOrder (abilityId dx (monAbility monT) /= "")
    itemOrder <- nextEffectOrder (monItem monT /= noItem)
    modify' (modifyMon tgt (\m -> m {monAbilityState = noEffectState {esOrder = abilityOrder}, monItemState = noEffectState {esOrder = itemOrder}}))
    -- queue.insertChoice({ choice: 'runSwitch', pokemon }): updateSpeed first, then a tie-aware insert
    getActionSpeed dx tgt >>= \sp -> modify' (modifyMon tgt (\m -> m {monSpeed = sp}))
    new <- resolveAction dx Action
      { acKind = KRunSwitch, acOrder = 0, acPriority = 0, acSpeed = 0, acActor = Just tgt, acArg = 0 }
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
runMove fmt dx user@(Actor p _) slot = do
  b <- get
  let target = activeActor (other p) b
      um = monOf b user
  case (slot < 0, monMoves um SV.!? slot) of
    (True, _) ->
      -- pokemon.deductPP finds no Struggle slot and returns 0, which runMove allows for struggle
      go target (dxStruggle dx) (pure ())
    -- LATENT: Showdown runs BeforeMove (with its frz/par draws) before the PP check (battle-actions.ts runMove
    -- ~312, then deductPP ~339); here a move at 0 PP returns before BeforeMove. Unreachable while a chosen move
    -- always has PP; it goes live with PP drain mid-turn (Pressure, Spite, Eerie Spell).
    (False, Just ms)
      | msPP ms > 0 ->
          -- pokemon.deductPP
          go target (moveData dx (msMove ms))
            (modify' (modifyMon user (\m -> m {monMoves = SV.imap (\i x -> if i == slot then x {msPP = msPP x - 1} else x) (monMoves m)})))
    _ -> pure ()
  where
    go target mv deductPP = do
      -- runEvent('BeforeMove'): before PP is deducted; a move stopped here costs no PP and returns from
      -- runMove without the closing resolveFaints / checkWin (there is nothing to resolve)
      -- (a move on the user targets the user: battle.getRandomTarget returns the Pokemon for 'self')
      -- (battle.getRandomTarget: `self`, `all`, `allySide` and `allyTeam` return the Pokemon; foeSide the foe)
      let eventTarget = if mrTarget (mvRule mv) `elem` [TargetSelf, TargetAllySide, TargetAll] then user else target
      proceed <- relayTruthy <$> runEvent dx EvBeforeMove user (Just eventTarget) (Just mv) RNone
      when proceed $ do
        void deductPP
        -- useMove's ModifyMove event: a frozen user of a defrost move thaws (conditions.ts frz.onModifyMove)
        void (runEvent dx EvModifyMove user (Just eventTarget) (Just mv) RNone)
        useMove fmt dx user target mv
        -- resolveFaints(); checkWin()
        resolveFaints dx False True
        void (checkWin Nothing)

-- | The hit steps of @trySpreadMoveHit@ (battle-actions.ts): target check, type immunity, accuracy, then the
-- hit loop. Spread targets and @randomNormal@ reach the one foe in singles ('TargetFoe'); a move on the
-- user ('TargetSelf') skips immunity and accuracy (@hitStepAccuracy@: a self-targeted status move).
{-# INLINABLE useMove #-}
useMove :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m ()
useMove fmt dx user target mv = case mrTarget rule of
  TargetSelf -> statusMove dx user user mv
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
     -- LATENT: the Invulnerability event (step 0), the Try and PrepareHit events of the move and runEvent('PrepareHit')
     -- of the hit steps have no handler.
     tryHit <- relayTruthy <$> runEvent dx EvTryHit target (Just user) (Just mv) RNone
     when tryHit $
      -- hitStepTypeImmunity (before accuracy: an immune target consumes no draws); a move with
      -- ignoreImmunity (every status move but a few) skips it
      unless (not (mrIgnoreImmunity rule) && isImmune dx (mvType mv) tm) $
       -- hitStepTryImmunity (battle-actions.ts 735): `gen >= 6 && move.flags['powder'] && target !== pokemon &&
       -- !this.dex.getImmunity('powder', target)`: Grass types are immune to a powder move (no draw)
       unless (mrPowder rule && typeImmuneTo dx "powder" tm) $ do
        -- hitStepAccuracy
        hits <- if mvAccuracy mv == neverMisses || (isToxic && hasTypeNamed dx "Poison" um) then pure True
          else randomChance (stagedAccuracy fmt (mvAccuracy mv) (monBoosts um) (monBoosts tm)) 100
        when hits $
          if mrDamaging rule
            then do
              hitLoop fmt dx user target mv
              -- useMoveInner: the move succeeded (`moveResult`), the user is queued to faint if it has no HP, then
              -- runEvent('AfterMoveSecondarySelf', pokemon, target, move): Life Orb's recoil. A status move
              -- raises it too, but no supported handler acts on one.
              void (runEvent dx EvAfterMoveSecondarySelf user (Just target) (Just mv) RNone)
            else statusMove dx user target mv
  where
    rule = mvRule mv
    -- ID-SPECIAL: battle-actions.ts hitStepAccuracy ~797 (and hitStepInvulnerabilityEvent ~693): in gen >= 8 Toxic
    -- used by a Poison-type skips the accuracy check (`accuracy = true`), so it draws nothing
    isToxic = mrOnHit rule == [EStatus Tox] && mvId mv == "toxic"

-- | @tryMoveHit@ (battle-actions.ts 883-919) of a move on a side or the field (@foeSide@, @allySide@, @all@): no
-- type immunity, accuracy or crit draw. The event points are @singleEvent('Try')@, @singleEvent('PrepareHit')@,
-- @runEvent('PrepareHit')@, then @runEvent('TryHitField')@ for @all@ or @runEvent('TryHitSide')@, then @moveHit@.
-- The @Try@ and @PrepareHit@ callbacks are the move's own (@onTry@, @onPrepareHit@), which a supported move has none
-- of; the @runEvent@s sort the handlers of the Pokemon in play (none attached today: Magic Bounce's
-- @onAllyTryHitSide@ is unsupported), and a sort over fewer than two handlers draws nothing, but they stay so that
-- a handler that appears later is sorted where Showdown sorts it. A falsy result fails the move.
-- @useMoveInner@ then runs @AfterMoveSecondarySelf@ (battle-actions.ts ~607), which this path skips like the
-- Pokemon-targeted status path: its only listener, Life Orb, is gated on @move.category !== 'Status'@, so no
-- supported handler can act. Unlike a Pokemon-targeted status move there are no @Update@ events inside: @hitStepMoveHitLoop@ is not used.
{-# INLINABLE sideMove #-}
sideMove :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
sideMove dx user target mv = do
  prepared <- relayTruthy <$> runEvent dx EvPrepareHit user (Just target) (Just mv) RNone
  when prepared $ do
    let tryHit = if mrTarget (mvRule mv) == TargetAll then EvTryHitField else EvTryHitSide
    ok <- relayTruthy <$> runEvent dx tryHit target (Just user) (Just mv) RNone
    when ok (moveHitSide dx user target mv)

-- | @moveHit@ -> @spreadMoveHit@ -> @runMoveEffects@ (battle-actions.ts 1430, 1175, 1330) of a side-targeted move:
-- the damage is @undefined@ (basePower 0), so only the effect on the target's side runs.
-- The @TryHitSide@ and @TryHitField@ callbacks of the move itself (@onTryHitSide@, ...) are not supported.
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
statusMove dx user tgt mv = do
  b <- get
  let tm = monOf b tgt
  done <- case mrOnHit (mvRule mv) of
    [EHeal n d]
      | monHp tm >= monMaxHp tm -> pure False
      -- amount = baseMaxhp * n / d; battle.heal(Math.round(amount), ...) (gen >= 5)
      | otherwise -> modify' (healMon tgt (roundDiv (monMaxHp tm * n) d)) >> pure True
    [EBoost bs] -> boostMon tgt bs
    -- `target.trySetStatus(moveData.status, ...)`; `!hitResult && move.status` fails the move
    [EStatus st] -> trySetStatus dx tgt st
    _ -> pure False -- not executable ('Engine.Rule.executable' admits one effect)
  when done $ do
    eachEvent dx EvUpdate -- eachEvent('Update') after the hit
    -- resolveFaints({ checkWin: !pokemon.hp }); the user is alive
    userDown <- gets (\bb -> monHp (monOf bb user) <= 0)
    resolveFaints dx False userDown
    eachEvent dx EvUpdate -- second eachEvent('Update')

-- | @hitStepMoveHitLoop@ (battle-actions.ts): the hit count, then one @spreadMoveHit@ per hit with an
-- @Update@ event after each, then the faint resolution, recoil and the closing @Update@.
{-# INLINABLE hitLoop #-}
hitLoop :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m ()
hitLoop fmt dx user target mv = do
  let rule = mvRule mv
  -- targetHits: multihit n is fixed; [2, 5] is battle.sample([2 x7, 3 x7, 4 x3, 5 x3]), one random(20) draw
  targetHits <- case mrHits rule of
    OneHit -> pure 1
    FixedHits n -> pure n
    TwoToFive -> (\i -> if i < 7 then 2 else if i < 14 then 3 else if i < 17 then 4 else 5) <$> roll 0 20
  total <- hits targetHits (1 :: Int) 0
  b <- get
  let userDown = monHp (monOf b user) <= 0
  -- gen >= 5: resolveFaints({ checkWin: !pokemon.hp })
  resolveFaints dx False userDown
  -- applyRecoilDamage (after the loop, from move.totalDamage; battle-actions.ts ~1439)
  let um = monOf b user
  case (mvStruggleRecoil mv, mrRecoil rule) of
    -- struggleRecoil: round(baseMaxhp / 4), at least 1, as directDamage
    (True, _) -> modify' (applyDamage user (max 1 (roundDiv (monMaxHp um) 4)))
    -- recoil: round(totalDamage * n / d), at least 1, as damage(.., 'recoil')
    (_, Just (n, d)) | total > 0 -> modify' (applyDamage user (max 1 (roundDiv (total * n) d)))
    _ -> pure ()
  -- `if (!damage.some(val => !!val || val === 0)) return damage;` guards the next Update: a hit always
  -- yields a numeric damage here (0 counts), so the guard always passes
  eachEvent dx EvUpdate -- second eachEvent('Update')
  -- afterMoveSecondaryEvent: runEvent('AfterMoveSecondary'); frz.onAfterMoveSecondary cures a frozen target of
  -- a thawsTarget move (conditions.ts). The target is passed as a one-element array in Showdown.
  void (runEvent dx EvAfterMoveSecondary target (Just user) (Just mv) RNone)
  where
    -- the loop `for (hit = 1; hit <= targetHits; hit++)`; returns move.totalDamage
    hits n hit acc
      | hit > n = pure acc
      | otherwise = do
          b <- get
          -- `if (targets.every(target => !target?.hp)) break;`
          if monHp (monOf b target) <= 0
            then pure acc
            else do
              dealt <- hitMove fmt dx user target mv
              hitEffects dx user target mv
              eachEvent dx EvUpdate -- eachEvent('Update') after each hit
              -- `if (!pokemon.hp && targets.length === 1) break;`
              userDown <- gets (\bb -> monHp (monOf bb user) <= 0)
              if userDown then pure (acc + dealt) else hits n (hit + 1) (acc + dealt)

-- | The steps of @spreadMoveHit@ after the damage, run once per hit: @selfDrops@ (4), @secondaries@ (5), then
-- the @DamagingHit@ event. They run even when the hit fainted the target (its faint is only queued), so the
-- secondary rolls are drawn but a status or flinch cannot be applied to it.
{-# INLINABLE hitEffects #-}
hitEffects :: MonadChance m => Dex -> Actor -> Actor -> MoveData -> Eng m ()
hitEffects dx user target mv = do
  let rule = mvRule mv
  -- selfDrops: `self.boosts` always draws random(100) (the chance, when absent, always passes) and applies
  -- to the user. move.selfDropped stops a repeat only for a single hit (`if (!move.multihit)`), so a
  -- multi-hit move draws every hit.
  forM_ (mrSelf rule) $ \e -> case e of
    EBoost bs -> roll 0 100 >> void (boostMon user bs)
    _ -> pure ()
  -- secondaries: for each, random(100); below the chance it hits the target (moveHit with isSecondary), then
  -- its `self` effects (selfDrops with isSecondary draws nothing)
  forM_ (mrSecondaries rule) $ \sec -> do
    r <- roll 0 100
    when (r < secChance sec) $ do
      forM_ (secTarget sec) (applyEffect dx target)
      forM_ (secSelf sec) (applyEffect dx user)
  -- runEvent('DamagingHit') (after the secondaries, per hit): frz.onDamagingHit cures a frozen target hit by
  -- a damaging Fire move. The target and the damage are one-element arrays in Showdown.
  void (runEvent dx EvDamagingHit target (Just user) (Just mv) RNone)

-- | One effect of a secondary on a Pokemon (@moveHit@ with @isSecondary@ -> @runMoveEffects@).
applyEffect :: MonadChance m => Dex -> Actor -> Effect -> Eng m ()
applyEffect dx a e = case e of
  EBoost bs -> void (boostMon a bs)
  EStatus st -> void (trySetStatus dx a st)
  -- addVolatile('flinch'): needs HP (no affectsFainted), fails if it is already there; no immunity, no
  -- TryAddVolatile handler, duration 1
  EFlinch -> void (addVolatile dx a CFlinch)
  EHeal _ _ -> pure () -- not executable as a secondary

-- | One @spreadMoveHit@ of a damaging move: crit roll, random factor, damage, drain. Returns the damage
-- actually dealt (@move.totalDamage@ accumulates it).
{-# INLINABLE hitMove #-}
hitMove :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m Int
hitMove fmt dx user target mv = do
  b <- get
  let um = monOf b user
      tm = monOf b target
  -- getDamage: crit roll, then modifyDamage's random(16)
  crit <- case critDenominator (mvCritRatio mv) of
    Nothing -> pure False
    Just d -> randomChance 1 d
  -- getDamage: `basePower = this.battle.clampIntRange(basePower, 1)`, then (after the crit roll)
  -- runEvent('BasePower', source, target, move, basePower, true) and the same clamp again; a handler only ever raises
  -- the power here, so the `!basePower` early return (damage 0, no further draw) is unreachable
  -- LATENT: the move's own BasePower callback (onEffect) is not run; moves with handlers are rejected.
  power <- (\r -> case r of RInt p -> max 1 p; _ -> max 1 (mvPower mv)) <$> runEvent dx EvBasePower user (Just target) (Just mv) (RInt (max 1 (mvPower mv)))
  let st = monStats
      (atkStat, defStat, atkStage0, defStage0)
        | mvPhysical mv = (stAtk (st um), stDef (st tm), bsAtk (monBoosts um), bsDef (monBoosts tm))
        | otherwise = (stSpa (st um), stSpd (st tm), bsSpa (monBoosts um), bsSpd (monBoosts tm))
      -- getDamage: a critical hit sets ignoreNegativeOffensive / ignorePositiveDefensive, which zero the
      -- attacker's negative offensive stage and the defender's positive defensive stage
      atkStage = if crit && atkStage0 < 0 then 0 else atkStage0
      defStage = if crit && defStage0 > 0 then 0 else defStage0
      attack0 = boostedStat atkStat atkStage
      defense0 = boostedStat defStat defStage
      (atkEv, defEv) = if mvPhysical mv then (EvModifyAtk, EvModifyDef) else (EvModifySpA, EvModifySpD)
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
  let pre = modifyDamage fmt dx um tm mv crit r weathered
  dmg <- finalDamage fmt . fromRelay pre <$> runEventCrit dx crit EvModifyDamage user (Just target) (Just mv) (RInt pre)
  let -- spreadDamage: clamp to at least 1 unless 0, then Pokemon.damage; `dealt` is the damage returned
      requested = if dmg == 0 then 0 else max 1 dmg
      dealt = min requested (monHp tm)
  modify' (applyDamage target requested)
  -- spreadDamage, effect.drain (battle.ts ~2187): heal the user round(dealt * n / d); battle.heal turns a
  -- positive amount below 1 into 1 and does nothing for 0, at full HP or when the user is down
  case mrDrain (mvRule mv) of
    Just (n, d) | dealt > 0 -> modify' (healMon user (roundDiv (dealt * n) d))
    _ -> pure ()
  pure dealt

-- | The integer an event returned, or the one it started with.
fromRelay :: Int -> Relay -> Int
fromRelay dflt r = case r of
  RInt v -> v
  _ -> dflt

-- | @Math.round(n / d)@ for non-negative @n@ and positive @d@ (exact: ties round up, as in JavaScript).
roundDiv :: Int -> Int -> Int
roundDiv n d = (2 * n + d) `quot` (2 * d)
