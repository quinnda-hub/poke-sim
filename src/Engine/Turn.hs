{-# LANGUAGE OverloadedStrings #-}
-- | The turn loop, structured after Showdown's sim/battle.ts and sim/battle-actions.ts.
--
-- Showdown's control flow is reproduced function by function: @start@ ('newBattle'),
-- @commitChoices@, @turnLoop@, @runAction@ (with its post-action tail), @runMove@ /
-- @useMove@ / @trySpreadMoveHit@, @switchIn@ / @runSwitch@, @resolveFaints@,
-- @checkFainted@, @checkWin@ and @endTurn@. Event points (@eachEvent@, @fieldEvent@) perform
-- Showdown's speed sorts even though no handler is attached to them in slice 0, because
-- those sorts consume random draws on speed ties.
module Engine.Turn
  ( newBattle
  , step
  , stagedAccuracy
  , roundDiv
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, gets, modify', put)
import Data.Maybe (isNothing)
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine.Chance
import Engine.Choice
import Engine.Damage
import Engine.Dex
import Engine.Format (Format, trunc)
import Engine.Queue
import Engine.Rule
import Engine.State
import Engine.Stats (boostedStat)
import Engine.Team

-- | The engine monad: battle state over the chance monad.
type Eng m = StateT Battle m

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
        , acSpeed = 1
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
  updateSpeedAll
  fmt <- gets bFormat
  old <- gets bQueue
  resolved <- mapM (resolveAction fmt dx) choices
  sorted <- sortQueue (SV.fromList resolved)
  modify' (\b -> b {bQueue = sorted SV.++ old, bRequest = ReqNone})
  turnLoop dx
{-# INLINABLE commitChoices #-}

-- | @battle.turnLoop@ (battle.ts): run queued actions until a request is made, the battle ends, or the
-- queue empties (then @endTurn@).
turnLoop :: MonadChance m => Dex -> Eng m ()
turnLoop dx = do
  fmt <- gets bFormat
  mid <- gets bMidTurn
  unless mid $ do
    -- queue.insertChoice({choice: 'beforeTurn'}); queue.addChoice({choice: 'residual'})
    before <- resolveAction fmt dx (plain KBeforeTurn)
    residual <- resolveAction fmt dx (plain KResidual)
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
          endTurn
          modify' (\b -> b {bMidTurn = False, bQueue = SV.empty})
        Just (a, rest) -> do
          modify' (\b -> b {bQueue = rest})
          runAction dx a
          stop <- gets (\b -> bRequest b /= ReqNone || bResult b /= Ongoing)
          unless stop loop
{-# INLINABLE turnLoop #-}

-- | @battle.endTurn@ (battle.ts): advance the turn counter, apply the 1000-turn limit and request moves.
{-# INLINABLE endTurn #-}
endTurn :: Monad m => Eng m ()
endTurn = do
  modify' (\b -> b {bTurn = bTurn b + 1})
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
      startAction fmt dx
      afterAction fmt dx ac
    KMove -> do
      b <- get
      case acActor ac of
        Just a | monActive (monOf b a), not (monFainted (monOf b a)) -> do
          runMove fmt dx a (acArg ac)
          afterAction fmt dx ac
        _ -> pure () -- the Pokemon left the field or fainted: no tail (battle.ts 'move' case returns false)
    KInstaSwitch -> switchAction fmt dx ac >> afterAction fmt dx ac
    KSwitch -> switchAction fmt dx ac >> afterAction fmt dx ac
    KRunSwitch -> do
      forM_ (acActor ac) runSwitch
      afterAction fmt dx ac
    KBeforeTurn -> do
      eachEvent -- eachEvent('BeforeTurn')
      afterAction fmt dx ac
    KResidual -> do
      updateSpeedAll
      fieldEvent -- fieldEvent('Residual')
      afterAction fmt dx ac
{-# INLINABLE runAction #-}

-- | @case 'team'@: rebuild the side's Pokemon order one action at a time. No tail.
teamAction :: Monad m => Action -> Eng m ()
teamAction ac = case acActor ac of
  Just (Actor p o) -> modify' $ modifySide p $ \s ->
    s {sSlots = if acArg ac == 0 then U.singleton o else U.snoc (sSlots s) o}
  Nothing -> pure ()

-- | @case 'start'@: set @pokemonLeft@ and switch in each side's lead.
{-# INLINABLE startAction #-}
startAction :: MonadChance m => Format -> Dex -> Eng m ()
startAction fmt dx = do
  forM_ [P1, P2] $ \p ->
    modify' (modifySide p (\s -> if sLeft s > 0 then s {sLeft = SV.length (sMons s)} else s))
  forM_ [P1, P2] $ \p -> do
    s <- gets (sideOf p)
    switchIn fmt dx p (activeOrig s)
  modify' (\b -> b {bMidTurn = True})

-- | @case 'switch'@ / @'instaswitch'@: switch the target in at the actor's position.
{-# INLINABLE switchAction #-}
switchAction :: MonadChance m => Format -> Dex -> Action -> Eng m ()
switchAction fmt dx ac = case acActor ac of
  Just (Actor p _) -> switchIn fmt dx p (acArg ac)
  Nothing -> pure ()

-- | The part of @runAction@ after the action's own work.
{-# INLINABLE afterAction #-}
afterAction :: MonadChance m => Format -> Dex -> Action -> Eng m ()
afterAction fmt dx ac = do
  -- fainting: resolveFaints({ checkWinIfNoFaints: true, checkWin: true })
  resolveFaints True True
  ended <- gets ((/= Ongoing) . bResult)
  unless ended $ do
    next <- gets (peekQueue . bQueue)
    -- switching (fainted pokemon): only when the queue is exhausted
    when (isNothing next) checkFainted
    -- gen >= 5: eachEvent('Update') unless the next action is an instaswitch
    when (acKind ac /= KStart && fmap acKind next /= Just KInstaSwitch) eachEvent
    flagged <- switchRequests
    if or flagged
      then modify' (\b -> b {bRequest = ReqSwitch})
      else do
        -- gen >= 8: speeds are refreshed and the queue re-sorted before the next move
        next' <- gets (peekQueue . bQueue)
        when (fmap acKind next' == Just KMove) $ do
          updateSpeedAll
          b <- get
          let refreshed = SV.map (\a -> if isNothing (acActor a) then a else actionSpeedOf fmt dx b a) (bQueue b)
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
-- Event points

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

-- | @pokemon.updateSpeed()@ for every active Pokemon (battle.ts @updateSpeed@).
{-# INLINABLE updateSpeedAll #-}
updateSpeedAll :: Monad m => Eng m ()
updateSpeedAll = do
  fmt <- gets bFormat
  b <- get
  forM_ (getAllActive False b) $ \a ->
    modify' (modifyMon a (\m -> m {monSpeed = monSpeedNow fmt m}))

-- | @battle.eachEvent@ (battle.ts), used for @Update@ and @BeforeTurn@: speed-sort the active Pokemon
-- by their cached @speed@ (ties are shuffled) and dispatch to each. No handlers exist in slice 0,
-- but the sort still draws on ties.
{-# INLINABLE eachEvent #-}
eachEvent :: MonadChance m => Eng m ()
eachEvent = do
  b <- get
  let speedOf a = monSpeed (monOf b a)
  void (speedSortWith (\x y -> compare (speedOf y) (speedOf x)) (getAllActive False b))

-- | The residual handlers of one Pokemon (battle.ts @findPokemonEventHandlers@ with @getKey = 'duration'@):
-- its status when it has an @onResidual@ (conditions.ts), and each volatile with a duration. Their sort key
-- is @onResidualOrder@: brn 10, psn and tox 9; flinch has a duration but no @onResidual@, so no order (last).
data Residual = ResBrn | ResPsn | ResTox | ResFlinch
  deriving (Eq, Show)

residualOrder :: Residual -> Int
residualOrder r = case r of
  ResBrn -> 10
  ResPsn -> 9
  ResTox -> 9
  ResFlinch -> maxBound -- `order || 4294967296`: no order sorts after every order

-- | @battle.fieldEvent('Residual')@ (battle.ts): the handlers of the field, sides and active Pokemon are
-- collected (the field and sides have none yet), sorted by @comparePriority@ (order ascending, speed
-- descending; the speed ties it breaks draw) and run one at a time, each followed by @resolveFaints()@.
--
-- Handlers are collected side by side, p1 first, status before volatiles, so a speed tie is shuffled from the
-- same starting order as Showdown's. @effectOrder@ never enters this sort: @resolvePriority@ copies it into a
-- handler only for @SwitchIn@ and @RedirectTarget@ callbacks (battle.ts ~1005), so two residual handlers that tie
-- on order, priority, speed and subOrder are shuffled, whatever order their effects were created in.
-- A handler of a fainted Pokemon is skipped; a volatile with a duration counts it down first and ends at 0
-- (flinch, duration 1, is removed here), skipping the faint resolution of that handler. When a faint ends the
-- battle the remaining handlers do not run.
-- LATENT: no field, side or ability handlers exist yet (weather, terrain, screens, hazards, Leftovers, ...);
-- add them to the collection here, with their order, priority and subOrder.
{-# INLINABLE fieldEvent #-}
fieldEvent :: MonadChance m => Eng m ()
fieldEvent = do
  b <- get
  let handlers =
        [ (a, k)
        | a <- getAllActive True b
        , let m = monOf b a
        , k <- [ResBrn | monStatus m == StatusBrn] ++ [ResPsn | monStatus m == StatusPsn]
                 ++ [ResTox | monStatus m == StatusTox] ++ [ResFlinch | monFlinch m]
        ]
      speedOf a = monSpeed (monOf b a)
      cmp (a, k) (a', k') = compare (residualOrder k) (residualOrder k') <> compare (speedOf a') (speedOf a)
  sorted <- speedSortWith cmp handlers
  run sorted
  where
    run [] = pure ()
    run ((a, k) : rest) = do
      m <- gets (`monOf` a)
      if monFainted m
        then run rest
        else case k of
          ResFlinch -> do
            -- handler.state.duration-- reaches 0: removeVolatile('flinch'), then `continue`
            modify' (modifyMon a (\x -> x {monFlinch = False}))
            run rest
          _ -> do
            -- singleEvent('Residual', status): skipped if the status changed; the damage is
            -- battle.damage(...), whose amounts are clampIntRange(baseMaxhp / n, 1) (floor, at least 1)
            let hp16 = max 1 (monMaxHp m `quot` 16)
            case k of
              ResBrn | monStatus m == StatusBrn -> modify' (applyDamage a hp16)
              ResPsn | monStatus m == StatusPsn -> modify' (applyDamage a (max 1 (monMaxHp m `quot` 8)))
              ResTox | monStatus m == StatusTox -> do
                -- `if (stage < 15) stage++`; damage(clampIntRange(baseMaxhp / 16, 1) * stage)
                let stage = if monStatusStage m < 15 then monStatusStage m + 1 else monStatusStage m
                modify' (modifyMon a (\x -> x {monStatusStage = stage}))
                modify' (applyDamage a (hp16 * stage))
              _ -> pure ()
            resolveFaints False True -- this.resolveFaints() (checkWin defaults to true)
            ended <- gets ((/= Ongoing) . bResult)
            unless ended (run rest)

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

-- | @battle.resolveFaints@ (battle.ts), gen 5+ path without events: faint everything in the faint
-- queue, then optionally check for a winner. Arguments: @checkWinIfNoFaints@, @checkWin@.
{-# INLINABLE resolveFaints #-}
resolveFaints :: Monad m => Bool -> Bool -> Eng m ()
resolveFaints checkWinIfNoFaints checkWin' = do
  b <- get
  when (bResult b == Ongoing) $
    case SV.toList (bFaintQueue b) of
      [] -> when checkWinIfNoFaints (void (checkWin Nothing))
      q -> do
        -- LATENT: faints queued while this loop runs (battle.ts ~2626, which forces checkWin) need a Faint
        -- event handler (Destiny Bond, ...); nothing in slice 0 queues a faint here.
        forM_ q $ \a@(Actor p _) -> do
          m <- gets (\bb -> monOf bb a)
          unless (monFainted m) $ do
            modify' (modifySide p (\s -> if sLeft s > 0 then s {sLeft = sLeft s - 1} else s))
            -- pokemon.clearVolatile(false) resets the stat stages, the volatiles (flinch) and the cached speed (setSpecies)
            modify' (modifyMon a (\mm -> mm
              { monSpeed = stSpe (monStats mm)
              , monBoosts = noBoosts
              , monFlinch = False
              , monFainted = True
              , monActive = False
              }))
        modify' (\bb -> bb {bFaintQueue = SV.empty})
        case reverse q of
          (lastFaint : _) | checkWin' -> void (checkWin (Just lastFaint))
          _ -> pure ()

-- | @battle.checkWin@: end the battle if a side has no Pokemon left. Returns whether it ended.
{-# INLINABLE checkWin #-}
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
{-# INLINABLE win #-}
win :: Monad m => Maybe Player -> Eng m ()
win w = do
  b <- get
  when (bResult b == Ongoing) $
    put b {bResult = maybe Tied Won w, bRequest = ReqNone}

-- ---------------------------------------------------------------------------
-- Switching

-- | @actions.switchIn@ (battle-actions.ts) for singles position 0, without events or messages.
--
-- LATENT: a drag (Roar, Whirlwind, Dragon Tail) skips @runPursuitActivation@ and runs @runSwitch@ at once
-- (battle-actions.ts ~97, ~208), and @isStarted@ (battle-actions.ts ~241) is not modelled; both matter
-- once forced switches and switch-in effects exist.
{-# INLINABLE switchIn #-}
switchIn :: MonadChance m => Format -> Dex -> Player -> Int -> Eng m ()
switchIn fmt dx p target = do
  b0 <- get
  let s0 = sideOf p b0
      tgt = Actor p target
  unless (monActive (monOf b0 tgt)) $ do
    let old = if sHasActive s0 then Just (activeActor p b0) else Nothing
    case old of
      Just o | monHp (monOf b0 o) > 0 -> do
        -- runPursuitActivation(oldActive): eachEvent('Update') (the BeforeSwitchOut event has no handlers)
        eachEvent
        -- queue.cancelAction(oldActive); oldActive.clearVolatile() (resets the stat stages, the volatiles and the cached speed; the status, and toxic's stage, stay)
        modify' (\b -> modifyMon o (\m -> m {monSpeed = stSpe (monStats m), monBoosts = noBoosts, monFlinch = False}) b {bQueue = cancelAction o (bQueue b)})
      _ -> pure ()
    case old of
      Just o -> do
        -- oldActive.isActive = false; `if (oldActive.fainted) oldActive.status = ''`
        modify' (modifyMon o (\m -> m {monActive = False, monStatus = if monFainted m then StatusNone else monStatus m}))
        -- the old active and the incoming Pokemon trade slots
        modify' (modifySide p (\s -> s {sSlots = swapSlot0 target (sSlots s)}))
      Nothing -> pure ()
    modify' (modifyMon tgt (\m -> m {monActive = True}) . modifySide p (\s -> s {sHasActive = True}))
    -- queue.insertChoice({ choice: 'runSwitch', pokemon }): updateSpeed first, then a tie-aware insert
    modify' (modifyMon tgt (\m -> m {monSpeed = monSpeedNow fmt m}))
    new <- resolveAction fmt dx Action
      { acKind = KRunSwitch, acOrder = 0, acPriority = 0, acSpeed = 0, acActor = Just tgt, acArg = 0 }
    q <- gets bQueue
    q' <- insertChoice new q
    modify' (\b -> b {bQueue = q'})
  where
    swapSlot0 o slots = case U.elemIndex o slots of
      Just i | i /= 0 -> slots U.// [(0, slots U.! i), (i, slots U.! 0)]
      _ -> slots

-- | @actions.runSwitch@ (battle-actions.ts): gather consecutive @runSwitch@ actions, speed-sort every
-- Pokemon on the field (including fainted ones) and run @fieldEvent('SwitchIn')@ for the switchers.
--
-- The only @onSwitchIn@ handler is toxic's (conditions.ts: @effectState.stage = 0@). Its order never matters:
-- @resolvePriority@ gives each SwitchIn handler the speed @pokemon.speed - speedOrder.indexOf(position) / 2@, so no
-- two Pokemon tie and the handlers' sort draws nothing, and the handlers touch only their own Pokemon. This
-- is also why the sorted list of the pre-sort below is discarded.
-- LATENT: abilities and items (Intimidate, Sticky Web, hazards, ...) add SwitchIn handlers whose order does
-- matter (@battle.speedOrder@, battle-actions.ts ~236, reused for event ordering at battle.ts ~1019); store the
-- sorted order then. @effectOrder@ (creation order of effect states) ties only same-Pokemon handlers.
{-# INLINABLE runSwitch #-}
runSwitch :: MonadChance m => Actor -> Eng m ()
runSwitch first = do
  -- while (queue.peek()?.choice === 'runSwitch') switchersIn.push(queue.shift().pokemon)
  q <- gets bQueue
  let following = SV.takeWhile ((== KRunSwitch) . acKind) q
      switchersIn = first : [a | Just a <- map acActor (SV.toList following)]
  modify' (\b -> b {bQueue = SV.drop (SV.length following) (bQueue b)})
  b <- get
  let speedOf a = monSpeed (monOf b a)
  -- battle.speedSort(allActive): comparePriority on Pokemon objects reduces to cached speed
  void (speedSortWith (\x y -> compare (speedOf y) (speedOf x)) (getAllActive True b))
  -- fieldEvent('SwitchIn', switchersIn): each handler is followed by resolveFaints()
  forM_ switchersIn $ \a -> do
    m <- gets (`monOf` a)
    when (monActive m && not (monFainted m) && monStatus m == StatusTox) $ do
      modify' (modifyMon a (\x -> x {monStatusStage = 0}))
      resolveFaints False True

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
      proceed <- beforeMove user (mvRule mv)
      when proceed $ do
        void deductPP
        -- useMove's ModifyMove event: a frozen user of a defrost move thaws (conditions.ts frz.onModifyMove)
        let rule = mvRule mv
        um <- gets (`monOf` user)
        when (mrDefrost rule && monStatus um == StatusFrz) (modify' (modifyMon user clearStatus))
        useMove fmt dx user target mv
        -- resolveFaints(); checkWin()
        resolveFaints False True
        void (checkWin Nothing)

-- | @runEvent('BeforeMove', pokemon, target, move)@ (battle-actions.ts runMove): the handlers are the user's
-- own: frz (@onBeforeMovePriority@ 10), flinch (8) and par (1). Sorted by priority they never tie (the
-- statuses are exclusive), so the sort draws nothing; the first to return false stops the event and the move.
-- No other handler exists (@onFoeBeforeMove@, @onAnyBeforeMove@, @onSourceBeforeMove@, side, field).
-- Returns whether the move goes ahead.
-- LATENT: sleep, confusion, taunt, disable, Protect-like volatiles, items and abilities add BeforeMove handlers.
{-# INLINABLE beforeMove #-}
beforeMove :: MonadChance m => Actor -> MoveRule -> Eng m Bool
beforeMove user rule = do
  m <- gets (`monOf` user)
  -- frz.onBeforeMove: a defrost move passes without a draw (it thaws in ModifyMove); otherwise
  -- randomChance(1, 5) cures the status and lets the move go on, and failing it is `cant`
  thawed <- case monStatus m of
    StatusFrz
      | mrDefrost rule -> pure True
      | otherwise -> do
          t <- randomChance 1 5
          when t (modify' (modifyMon user clearStatus))
          pure t
    _ -> pure True
  if not thawed then pure False
    else if monFlinch m then pure False -- flinch.onBeforeMove: `cant`, runEvent('Flinch') has no handlers
    else case monStatus m of
      StatusPar -> not <$> randomChance 1 4 -- par.onBeforeMove: full paralysis
      _ -> pure True

-- | @setStatus('')@ (@cureStatus@, @clearStatus@): the status goes, and with it its fresh @statusState@.
clearStatus :: Mon -> Mon
clearStatus m = m {monStatus = StatusNone, monStatusStage = -1}

-- | The hit steps of @trySpreadMoveHit@ (battle-actions.ts): target check, type immunity, accuracy, then the
-- hit loop. Spread targets and @randomNormal@ reach the one foe in singles ('TargetFoe'); a move on the
-- user ('TargetSelf') skips immunity and accuracy (@hitStepAccuracy@: a self-targeted status move).
{-# INLINABLE useMove #-}
useMove :: MonadChance m => Format -> Dex -> Actor -> Actor -> MoveData -> Eng m ()
useMove fmt dx user target mv = case mrTarget rule of
  TargetSelf -> statusMove dx user user mv
  TargetFoe -> do
    b <- get
    let tm = monOf b target
        um = monOf b user
    -- getMoveTargets (battle-actions.ts ~842-859, pokemon.ts getMoveTargets): a fainted target leaves no
    -- targets, so the move fails before any draw. getRandomTarget in singles returns foe.active[0] even when
    -- it has fainted; the spread targets (allAdjacentFoes, allAdjacent) use side.foes(), which skips it.
    unless (monFainted tm) $
      -- hitStepTypeImmunity (before accuracy: an immune target consumes no draws); a move with
      -- ignoreImmunity (every status move but a few) skips it
      unless (not (mrIgnoreImmunity rule) && isImmune dx (mvType mv) tm) $ do
        -- hitStepAccuracy
        hits <- if mvAccuracy mv == neverMisses || (isToxic && hasTypeNamed dx "Poison" um) then pure True
          else randomChance (stagedAccuracy fmt (mvAccuracy mv) (monBoosts um) (monBoosts tm)) 100
        when hits $
          if mrDamaging rule then hitLoop fmt dx user target mv else statusMove dx user target mv
  where
    rule = mvRule mv
    -- ID-SPECIAL: battle-actions.ts hitStepAccuracy ~797 (and hitStepInvulnerabilityEvent ~693): in gen >= 8 Toxic
    -- used by a Poison-type skips the accuracy check (`accuracy = true`), so it draws nothing
    isToxic = mrOnHit rule == [EStatus Tox] && mvId mv == "toxic"

-- | @hitStepAccuracy@'s stage step (battle-actions.ts): the user's accuracy stage minus the target's evasion
-- stage, each clamped to [-6, 6], scales the accuracy by @(3 + s) / 3@ or @3 / (3 - s)@, truncated.
stagedAccuracy :: Format -> Int -> Boosts -> Boosts -> Int
stagedAccuracy fmt acc user tgt
  | stage > 0 = trunc fmt (acc * (3 + stage) `quot` 3)
  | stage < 0 = trunc fmt (acc * 3 `quot` (3 - stage))
  | otherwise = acc
  where
    stage = clampStage (clampStage (bsAccuracy user) - bsEvasion tgt)

clampStage :: Int -> Int
clampStage = max (-6) . min 6

-- | @battle.boost@ (battle.ts) of one target: each stage moves by its change clamped to [-6, 6]
-- (@getCappedBoost@). Returns whether any stage changed. It fails for a target with no HP or off the field,
-- and when the foe side has no Pokemon left. The order of the stats is unobservable (independent stages;
-- the boost events have no handlers).
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
    eachEvent -- eachEvent('Update') after the hit
    -- resolveFaints({ checkWin: !pokemon.hp }); the user is alive
    userDown <- gets (\bb -> monHp (monOf bb user) <= 0)
    resolveFaints False userDown
    eachEvent -- second eachEvent('Update')

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
  resolveFaints False userDown
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
  eachEvent -- second eachEvent('Update')
  -- afterMoveSecondaryEvent: runEvent('AfterMoveSecondary'); frz.onAfterMoveSecondary cures a frozen target of
  -- a thawsTarget move (conditions.ts). cureStatus does nothing for a target with no HP.
  when (mrThawsTarget rule) (thawTarget target)
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
              eachEvent -- eachEvent('Update') after each hit
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
  -- a damaging Fire move (conditions.ts; `move.id !== 'polarflare'`, which has handlers and is not supported)
  tgtM <- gets (`monOf` target)
  when (monStatus tgtM == StatusFrz && typeIsNamed dx (mvType mv) "Fire") (thawTarget target)

-- | One effect of a secondary on a Pokemon (@moveHit@ with @isSecondary@ -> @runMoveEffects@).
applyEffect :: Monad m => Dex -> Actor -> Effect -> Eng m ()
applyEffect dx a e = case e of
  EBoost bs -> void (boostMon a bs)
  EStatus st -> void (trySetStatus dx a st)
  EFlinch -> modify' (modifyMon a (\x -> if monHp x > 0 && not (monFlinch x) then x {monFlinch = True} else x))
    -- addVolatile('flinch'): needs HP (no affectsFainted), fails if it is already there; no immunity, no
    -- TryAddVolatile handler, duration 1
  EHeal _ _ -> pure () -- not executable as a secondary

-- | @pokemon.cureStatus()@: needs HP and a status.
thawTarget :: Monad m => Actor -> Eng m ()
thawTarget a = modify' (modifyMon a (\x -> if monHp x > 0 && monStatus x == StatusFrz then clearStatus x else x))

-- | @pokemon.trySetStatus(status)@ -> @setStatus(this.status || status)@ (pokemon.ts) of a Pokemon, with no
-- ability, item or field effect: fails with no HP, off the field, with any status already (including the same
-- one) or when the type is immune; otherwise sets the status and its fresh @statusState@ (toxic's @onStart@
-- sets the stage to 0). @runStatusImmunity@, @SetStatus@ and @AfterSetStatus@ draw nothing.
trySetStatus :: Monad m => Dex -> Actor -> StatusId -> Eng m Bool
trySetStatus dx a st = do
  b <- get
  let m = monOf b a
      has = monStatus m `elem` [StatusBrn, StatusPar, StatusPsn, StatusTox, StatusFrz]
  if monHp m <= 0 || not (monActive m) || has || statusImmune dx st m
    then pure False
    else do
      modify' (modifyMon a (\x -> x {monStatus = monStatusOf st, monStatusStage = if st == Tox then 0 else -1}))
      pure True
  where
    monStatusOf s = case s of
      Brn -> StatusBrn
      Par -> StatusPar
      Psn -> StatusPsn
      Tox -> StatusTox
      Frz -> StatusFrz

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
  -- getDamage: `basePower = this.battle.clampIntRange(basePower, 1)` (also after the BasePower event)
  let power = max 1 (mvPower mv)
      st = monStats
      (atkStat, defStat, atkStage0, defStage0)
        | mvPhysical mv = (stAtk (st um), stDef (st tm), bsAtk (monBoosts um), bsDef (monBoosts tm))
        | otherwise = (stSpa (st um), stSpd (st tm), bsSpa (monBoosts um), bsSpd (monBoosts tm))
      -- getDamage: a critical hit sets ignoreNegativeOffensive / ignorePositiveDefensive, which zero the
      -- attacker's negative offensive stage and the defender's positive defensive stage
      atkStage = if crit && atkStage0 < 0 then 0 else atkStage0
      defStage = if crit && defStage0 > 0 then 0 else defStage0
      attack = boostedStat atkStat atkStage
      defense = boostedStat defStat defStage
      base = baseDamage fmt (monLevel um) power attack defense
  r <- roll 0 16
  let dmg = modifyDamage fmt dx um tm mv crit r base
      -- spreadDamage: clamp to at least 1 unless 0, then Pokemon.damage; `dealt` is the damage returned
      requested = if dmg == 0 then 0 else max 1 dmg
      dealt = min requested (monHp tm)
  modify' (applyDamage target requested)
  -- spreadDamage, effect.drain (battle.ts ~2187): heal the user round(dealt * n / d); battle.heal turns a
  -- positive amount below 1 into 1 and does nothing for 0, at full HP or when the user is down
  case mrDrain (mvRule mv) of
    Just (n, d) | dealt > 0 -> modify' (healMon user (roundDiv (dealt * n) d))
    _ -> pure ()
  pure dealt

-- | @Math.round(n / d)@ for non-negative @n@ and positive @d@ (exact: ties round up, as in JavaScript).
roundDiv :: Int -> Int -> Int
roundDiv n d = (2 * n + d) `quot` (2 * d)

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
