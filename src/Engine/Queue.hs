{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
-- Unfoldings are exposed so that "Engine.Turn" can specialise every engine function to the concrete chance monads
-- ('Engine.Chance.Sample', 'Engine.Chance.Replay'); without them each call here passes a Monad dictionary.
-- | The action queue (Showdown sim/battle-queue.ts and the sorting parts of battle.ts).
--
-- Actions carry Showdown's sort key (order, priority, speed, subOrder, effectOrder);
-- 'sortQueue' is @queue.sort()@ and 'insertChoice' is @queue.insertChoice()@.
module Engine.Queue
  ( orderOf
  , comparePriority
  , sortQueue
  , insertChoice
  , actionSpeedOf
  , resolveAction
  , cancelAction
  ) where

import Control.Monad.Trans.State.Strict (get, modify')
import qualified Data.Vector.Strict as SV

import Engine.Chance (MonadChance (..), speedSortWith)
import Engine.Dex
import Engine.Dispatch (Eng, getActionSpeed, movePriorityEvent)
import Engine.Event (SortKey (..), compareKey, priorityScale, speedScale)
import Engine.State

-- | Showdown's @orders@ table (battle-queue.ts @resolveAction@).
orderOf :: ActionKind -> Int
orderOf k = case k of
  KTeam -> 1
  KStart -> 2
  KInstaSwitch -> 3
  KBeforeTurn -> 4
  KRunSwitch -> 101
  KSwitch -> 103
  KTerastallize -> 106
  -- battle-queue.ts 189: priorityChargeMove: 107
  KPriorityChargeMove -> 107
  KMove -> 200
  KResidual -> 300

-- | @battle.comparePriority@ of two actions: order ascending, priority descending, speed descending (subOrder and
-- effectOrder are always 0). 'LT' means the first goes first.
comparePriority :: Action -> Action -> Ordering
comparePriority a b = compareKey (key a) (key b)
  where
    key x = SortKey (acOrder x) (acPriority x) (acSpeed x) 0 0

-- | @queue.sort()@ / @battle.speedSort(queue.list)@.
{-# INLINABLE sortQueue #-}
sortQueue :: MonadChance m => SV.Vector Action -> m (SV.Vector Action)
sortQueue q = SV.fromList <$> speedSortWith comparePriority (SV.toList q)

-- | @battle.getActionSpeed(action)@: refresh priority (moves) and speed (anything with a Pokemon). Priority is
-- scaled by 'priorityScale' and speed by 'speedScale'.
actionSpeedOf :: MonadChance m => Dex -> Action -> Eng m Action
actionSpeedOf dx ac = do
  b <- get
  withPriority <- case (acKind ac, acActor ac) of
    (KMove, Just a@(Actor p _)) -> case moveInSlot dx (monOf b a) (acArg ac) of
      -- the priority the action has now (battle.ts 2690-2705): the move's ModifyPriority handler reads the field as it is
      Just mv -> (\pr -> ac {acPriority = pr * priorityScale}) <$> movePriorityEvent dx a (Just (activeActor (other p) b)) mv
      Nothing -> pure ac {acPriority = 0}
    _ -> pure ac
  case acActor ac of
    Nothing -> pure withPriority {acSpeed = 1 * speedScale}
    Just a -> do
      s <- getActionSpeed dx a
      pure withPriority {acSpeed = s * speedScale}

-- | The move in a slot; Struggle is slot -1.
-- LATENT: Showdown's getActionSpeed also adds fractionalPriority (battle.ts 2701); the scaled keys have room for the
-- fraction, but nothing sets one yet.
moveInSlot :: Dex -> Mon -> Int -> Maybe MoveData
moveInSlot dx m slot
  | slot < 0 = Just (dxStruggle dx)
  | otherwise = (\ms -> moveData dx (msMove ms)) <$> monMoves m SV.!? slot

-- | @queue.resolveAction@ for a freshly built action: fill in order, priority and speed,
-- and clear the switch flag of a switching Pokemon.
{-# INLINABLE resolveAction #-}
resolveAction :: MonadChance m => Dex -> Action -> Eng m Action
resolveAction dx ac0 = do
  let ac = ac0 {acOrder = orderOf (acKind ac0)}
  case (acKind ac, acActor ac) of
    (k, Just a) | k == KSwitch || k == KInstaSwitch -> modify' (modifyMon a (\m -> m {monSwitchFlag = NoSwitch}))
    _ -> pure ()
  actionSpeedOf dx ac

-- | @queue.insertChoice@: place a new action where it would have sorted among the existing
-- ones without re-sorting them; a tie draws the position uniformly.
{-# INLINABLE insertChoice #-}
insertChoice :: MonadChance m => Action -> SV.Vector Action -> m (SV.Vector Action)
insertChoice new queue = do
  let list = SV.toList queue
      compared = [(i, comparePriority new cur) | (i, cur) <- zip [0 ..] list]
      firstIndex = case [i | (i, c) <- compared, c /= GT] of
        (i : _) -> Just i
        [] -> Nothing
      -- the loop stops at the first strictly-earlier element
      lastIndex = case [i | (i, c) <- compared, c == LT] of
        (i : _) -> Just i
        [] -> Nothing
  case firstIndex of
    Nothing -> pure (SV.snoc queue new)
    Just f -> do
      let l = maybe (length list) id lastIndex
      idx <- if f == l then pure f else roll f (l + 1)
      let (before, after) = splitAt idx list
      pure (SV.fromList (before ++ new : after))

-- | @queue.cancelAction(pokemon)@: drop every queued action of a Pokemon except @runSwitch@.
cancelAction :: Actor -> SV.Vector Action -> SV.Vector Action
cancelAction a = SV.filter (\ac -> acActor ac /= Just a || acKind ac == KRunSwitch)
