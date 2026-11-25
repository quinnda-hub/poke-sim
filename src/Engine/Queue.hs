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
  , monActionSpeed
  , monSpeedNow
  , resolveAction
  , cancelAction
  ) where

import Control.Monad.Trans.State.Strict (StateT, gets, modify')
import qualified Data.Vector.Strict as SV

import Engine.Chance (MonadChance (..), speedSortWith)
import Engine.Dex
import Engine.Format (Format)
import Engine.State
import Engine.Stats (actionSpeed)

-- | Showdown's @orders@ table (battle-queue.ts @resolveAction@).
orderOf :: ActionKind -> Int
orderOf k = case k of
  KTeam -> 1
  KStart -> 2
  KInstaSwitch -> 3
  KBeforeTurn -> 4
  KRunSwitch -> 101
  KSwitch -> 103
  KMove -> 200
  KResidual -> 300

-- | @battle.comparePriority@: order ascending, priority descending, speed descending
-- (subOrder and effectOrder are always 0 in slice 0). 'LT' means the first goes first.
comparePriority :: Action -> Action -> Ordering
comparePriority a b =
  compare (acOrder a) (acOrder b)
    <> compare (acPriority b) (acPriority a)
    <> compare (acSpeed b) (acSpeed a)

-- | @queue.sort()@ / @battle.speedSort(queue.list)@.
{-# INLINABLE sortQueue #-}
sortQueue :: MonadChance m => SV.Vector Action -> m (SV.Vector Action)
sortQueue q = SV.fromList <$> speedSortWith comparePriority (SV.toList q)

-- | @pokemon.getActionSpeed()@ of a Pokemon right now: its Spe stat with its current stage and paralysis.
monSpeedNow :: Format -> Mon -> Int
monSpeedNow fmt m = actionSpeed fmt (stSpe (monStats m)) (bsSpe (monBoosts m)) (monStatus m == StatusPar)
{-# INLINE monSpeedNow #-}

-- | @pokemon.getActionSpeed()@ for the Pokemon of an actor.
monActionSpeed :: Format -> Battle -> Actor -> Int
monActionSpeed fmt b a = monSpeedNow fmt (monOf b a)

-- | @battle.getActionSpeed(action)@: refresh priority (moves) and speed (anything with a Pokemon).
actionSpeedOf :: Format -> Dex -> Battle -> Action -> Action
actionSpeedOf fmt dx b ac =
  let withPriority = case (acKind ac, acActor ac) of
        (KMove, Just a) -> ac {acPriority = movePriority dx (monOf b a) (acArg ac)}
        _ -> ac
   in case acActor ac of
        Nothing -> withPriority {acSpeed = 1}
        Just a -> withPriority {acSpeed = monActionSpeed fmt b a}

-- | Base priority of the move in a slot; Struggle is slot -1.
-- LATENT: Showdown's getActionSpeed also runs ModifyPriority and adds fractionalPriority (battle.ts ~2701);
-- integer priority/speed keys are enough only while no ability, item or move changes either.
movePriority :: Dex -> Mon -> Int -> Int
movePriority dx m slot
  | slot < 0 = mvPriority (dxStruggle dx)
  | otherwise = case monMoves m SV.!? slot of
      Just ms -> mvPriority (moveData dx (msMove ms))
      Nothing -> 0

-- | @queue.resolveAction@ for a freshly built action: fill in order, priority and speed,
-- and clear the switch flag of a switching Pokemon.
{-# INLINABLE resolveAction #-}
resolveAction :: Monad m => Format -> Dex -> Action -> StateT Battle m Action
resolveAction fmt dx ac0 = do
  let ac = ac0 {acOrder = orderOf (acKind ac0)}
  case (acKind ac, acActor ac) of
    (k, Just a) | k == KSwitch || k == KInstaSwitch -> modify' (modifyMon a (\m -> m {monSwitchFlag = False}))
    _ -> pure ()
  gets (\b -> actionSpeedOf fmt dx b ac)

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
