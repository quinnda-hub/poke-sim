{-# LANGUAGE BangPatterns, UnboxedTuples #-}
-- | The single source of randomness for the engine.
--
-- Every random draw is a call to 'roll', mirroring Pokemon Showdown's
-- @PRNG.random(from, to)@ (sim/prng.ts). Everything else ('randomChance',
-- 'shuffleRange', 'speedSortWith') is built from it exactly as in prng.ts and
-- battle.ts, so the sequence of @(lo, hi)@ requests is identical to Showdown's.
--
-- Two interpreters are provided: 'Replay' (consumes a recorded draw list and
-- reports the first divergence) and 'Sample' (fast, seedable, no Showdown seed
-- compatibility).
module Engine.Chance
  ( MonadChance (..)
  , randomChance
  , shuffleRange
  , speedSortWith
    -- * Replay interpreter
  , Draw (..)
  , Divergence (..)
  , ReplayState (..)
  , Replay
  , runReplay
  , replayStateOf
    -- * Sampling interpreter
  , Sample
  , runSample
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.List (foldl')
import System.Random.SplitMix (SMGen, nextWord64)
import Data.Bits (shiftR)

-- | A monad that can draw uniform integers.
class Monad m => MonadChance m where
  -- | @roll lo hi@ is uniform in the half-open range @[lo, hi)@
  -- (Showdown: @prng.random(lo, hi)@; one-argument @random(n)@ is @roll 0 n@).
  roll :: Int -> Int -> m Int

instance MonadChance m => MonadChance (StateT s m) where
  roll lo hi = lift (roll lo hi)
  {-# INLINE roll #-}

-- | @randomChance n d@ is true with probability @n/d@ (prng.ts @randomChance@).
randomChance :: MonadChance m => Int -> Int -> m Bool
randomChance n d = (< n) <$> roll 0 d
{-# INLINE randomChance #-}

-- | Fisher-Yates shuffle of positions @[start, end)@ of a list (prng.ts
-- @shuffle@): for each @start .. end-2@, draw @j@ in @[start, end)@ and swap.
{-# INLINABLE shuffleRange #-}
shuffleRange :: MonadChance m => Int -> Int -> [a] -> m [a]
shuffleRange start end xs
  | start >= end - 1 = pure xs
  | otherwise = do
      j <- roll start end
      shuffleRange (start + 1) end (if j == start then xs else swapAt start j xs)

-- | Swap two positions of a list.
swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs
  | i == j = xs
  | otherwise = zipWith pick [0 ..] xs
  where
    xi = xs !! i
    xj = xs !! j
    pick k x
      | k == i = xj
      | k == j = xi
      | otherwise = x

-- | Showdown's @Battle.speedSort@ (battle.ts): a selection sort that gathers
-- each group of elements comparing equal and shuffles the group with
-- 'shuffleRange'. The placement uses swaps exactly like the original, because
-- the order in which later ties are encountered feeds the shuffles.
--
-- The comparator returns 'LT' when its first argument goes first.
{-# INLINABLE speedSortWith #-}
speedSortWith :: MonadChance m => (a -> a -> Ordering) -> [a] -> m [a]
speedSortWith cmp xs0 = case xs0 of
  [] -> pure xs0
  [_] -> pure xs0
  [a, b] -> case cmp a b of
    LT -> pure xs0
    GT -> pure [b, a]
    EQ -> do
      j <- roll 0 2
      pure (if j == 0 then xs0 else [b, a])
  _ -> go 0 xs0
  where
    n = length xs0
    go !sorted xs
      | sorted + 1 >= n = pure xs
      | otherwise = do
          let idxs = nextIndexes sorted xs
              k = length idxs
              placed = foldl' place xs (zip [0 ..] idxs)
              place acc (i, ix)
                | ix /= sorted + i = swapAt (sorted + i) ix acc
                | otherwise = acc
          xs' <- if k > 1 then shuffleRange sorted (sorted + k) placed else pure placed
          go (sorted + k) xs'
    nextIndexes sorted xs =
      let candidates = zip [sorted + 1 ..] (drop (sorted + 1) xs)
          start = xs !! sorted
          step (bestIx, bestV, acc) (i, v) = case cmp bestV v of
            LT -> (bestIx, bestV, acc)
            GT -> (i, v, [i])
            EQ -> (bestIx, bestV, acc ++ [i])
       in case foldl' step (sorted, start, [sorted]) candidates of
            (_, _, acc) -> acc

-- ---------------------------------------------------------------------------
-- Replay

-- | One recorded draw: requested range and result.
data Draw = Draw !Int !Int !Int
  deriving (Eq, Show)

-- | Why a replay failed.
data Divergence
  = -- | Draw index, recorded range, requested range.
    RangeMismatch !Int (Int, Int) (Int, Int)
  | -- | The recording ran out: draw index and the range requested.
    Exhausted !Int (Int, Int)
  deriving (Eq, Show)

-- | Remaining recorded draws and how many have been consumed.
data ReplayState = ReplayState
  { rsRemaining :: [Draw]
  , rsConsumed :: !Int
  }
  deriving (Show)

-- | Replays recorded draws, failing at the first divergence.
newtype Replay a = Replay (StateT ReplayState (Either Divergence) a)

instance Functor Replay where
  fmap f (Replay m) = Replay (fmap f m)

instance Applicative Replay where
  pure = Replay . pure
  Replay f <*> Replay a = Replay (f <*> a)

instance Monad Replay where
  Replay m >>= f = Replay (m >>= \a -> let Replay r = f a in r)

instance MonadChance Replay where
  roll lo hi = Replay (StateT go)
    where
      go (ReplayState [] i) = Left (Exhausted i (lo, hi))
      go (ReplayState (Draw dlo dhi r : rest) i)
        | dlo /= lo || dhi /= hi = Left (RangeMismatch i (dlo, dhi) (lo, hi))
        | otherwise = Right (r, ReplayState rest (i + 1))

-- | Run a replay computation from the given state.
runReplay :: ReplayState -> Replay a -> Either Divergence (a, ReplayState)
runReplay s (Replay m) = runStateT m s

-- | Initial replay state from a full draw list.
replayStateOf :: [Draw] -> ReplayState
replayStateOf ds = ReplayState ds 0

-- ---------------------------------------------------------------------------
-- Sampling

-- | Fast sampling interpreter over a splitmix generator.
newtype Sample a = Sample (SMGen -> (# a, SMGen #))

instance Functor Sample where
  fmap f (Sample g) = Sample (\s -> case g s of (# a, s' #) -> (# f a, s' #))
  {-# INLINE fmap #-}

instance Applicative Sample where
  pure a = Sample (\s -> (# a, s #))
  {-# INLINE pure #-}
  Sample f <*> Sample g = Sample (\s -> case f s of
    (# h, s' #) -> case g s' of (# a, s'' #) -> (# h a, s'' #))
  {-# INLINE (<*>) #-}

instance Monad Sample where
  Sample g >>= f = Sample (\s -> case g s of
    (# a, s' #) -> let Sample h = f a in h s')
  {-# INLINE (>>=) #-}

instance MonadChance Sample where
  roll lo hi = Sample (\s ->
    let (w, s') = nextWord64 s
        n = hi - lo
        -- multiply-shift range reduction; bias is below 2^-31 for the tiny ranges used here
        r = lo + fromIntegral (((w `shiftR` 32) * fromIntegral n) `shiftR` 32)
     in (# r, s' #))
  {-# INLINE roll #-}

-- | Run a sampling computation from a generator.
runSample :: SMGen -> Sample a -> (a, SMGen)
runSample g (Sample f) = case f g of (# a, g' #) -> (a, g')
