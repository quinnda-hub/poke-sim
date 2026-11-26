-- | Stat calculation (Showdown @battle.spreadModify@ / @statModify@, battle.ts).
module Engine.Stats
  ( hpStat
  , otherStat
  , boostedStat
  , parSpe
  , finalSpeed
  , actionSpeed
  ) where

import Engine.Format (Format (..), TruncRule (..), trunc, truncBits)
import Engine.Modifier (finalModify)

-- | Max HP: @tr(tr(2B + IV + tr(EV/4) + 100) * L / 100 + 10)@.
hpStat :: Format -> Int -> Int -> Int -> Int -> Int
hpStat f base iv ev level =
  trunc f ((trunc f (2 * base + iv + ev `quot` 4 + 100) * level) `quot` 100 + 10)

-- | A non-HP stat; @nature@ is 1 for a boosting nature, -1 for a lowering one, 0 otherwise.
-- @tr(tr(2B + IV + tr(EV/4)) * L / 100 + 5)@, then the nature with 16-bit truncation.
otherStat :: Format -> Int -> Int -> Int -> Int -> Int -> Int
otherStat f base iv ev level nature =
  let raw = trunc f ((trunc f (2 * base + iv + ev `quot` 4) * level) `quot` 100 + 5)
   in case compare nature 0 of
        GT -> trunc f (truncBits f 16 (raw * 110) `quot` 100)
        LT -> trunc f (truncBits f 16 (raw * 90) `quot` 100)
        EQ -> raw

-- | @getStat@ / @calculateStat@'s stage step: @floor(stat * [1, 1.5, .. 4][stage])@ for a stage in [-6, 6]
-- (a negative stage divides). Exact in integers: @stat * (2 + s) / 2@ and @stat * 2 / (2 - s)@.
boostedStat :: Int -> Int -> Int
boostedStat stat stage
  | stage >= 0 = stat * (2 + min 6 stage) `quot` 2
  | otherwise = stat * 2 `quot` (2 + min 6 (negate stage))

-- | conditions.ts @par.onModifySpe@: @spe = finalModify(spe); floor(spe * 50 / 100)@ ("Paralysis occurs after all
-- other Speed modifiers, so evaluate all modifiers up to this point first"), with the event's accumulated modifier
-- (scaled by 4096). With none, @finalModify@ is the identity. Returns the speed and the reset modifier.
-- (The Quick Feet exception needs abilities, which the state does not hold yet.)
parSpe :: Format -> Int -> Int -> (Int, Int)
parSpe f modifier spe = let (v, m) = finalModify f spe modifier in (v * 50 `quot` 100, m)

-- | The end of @pokemon.getStat('spe')@ and @getActionSpeed()@ (pokemon.ts 655, 662-670), after the
-- @ModifySpe@ event: the stat is capped at 10000 unless the format overrides @trunc@, Trick Room turns it into
-- @10000 - speed@, then @tr(speed, 13)@.
-- The cap cannot be reached at levels <= 100 (the largest boosted Spe is under 3000); it is kept for fidelity.
actionSpeed :: Format -> Bool -> Int -> Int
actionSpeed f trickRoom modified = truncBits f 13 (if trickRoom then 10000 - capped else capped)
  where
    capped = case fmtTrunc f of
      TruncUnsigned32 -> min 10000 modified
      TruncPlain -> modified
{-# INLINE actionSpeed #-}

-- | 'actionSpeed' with no Trick Room.
finalSpeed :: Format -> Int -> Int
finalSpeed f = actionSpeed f False
{-# INLINE finalSpeed #-}
