-- | Stat calculation (Showdown @battle.spreadModify@ / @statModify@, battle.ts).
module Engine.Stats
  ( hpStat
  , otherStat
  , boostedStat
  , actionSpeed
  ) where

import Engine.Format (Format (..), TruncRule (..), trunc, truncBits)

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

-- | @pokemon.getActionSpeed()@ (pokemon.ts) from the Spe stat, its stage and whether the Pokemon is
-- paralysed: @getStat('spe')@ (stage, then the @ModifySpe@ event) capped at 10000 unless the format overrides
-- @trunc@ (pokemon.ts ~658), then @tr(speed, 13)@.
-- Paralysis is conditions.ts @par.onModifySpe@: @floor(finalModify(spe) * 50 / 100)@, the last speed modifier
-- (priority -101); with no other modifier @finalModify@ is the identity.
-- The cap cannot be reached at levels <= 100 (the largest boosted Spe is under 3000); it is kept for fidelity.
-- LATENT: items, abilities and Tailwind also run @ModifySpe@, and Trick Room inverts the result; none exists yet.
actionSpeed :: Format -> Int -> Int -> Bool -> Int
actionSpeed f speed stage paralysed = truncBits f 13 (case fmtTrunc f of
  TruncUnsigned32 -> min 10000 modified
  TruncPlain -> modified)
  where
    boosted = boostedStat speed stage
    modified = if paralysed then boosted * 50 `quot` 100 else boosted
{-# INLINE actionSpeed #-}
