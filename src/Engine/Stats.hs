-- | Stat calculation (Showdown @battle.spreadModify@ / @statModify@, battle.ts).
module Engine.Stats
  ( hpStat
  , otherStat
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

-- | @pokemon.getActionSpeed()@ with no boosts or modifiers (pokemon.ts): @getStat('spe')@ capped at 10000
-- unless the format overrides @trunc@ (pokemon.ts ~658), then @tr(speed, 13)@.
-- The cap cannot be reached at levels <= 100 (the largest Spe stat is under 700); it is kept for fidelity.
-- LATENT: Showdown runs the @ModifySpe@ event inside getStat, and the team-preview and action speeds
-- here do not go through it (items, abilities, Trick Room, paralysis, Tailwind).
actionSpeed :: Format -> Int -> Int
actionSpeed f speed = truncBits f 13 (case fmtTrunc f of
  TruncUnsigned32 -> min 10000 speed
  TruncPlain -> speed)
{-# INLINE actionSpeed #-}
