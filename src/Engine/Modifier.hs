-- | Modifier arithmetic of Showdown's @sim/battle.ts@ (@chain@, @chainModify@, @modify@, @finalModify@,
-- 2321-2401), in integers.
--
-- A modifier is a multiplier in units of 1/4096: 'baseMod' (4096) is 1, 6144 is 1.5. Showdown keeps
-- @event.modifier@ as the double @units / 4096@; every use multiplies it back by 4096 first, which is exact, so
-- the integer is the same value. Fractions are written as integer pairs (1.3 is @(13, 10)@), as in
-- @chainModify([13, 10])@.
module Engine.Modifier
  ( baseMod
  , chain
  , chainModify
  , modify
  , modifyBy
  , finalModify
  ) where

import Data.Bits (shiftR)
import Data.Int (Int32)

import Engine.Format (Format, trunc)

-- | The modifier 1 (@event.modifier = 1@).
baseMod :: Int
baseMod = 4096

-- | @(a * b + 0x800) >> 12@: JavaScript's @>>@ works on a signed 32-bit integer, so a product past 2^31 wraps.
mulRound :: Int -> Int -> Int
mulRound a b = fromIntegral (fromIntegral (a * b + 2048) :: Int32) `shiftR` 12

-- | @battle.chain(previousMod, nextMod)@ of two scaled modifiers, scaled: @M'' = ((M * M') + 0x800) >> 12@.
chain :: Int -> Int -> Int
chain = mulRound

-- | @battle.chainModify(numerator, denominator)@: the event's modifier (scaled) times @n / d@. The next factor is
-- @trunc(n * 4096 / d)@.
chainModify :: Format -> Int -> (Int, Int) -> Int
chainModify f prev (n, d) = mulRound prev (trunc f (n * 4096 `quot` d))

-- | @battle.modify(value, modifier)@ of a scaled modifier: @tr((tr(value * modifier) + 2048 - 1) / 4096)@.
modify :: Format -> Int -> Int -> Int
modify f v m = trunc f ((trunc f (v * m) + 2047) `quot` 4096)

-- | @battle.modify(value, n, d)@: the modifier is @trunc(n * 4096 / d)@.
modifyBy :: Format -> Int -> (Int, Int) -> Int
modifyBy f v (n, d) = modify f v (trunc f (n * 4096 `quot` d))

-- | @battle.finalModify(relayVar)@: apply the accumulated modifier, then reset it to 1. Returns the value and the
-- new modifier.
finalModify :: Format -> Int -> Int -> (Int, Int)
finalModify f v m = (modify f v m, baseMod)
