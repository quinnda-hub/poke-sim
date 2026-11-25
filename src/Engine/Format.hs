-- | Format-dependent rules that the engine consults.
--
-- Mirrors what Showdown attaches per format (config/formats.ts). For slice 0
-- the only difference that matters is @battle.trunc@.
module Engine.Format
  ( TruncRule (..)
  , Format (..)
  , gen9ou
  , trunc
  , truncBits
  ) where

import Data.Bits ((.&.))

-- | Showdown's @battle.trunc@ variants.
data TruncRule
  = -- | @dex.trunc@ (sim/dex.ts): wrap to unsigned 32 bits; with a bit count, take the value modulo @2^bits@.
    TruncUnsigned32
  | -- | @Math.trunc@ (formats with @battle: { trunc: Math.trunc }@): no wrapping, bit count ignored.
    TruncPlain
  deriving (Eq, Show)

-- | The format knobs the engine reads.
newtype Format = Format
  { fmtTrunc :: TruncRule
  }
  deriving (Eq, Show)

-- | @[Gen 9] OU@.
gen9ou :: Format
gen9ou = Format {fmtTrunc = TruncUnsigned32}

-- | @tr(x)@ for an already-integral non-negative value (fractions are
-- truncated by the caller with 'quot').
trunc :: Format -> Int -> Int
trunc f x = case fmtTrunc f of
  TruncUnsigned32 -> x .&. 0xFFFFFFFF
  TruncPlain -> x
{-# INLINE trunc #-}

-- | @tr(x, bits)@.
truncBits :: Format -> Int -> Int -> Int
truncBits f bits x = case fmtTrunc f of
  TruncUnsigned32 -> (x .&. 0xFFFFFFFF) `rem` (2 ^ bits)
  TruncPlain -> x
{-# INLINE truncBits #-}
