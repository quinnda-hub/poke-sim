-- | Type matchups, critical-hit odds and the damage formula
-- (Showdown battle-actions.ts @getDamage@ / @modifyDamage@, battle.ts @modify@ / @randomizer@).
module Engine.Damage
  ( critDenominator
  , isImmune
  , typeMod
  , baseDamage
  , modifyDamage
  ) where

import Engine.Dex
import Engine.Format (Format, trunc, truncBits)
import Engine.State (Mon (..))

-- | Denominator of the crit chance for a crit stage, or 'Nothing' when the move cannot crit
-- (gen 7+ table @[0, 24, 8, 2, 1]@ indexed by the clamped @critRatio@).
critDenominator :: Int -> Maybe Int
critDenominator ratio = case max 0 (min 4 ratio) of
  0 -> Nothing
  1 -> Just 24
  2 -> Just 8
  3 -> Just 2
  _ -> Just 1

-- | Type immunity of the defender to a move type (@runImmunity@ through the type chart).
isImmune :: Dex -> Int -> Mon -> Bool
isImmune dx atkType m =
  -- pokemon.ts runImmunity: the typeless "???" (Struggle) is never blocked
  -- LATENT: species types are used where Showdown calls getTypes() (Terastallization, Roost, type changes).
  -- LATENT: Ground immunity in Showdown goes through isGrounded() (Levitate, Air Balloon, Gravity, Roost).
  atkType /= noType && (typeEffect dx (monType1 m) atkType == 3
    || (monType2 m /= noType && typeEffect dx (monType2 m) atkType == 3))

-- | Total type effectiveness exponent (@runEffectiveness@): +1 per super-effective type, -1 per resisted, clamped to [-6, 6].
typeMod :: Dex -> Int -> Mon -> Int
typeMod dx atkType m
  | atkType == noType = 0 -- dex.getEffectiveness: an unknown source type is neutral
  | otherwise = max (-6) (min 6 (one (monType1 m) + (if monType2 m == noType then 0 else one (monType2 m))))
  where
    one t = case typeEffect dx t atkType of
      1 -> 1
      2 -> -1
      _ -> 0

-- | @tr(tr(tr(tr(2L/5 + 2) * BP * A) / D) / 50)@.
baseDamage :: Format -> Int -> Int -> Int -> Int -> Int
baseDamage f level power atk def =
  let t1 = (2 * level) `quot` 5 + 2
      t2 = trunc f (trunc f (t1 * power * atk) `quotZ` def)
   in trunc f (t2 `quot` 50)
  where
    -- JS division by zero yields Infinity, which truncates to 0.
    quotZ a b = if b == 0 then 0 else a `quot` b

-- | @modifyDamage@ for a plain move: crit, random factor, STAB, type effectiveness, minimum 1, 16-bit truncation.
-- @roll16@ is the already drawn value of @random(16)@.
modifyDamage :: Format -> Dex -> Mon -> Mon -> MoveData -> Bool -> Int -> Int -> Int
modifyDamage f dx attacker defender mv crit roll16 base0 =
  let base1 = base0 + 2
      base2 = if crit then trunc f (base1 * 3 `quot` 2) else base1
      base3 = trunc f (trunc f (base2 * (100 - roll16)) `quot` 100)
      -- "???" never gets STAB (modifyDamage: `if (type !== '???')`)
      -- LATENT: Showdown uses hasType()/getTypes() and runs ModifySTAB (Adaptability, Terastallization).
      stab = mvType mv /= noType && (mvType mv == monType1 attacker || mvType mv == monType2 attacker)
      base4 = if stab then modifyBy 6144 base3 else base3
      tm = typeMod dx (mvType mv) defender
      base5
        | tm > 0 = base4 * 2 ^ tm
        | tm < 0 = iterate (\x -> trunc f (x `quot` 2)) base4 !! negate tm
        | otherwise = base4
   in if base5 == 0 then 1 else truncBits f 16 base5
  where
    -- battle.modify(value, m) with the modifier already scaled by 4096.
    -- LATENT: Showdown accumulates ability/item modifiers with chainModify (battle.ts ~2337) before one modify();
    -- here STAB is the only modifier, so a single modifyBy is equivalent.
    modifyBy m v = trunc f ((trunc f (v * m) + 2048 - 1) `quot` 4096)
