{-# LANGUAGE OverloadedStrings #-}
-- | Type matchups, critical-hit odds and the damage formula
-- (Showdown battle-actions.ts @getDamage@ / @modifyDamage@, battle.ts @modify@ / @randomizer@).
module Engine.Damage
  ( critDenominator
  , isImmune
  , statusImmune
  , hasTypeNamed
  , typeIsNamed
  , typeMod
  , baseDamage
  , modifyDamage
  ) where

import qualified Data.Text as T
import qualified Data.Vector as V

import Engine.Dex
import Engine.Format (Format, trunc, truncBits)
import Engine.Rule (StatusId (..))
import Engine.State (Mon (..), MonStatus (..))

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

-- | Whether a Pokemon has the type with this name (@pokemon.hasType@ with no type changes yet).
hasTypeNamed :: Dex -> T.Text -> Mon -> Bool
hasTypeNamed dx name m = typeIsNamed dx (monType1 m) name || typeIsNamed dx (monType2 m) name

-- | Whether a type id ('noType' included) is the type with this name.
typeIsNamed :: Dex -> Int -> T.Text -> Bool
typeIsNamed dx t name = t >= 0 && t < V.length (dxTypeNames dx) && dxTypeNames dx V.! t == name

-- | The natural status immunity of @pokemon.runStatusImmunity@ / @dex.getImmunity@ (pokemon.ts @setStatus@;
-- @tox@ is checked as @psn@): data/typechart.ts gives @brn@ to Fire, @par@ to Electric, @psn@ to Poison and
-- Steel, @frz@ to Ice.
-- REVISIT: this table is hand-copied from data/typechart.ts (the status keys of @damageTaken@), because
-- tools/oracle/export-dex.js does not export them; a re-pin would not regenerate it. Export them and load
-- them from data/gen9/types.json when the export is next changed (slice 2a).
-- LATENT: abilities (Limber, Immunity, Water Veil, ...), Misty Terrain, Safeguard and the @Immunity@ event
-- are not modelled; nothing in the engine can attach to them yet.
statusImmune :: Dex -> StatusId -> Mon -> Bool
statusImmune dx st m = any (\t -> hasTypeNamed dx t m) types
  where
    types = case st of
      Brn -> ["Fire"]
      Par -> ["Electric"]
      Psn -> ["Poison", "Steel"]
      Tox -> ["Poison", "Steel"]
      Frz -> ["Ice"]

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
      -- battle-actions.ts modifyDamage ~1874: `if (pokemon.status === 'brn' && move.category === 'Physical' &&
      -- !pokemon.hasAbility('guts'))` and (gen >= 6) the move is not Facade (which has a handler, so is not
      -- supported): `modify(baseDamage, 0.5)`, after the type step and before the final ModifyDamage event
      base6 = if monStatus attacker == StatusBrn && mvPhysical mv then modifyBy 2048 base5 else base5
   in if base6 == 0 then 1 else truncBits f 16 base6
  where
    -- battle.modify(value, m) with the modifier already scaled by 4096.
    -- LATENT: Showdown accumulates ability/item modifiers with chainModify (battle.ts ~2337) before one modify();
    -- here STAB and burn are separate modify() calls in Showdown too, so modifyBy per step is equivalent.
    modifyBy m v = trunc f ((trunc f (v * m) + 2048 - 1) `quot` 4096)
