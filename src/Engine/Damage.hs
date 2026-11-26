{-# LANGUAGE OverloadedStrings #-}
-- | Type matchups, critical-hit odds and the damage formula
-- (Showdown battle-actions.ts @getDamage@ / @modifyDamage@, battle.ts @modify@ / @randomizer@).
module Engine.Damage
  ( critDenominator
  , isImmune
  , typeImmuneTo
  , statusKey
  , monTypes
  , hasTypeNamed
  , hasItemNamed
  , typeIdNamed
  , isGrounded
  , isSemiInvulnerable
  , typeIsNamed
  , typeMod
  , moveTypeMod
  , baseDamage
  , modifyDamage
  , finalDamage
  ) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV

import Engine.Dex
import Engine.Event (Body (..), CondId, EventId (..), HandlerSpec (..), NativeId (..), Prefix (..))
import Engine.Format (Format, trunc, truncBits)
import Engine.Modifier (modify)
import Engine.Rule (StatusId (..))
import Engine.State (Mon (..), MonStatus (..), Volatile (..), noItem)

-- | Denominator of the crit chance for a crit stage, or 'Nothing' when the move cannot crit
-- (gen 7+ table @[0, 24, 8, 2, 1]@ indexed by the clamped @critRatio@).
critDenominator :: Int -> Maybe Int
critDenominator ratio = case max 0 (min 4 ratio) of
  0 -> Nothing
  1 -> Just 24
  2 -> Just 8
  3 -> Just 2
  _ -> Just 1

-- | @pokemon.getTypes()@ (pokemon.ts 2160-2168): @runEvent('Type', this, null, null, this.types)@, then Normal for a Pokemon
-- left with none (gen >= 5). The pair of types, 'noType' for a missing second one. Every read of a Pokemon's types goes
-- through here (STAB, the type chart, @getImmunity@, @runImmunity@, @hasType@, @isGrounded@); 'monType1' and 'monType2' are
-- Showdown's stored @this.types@.
--
-- The event's only listener is the Roost volatile (data/moves.ts @roost.condition.onType@: the types without Flying). An
-- effect is supported only when every handler the export lists for it has a body, and "Engine.Effects" has none for
-- @Type@ (a property checks that no table but Roost's volatile has one). So the @runEvent@ is a fold over the Pokemon's
-- volatiles that have the handler, as a pure function; at most one does, so it sorts nothing and draws nothing. The fast path,
-- a Pokemon with no volatile, returns the stored types: exact because no ability, item, status, side, field or foe handler of
-- the event exists.
-- LATENT: a Terastallized Pokemon returns @[terastallized]@ before the event (2161), and @addedType@ (Trick-or-Treat) is
-- appended after it (2167); neither is modelled. @setType@ (Soak, Burn Up) would overwrite 'monType1'/'monType2'.
monTypes :: Dex -> Mon -> (Int, Int)
monTypes dx m
  | SV.null vs = (monType1 m, monType2 m)
  | otherwise = SV.foldl' apply (monType1 m, monType2 m) vs
  where
    vs = monVolatiles m
    apply ts (Volatile i _) = case condSpec (conditionOf dx (toEnum i :: CondId)) POwn EvType >>= hsBody of
      -- roost.onType(types): `types.filter(type => type !== 'Flying')`; an empty result is ['Normal'] (getTypes)
      Just (BNative NRoostType) -> withoutFlying ts
      _ -> ts
    withoutFlying (t1, t2) =
      case [t | t <- [t1, t2], t /= noType, not (typeIsNamed dx t "Flying")] of
        [] -> (fromMaybe noType (typeIdNamed dx "Normal"), noType)
        [a] -> (a, noType)
        (a : b' : _) -> (a, b')

-- | Type immunity of the defender to a move type (@pokemon.runImmunity@, pokemon.ts 2258-2290; the move's own
-- @ignoreImmunity@ is the caller's). The typeless "???" (Struggle) is never blocked. A Ground move goes through
-- @isGrounded(negateImmunity)@ (a Roosted Flying type is grounded, so Ground hits it), every other type through
-- @dex.getImmunity(type, this)@ on @getTypes()@ ('monTypes').
-- LATENT: @runEvent('NegateImmunity')@ (Scrappy, Ring Target, Iron Ball) has no listener.
isImmune :: Dex -> Int -> Mon -> Bool
isImmune dx atkType m
  | atkType == noType = False
  | typeIsNamed dx atkType "Ground" = not (isGrounded dx m)
  | otherwise = typeEffect dx t1 atkType == 3 || (t2 /= noType && typeEffect dx t2 atkType == 3)
  where
    (t1, t2) = monTypes dx m

-- | Whether a Pokemon has the type with this name (@pokemon.hasType@, which reads @getTypes()@: 'monTypes').
hasTypeNamed :: Dex -> T.Text -> Mon -> Bool
hasTypeNamed dx name m = typeIsNamed dx t1 name || typeIsNamed dx t2 name
  where
    (t1, t2) = monTypes dx m

-- | Whether a Pokemon holds the item with this id (@pokemon.hasItem@).
-- LATENT: @hasItem@ is false while @ignoringItem()@ (Klutz, Embargo, Magic Room); none can happen yet.
hasItemNamed :: Dex -> T.Text -> Mon -> Bool
hasItemNamed dx name m = monItem m /= noItem && itemId dx (monItem m) == name

-- | The id of the type with this name.
typeIdNamed :: Dex -> T.Text -> Maybe Int
typeIdNamed dx name = V.findIndex (== name) (dxTypeNames dx)

-- | @pokemon.isGrounded()@ (pokemon.ts 2170) as a boolean (Showdown's @null@, Levitate, counts as not grounded):
-- a Pokemon with the Flying type, as @getTypes()@ gives it (a Roosted Flying type has lost it: see 'isImmune'), is not
-- grounded.
-- LATENT: the rest of the function reads state the engine does not hold yet. Grounded regardless of type:
-- Gravity (field pseudo-weather), Ingrain and Smack Down (volatiles), Iron Ball (item). Not grounded: Levitate (and
-- Eelevate), Magnet Rise and Telekinesis (volatiles), Air Balloon (item). None of these is supported, so a Pokemon is
-- grounded exactly when it does not have the Flying type. (The Burn Up clause, a @???@ Roosted type, is not modelled.)
-- The terrains' bodies also read @isSemiInvulnerable()@ ('isSemiInvulnerable').
isGrounded :: Dex -> Mon -> Bool
isGrounded dx m = not (hasTypeNamed dx "Flying" m)

-- | @pokemon.isSemiInvulnerable()@ (pokemon.ts 2184): the @fly@, @bounce@, @dive@, @dig@, @phantomforce@, @shadowforce@
-- and @skydrop@ volatiles.
-- LATENT: none of these volatiles exists in the state (their moves are unsupported), so this is always false.
isSemiInvulnerable :: Mon -> Bool
isSemiInvulnerable _ = False

-- | Whether a type id ('noType' included) is the type with this name.
typeIsNamed :: Dex -> Int -> T.Text -> Bool
typeIsNamed dx t name = t >= 0 && t < V.length (dxTypeNames dx) && dxTypeNames dx V.! t == name

-- | The natural immunity of @dex.getImmunity(type, pokemon)@ (pokemon.ts @runStatusImmunity@): a type is immune when
-- its typechart @damageTaken@ entry for the key is 3 (loaded from @types.json@ @otherDamageTaken@). The key is the
-- status (@tox@ is checked as @psn@: pokemon.ts @setStatus@) or the weather (@sandstorm@).
-- The @Immunity@ event (Sunny Day, Sand Rush) runs after this in "Engine.Dispatch.runStatusImmunity".
-- LATENT: Misty Terrain and Safeguard (@SetStatus@, @TryAddVolatile@), and the ability checks of @setStatus@
-- (Corrosion), are not modelled; Overcoat's @Immunity@ (powder, sandstorm) comes with phase 3d.
typeImmuneTo :: Dex -> T.Text -> Mon -> Bool
typeImmuneTo dx key m = any immune [t1, t2]
  where
    (t1, t2) = monTypes dx m
    immune t = t /= noType && key `elem` (dxTypeImmune dx V.! t)

-- | The key @setStatus@ passes to @runStatusImmunity@ for a status.
statusKey :: StatusId -> T.Text
statusKey st = case st of
  Brn -> "brn"
  Par -> "par"
  Psn -> "psn"
  Tox -> "psn"
  Frz -> "frz"

-- | Total type effectiveness exponent (@runEffectiveness@): +1 per super-effective type, -1 per resisted, clamped to [-6, 6].
typeMod :: Dex -> Int -> Mon -> Int
typeMod dx atkType m
  | atkType == noType = 0 -- dex.getEffectiveness: an unknown source type is neutral
  | otherwise = max (-6) (min 6 (one t1 + (if t2 == noType then 0 else one t2)))
  where
    (t1, t2) = monTypes dx m
    one t = case typeEffect dx t atkType of
      1 -> 1
      2 -> -1
      _ -> 0

-- | @pokemon.runEffectiveness(move)@ (pokemon.ts 2230-2245) of the move in use: for each defending type,
-- @dex.getEffectiveness(move, type)@, then @singleEvent('Effectiveness', move, null, this, type, move, typeMod)@, which a
-- handler of the move may replace (Freeze-Dry, Flying Press), then @runEvent('Effectiveness', ...)@; the total is
-- clamped to [-6, 6] by the caller (@modifyDamage@, battle-actions.ts 1855).
--
-- The fast path is 'typeMod': a move with no @Effectiveness@ handler of its own. It is sound because no other supported
-- effect listens to @Effectiveness@: an ability, item or condition is supported only when each handler the export lists
-- has a body, and "Engine.Effects" has none for this event except the moves' own (a property checks it). So the
-- @runEvent@ has no handler to run, and with fewer than two it draws nothing.
-- LATENT: Tera Shell and Terastallization (the other parts of @runEffectiveness@) are not supported.
moveTypeMod :: Dex -> MoveData -> Mon -> Int
moveTypeMod dx mv m = case moveSpec mv POwn EvEffectiveness of
  Nothing -> typeMod dx (mvType mv) m
  Just spec
    | mvType mv == noType -> 0
    | otherwise -> max (-6) (min 6 (one t1 + (if t2 == noType then 0 else one t2)))
    where
      (t1, t2) = monTypes dx m
      one t =
        let base = case typeEffect dx t (mvType mv) of
              1 -> 1
              2 -> -1
              _ -> 0
         in case hsBody spec of
              Just (BNative n) -> effectiveness n t base
              _ -> base
  where
    flying = typeIdNamed dx "Flying"
    -- freezedry.onEffectiveness(typeMod, target, type): `if (type === 'Water') return 1`
    effectiveness n t base = case n of
      NFreezeDryEffectiveness | typeIsNamed dx t "Water" -> 1
      -- flyingpress.onEffectiveness(typeMod, target, type, move): `typeMod + this.dex.getEffectiveness('Flying', type)`
      NFlyingPressEffectiveness -> base + maybe 0 (\f -> case typeEffect dx t f of 1 -> 1; 2 -> -1; _ -> 0) flying
      _ -> base

-- | @tr(tr(tr(tr(2L/5 + 2) * BP * A) / D) / 50)@.
baseDamage :: Format -> Int -> Int -> Int -> Int -> Int
baseDamage f level power atk def =
  let t1 = (2 * level) `quot` 5 + 2
      t2 = trunc f (trunc f (t1 * power * atk) `quotZ` def)
   in trunc f (t2 `quot` 50)
  where
    -- JS division by zero yields Infinity, which truncates to 0.
    quotZ a b = if b == 0 then 0 else a `quot` b

-- | @modifyDamage@ (battle-actions.ts ~1790-1890) from after the @WeatherModifyDamage@ event, up to the @ModifyDamage@
-- event: crit, random factor, STAB, type effectiveness and the burn. 'finalDamage' is the rest. The caller adds 2 to
-- the base damage (`baseDamage += 2`), runs @WeatherModifyDamage@ on it (1804) and passes the result as @base1@.
-- @roll16@ is the already drawn value of @random(16)@.
modifyDamage :: Format -> Dex -> Mon -> Mon -> MoveData -> Bool -> Int -> Int -> Int
modifyDamage f dx attacker defender mv crit roll16 base1 =
  let base2 = if crit then trunc f (base1 * 3 `quot` 2) else base1
      base3 = trunc f (trunc f (base2 * (100 - roll16)) `quot` 100)
      -- "???" never gets STAB (modifyDamage: `if (type !== '???')`); `baseDamage = this.battle.modify(baseDamage, stab)`
      -- with stab 1.5, a modify() of its own (battle-actions.ts ~1850), not a chainModify
      -- `pokemon.hasType(type) || pokemon.getTypes(false, true).includes(type)`: both read getTypes() ('monTypes')
      -- LATENT: Showdown runs ModifySTAB (Adaptability, Terastallization).
      (at1, at2) = monTypes dx attacker
      stab = mvType mv /= noType && (mvType mv == at1 || mvType mv == at2)
      base4 = if stab then modify f base3 6144 else base3
      tm = moveTypeMod dx mv defender
      base5
        | tm > 0 = base4 * 2 ^ tm
        | tm < 0 = iterate (\x -> trunc f (x `quot` 2)) base4 !! negate tm
        | otherwise = base4
      -- battle-actions.ts modifyDamage ~1874: `if (pokemon.status === 'brn' && move.category === 'Physical' &&
      -- !pokemon.hasAbility('guts'))` and (gen >= 6) the move is not Facade:
      -- `modify(baseDamage, 0.5)` (a modify() of its own, not a chainModify), after the type
      -- step and before the final ModifyDamage event
      base6 = if monStatus attacker == StatusBrn && mvPhysical mv && mvId mv /= "facade" then modify f base5 2048 else base5
   in base6

-- | The end of @modifyDamage@ after @runEvent('ModifyDamage')@: `if (this.battle.gen !== 5 && !baseDamage) return 1`,
-- then 16-bit truncation, which can truncate to 0 (battle-actions.ts 1895-1899).
finalDamage :: Format -> Int -> Int
finalDamage f d = if d == 0 then 1 else truncBits f 16 d
