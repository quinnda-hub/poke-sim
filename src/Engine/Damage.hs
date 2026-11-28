{-# LANGUAGE OverloadedStrings #-}
-- | Type matchups, critical-hit odds and the damage formula
-- (Showdown battle-actions.ts @getDamage@ / @modifyDamage@, battle.ts @modify@ / @randomizer@).
module Engine.Damage
  ( critDenominator
  , isImmune
  , typeImmuneTo
  , statusKey
  , monTypes
  , monTypesPreTera
  , teraFloor
  , isStellar
  , stellarBoosts
  , hasTypeNamed
  , hasItemNamed
  , typeIdNamed
  , isGrounded
  , ignoringAbility
  , suppressingAbility
  , hasAbility
  , isSemiInvulnerable
  , typeIsNamed
  , typeMod
  , moveTypeMod
  , baseDamage
  , modifyDamage
  , finalDamage
  ) where

import Data.Bits (testBit)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV

import Engine.Dex
import Engine.Event (Body (..), CondId, EventId (..), HandlerSpec (..), NativeId (..), Prefix (..))
import Engine.Format (Format, trunc, truncBits)
import Engine.Modifier (modify)
import Engine.Rule (Hits (..), MoveRule (..), StatusId (..))
import Engine.State (Actor, Battle (..), Mon (..), MonStatus (..), MoveInUse (..), Volatile (..), monOf, noItem)

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
-- A Terastallized Pokemon returns @[terastallized]@ before the event (2161-2163), unless its Tera type is Stellar, which
-- keeps its types and runs the event as usual. Roost's volatile does not apply to a Terastallized Pokemon, and cannot be added
-- (data/moves.ts 15449).
-- LATENT: @addedType@ (Trick-or-Treat) is appended after the event (2167) and is not modelled. @setType@ (Soak, Burn Up)
-- would overwrite 'monType1'/'monType2'.
monTypes :: Dex -> Mon -> (Int, Int)
monTypes dx m
  | monTerastallized m && not (isStellar dx m) = (monTeraType m, noType)
  | otherwise = monTypesPreTera dx m

-- | @pokemon.terastallized === 'Stellar'@.
isStellar :: Dex -> Mon -> Bool
isStellar dx m = monTerastallized m && typeIsNamed dx (monTeraType m) "Stellar"

-- | Whether this hit's STAB step (battle-actions.ts 1835-1840) takes the Stellar branch for the attacker: it is Stellar, the move
-- has a type (@type !== '???'@, 1822) and the type is not yet in @stellarBoostedTypes@, or the move already carries
-- @stellarBoosted@ (an earlier hit of the same use). The branch then marks the move and pushes the type.
stellarBoosts :: Dex -> Mon -> MoveData -> Bool
stellarBoosts dx m mv =
  isStellar dx m && mvType mv /= noType && (not (testBit (monStellarBoosted m) (mvType mv)) || mvStellarBoosted mv)

-- | @pokemon.getTypes(false, true)@ (@preterastallized@): the stored types through the @Type@ event, as 'monTypes' computes
-- them for a Pokemon that has not Terastallized.
monTypesPreTera :: Dex -> Mon -> (Int, Int)
monTypesPreTera dx m
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
isImmune :: Dex -> Battle -> Actor -> Int -> Bool
isImmune dx b a atkType
  | atkType == noType = False
  | typeIsNamed dx atkType "Ground" = not (isGrounded dx b a)
  | otherwise = typeEffect dx t1 atkType == 3 || (t2 /= noType && typeEffect dx t2 atkType == 3)
  where
    (t1, t2) = monTypes dx (monOf b a)

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
-- Levitate: `this.hasAbility('levitate') && !this.battle.suppressingAbility(this)` (pokemon.ts 2185) returns null, not grounded. It goes
-- through 'hasAbility', so Gastro Acid, Neutralizing Gas and Mold Breaker all reach it.
isGrounded :: Dex -> Battle -> Actor -> Bool
isGrounded dx b a = not (hasTypeNamed dx "Flying" (monOf b a)) && not (hasAbility dx b a "levitate" && not (suppressingAbility dx b a))

-- | @pokemon.ignoringAbility()@ (pokemon.ts 879-895): a Pokemon not in play (gen >= 5). It is the one function that 'hasAbility' and
-- the dispatcher's ability gate ('Engine.Dispatch.runEvent', battle.ts 884-891) consult.
-- LATENT(8f): Gastro Acid, Neutralizing Gas (and the Ability Shield and @cantsuppress@ exceptions) and a transformed Pokemon's
-- @notransform@ ability are not modelled: no effect suppresses an ability yet.
ignoringAbility :: Battle -> Actor -> Bool
ignoringAbility b a = not (monActive (monOf b a))

-- | @battle.suppressingAbility(target)@ (battle.ts 366-369): the move in use (Mold Breaker and its kin set @ignoreAbility@) while its
-- user is in play and is not the target, and the target has no Ability Shield. 'bActiveMove' holds @ignoreAbility@, always False
-- until a phase sets it, so this is False.
suppressingAbility :: Dex -> Battle -> Actor -> Bool
suppressingAbility dx b a = case bActiveMove b of
  Just (MoveInUse user ignore) -> ignore && monActive (monOf b user) && user /= a && not (hasItemNamed dx "abilityshield" (monOf b a))
  Nothing -> False

-- | @pokemon.hasAbility(ability)@ (pokemon.ts): false while 'ignoringAbility', otherwise the ability's id is @ability@. Every test of a
-- Pokemon's ability outside the dispatcher's handler collection goes through here.
-- LATENT: Showdown's version takes a list and compares @this.ability@ only; 'monAbility' is that.
hasAbility :: Dex -> Battle -> Actor -> T.Text -> Bool
hasAbility dx b a name = not (ignoringAbility b a) && abilityId dx (monAbility (monOf b a)) == name

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
-- @roll16@ is the already drawn value of @random(16)@. @guts@ is @pokemon.hasAbility('guts')@ of the attacker (slice 8b), which the caller
-- reads through 'hasAbility': a burned Guts holder's damage is not halved.
modifyDamage :: Format -> Dex -> Mon -> Mon -> MoveData -> Bool -> Bool -> Int -> Int -> Int
modifyDamage f dx attacker defender mv crit guts roll16 base1 =
  let base2 = if crit then trunc f (base1 * 3 `quot` 2) else base1
      base3 = trunc f (trunc f (base2 * (100 - roll16)) `quot` 100)
      -- "???" never gets STAB (modifyDamage: `if (type !== '???')`); `baseDamage = this.battle.modify(baseDamage, stab)`
      -- with stab 1.5, a modify() of its own (battle-actions.ts ~1850), not a chainModify
      -- `pokemon.hasType(type) || pokemon.getTypes(false, true).includes(type)`: both read getTypes() ('monTypes')
      -- (1827: `pokemon.hasType(type) || pokemon.getTypes(false, true).includes(type)`), then 2 when the Tera type is the move's
      -- type and an original type (1844-1846)
      (at1, at2) = monTypes dx attacker
      (pt1, pt2) = monTypesPreTera dx attacker
      ty = mvType mv
      stab = ty /= noType && (ty == at1 || ty == at2 || ty == pt1 || ty == pt2)
      teraStab = monTerastallized attacker && monTeraType attacker == ty && (ty == pt1 || ty == pt2)
      -- Stellar (1835-1841): 2 for STAB, else [4915, 4096] (battle.modify: numerator 4915 over 4096, a modifier of 4915); no
      -- ModifySTAB. Not Stellar: the 2x above, then runEvent('ModifySTAB') (1847), whose only listener, Adaptability, is not
      -- supported (LATENT).
      base4
        | stellarBoosts dx attacker mv = modify f base3 (if stab then 8192 else 4915)
        | stab = modify f base3 (if teraStab then 8192 else 6144)
        | otherwise = base3
      tm = moveTypeMod dx mv defender
      base5
        | tm > 0 = base4 * 2 ^ tm
        | tm < 0 = iterate (\x -> trunc f (x `quot` 2)) base4 !! negate tm
        | otherwise = base4
      -- battle-actions.ts modifyDamage ~1874: `if (pokemon.status === 'brn' && move.category === 'Physical' &&
      -- !pokemon.hasAbility('guts'))` and (gen >= 6) the move is not Facade:
      -- `modify(baseDamage, 0.5)` (a modify() of its own, not a chainModify), after the type
      -- step and before the final ModifyDamage event
      base6 = if monStatus attacker == StatusBrn && mvPhysical mv && not guts && mvId mv /= "facade" then modify f base5 2048 else base5
   in base6

-- | The base-power floor of a Terastallized user (battle-actions.ts 1717-1726): a move of the user's Tera type (`hasType` reads
-- 'monTypes': the Tera type alone) with base power under 60, whose dex priority is at most 0 and which has no multihit (the
-- dex's), and is not variable-power (@basePower@ 0 or 150 with a @basePowerCallback@), is raised to 60. Runs after the
-- @BasePower@ event and its clamp. A Stellar user's condition is instead that the move's type (Struggle's '???' included, which
-- is never pushed) is not in @stellarBoostedTypes@ (1718-1719).
teraFloor :: Dex -> Mon -> MoveData -> Int -> Int
teraFloor dx user mv power
  | monTerastallized user, ofType
  , power < 60, mvPriority mv <= 0, mrHits (mvRule mv) == OneHit
  , not ((mvPower mv == 0 || mvPower mv == 150) && hasCallback) = 60
  | otherwise = power
  where
    ofType
      | isStellar dx user = mvType mv == noType || not (testBit (monStellarBoosted user) (mvType mv))
      | otherwise = mvType mv /= noType && mvType mv == fst (monTypes dx user)
    hasCallback = case moveSpec mv POwn EvBasePowerCallback of
      Just _ -> True
      Nothing -> False

-- | The end of @modifyDamage@ after @runEvent('ModifyDamage')@: `if (this.battle.gen !== 5 && !baseDamage) return 1`,
-- then 16-bit truncation, which can truncate to 0 (battle-actions.ts 1895-1899).
finalDamage :: Format -> Int -> Int
finalDamage f d = if d == 0 then 1 else truncBits f 16 d
