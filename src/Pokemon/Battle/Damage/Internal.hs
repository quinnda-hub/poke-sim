{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE Strict                #-}

module Pokemon.Battle.Damage.Internal (calculateDamage) where

import           Control.Monad                       (when)
import           Data.Maybe                          (fromMaybe)
import           Pokemon.Battle.BattleState.Internal (BattleM, getRNG,
                                                      updateRNG)
import           Pokemon.Battle.Logging              (logCrit)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..),
                                                      PokemonBoosts (..))
import           Pokemon.Data.Move.Internal          (Move, MoveCategory (..),
                                                      basePower, category,
                                                      critRatio, moveType)
import           Pokemon.Mechanics.PType             (PType (..))
import           System.Random                       (Random (randomR), StdGen)

--------------------------------------------------------------------------------
-- Damage Calculation
--------------------------------------------------------------------------------

-- | Calculates damage for a move from an attacker to a defender.
--
--   The simplified damage formula used is:
--
--     Damage = floor( baseDamage * Modifier )
--
--   where:
--
--     baseDamage = (((2 * Level / 5 + 2) * Power * (A / D)) / 50) + 2
--
--   and Modifier is the product of:
--
--     - A random factor between 0.85 and 1.00,
--     - STAB: 1.5 if the move's type matches the attacker's type, otherwise 1.0,
--     - Type effectiveness: assumed 1.0 (neutral) for now,
--
--   The function uses the BattleM monad to retrieve and update the random number
--   generator stored in BattleState.
calculateDamage :: Move -> Pokemon -> Pokemon -> BattleM Int
calculateDamage move attacker defender
  | move.category == Status = return 0
  | otherwise = do
     let lvl   = attacker.level
         power = fromMaybe 0 move.basePower
         (baseAStat, baseDStat, boostAStage, boostDStage) =
           case move.category of
             Physical -> (attacker.atk, defender.def, attacker.boosts.atk, defender.boosts.def)
             Special  -> (attacker.spa, defender.spd, attacker.boosts.spa, defender.boosts.spd)
             _        -> (0, 0, 0, 0) -- This would only occur if there was an option besides Physical, Special and Status.
         effectiveA = floor $ fromIntegral baseAStat * boostMultiplier boostAStage
     rng0 <- getRNG
     let (randInt, rng1) = randomR (85, 100) rng0 :: (Int, StdGen)
         (isCrit,  rng2) = rollCritical move rng1
         -- If it is a critical hit, ignore the defender's boosts.
         effectiveD = if isCrit
                      then baseDStat
                      else floor $ fromIntegral baseDStat * boostMultiplier boostDStage
         -- Base damage computed using integer arithmetic.
         baseDamage = (((2 * lvl) `div` 5 + 2) * power * effectiveA)
                      `div` effectiveD `div` 50 + 2
     updateRNG rng2
     when isCrit $ logCrit attacker.name
     return (calcDamage randInt isCrit baseDamage)
  where
    -- Computes the boost multiplier for the given stage.
    boostMultiplier :: Int -> Double
    boostMultiplier stage
      | stage >= 0 = (2 + fromIntegral stage) / 2
      | otherwise  = 2 / (2 - fromIntegral stage)
    -- | Determines whether the move results in a critical hit based on its crit ratio.
    rollCritical :: Move -> StdGen -> (Bool, StdGen)
    rollCritical m rng =
      case m.critRatio of
        Nothing ->
          let (roll, rng') = randomR (1, 24) rng :: (Int, StdGen)
          in (roll == 1, rng')
        Just _ ->
          let (roll, rng') = randomR (1, 12) rng :: (Int, StdGen)
          in (roll == 1, rng')

    -- | Given a random integer, compute the final damage.
    calcDamage randInt isCrit baseDamage =
      let randFactor = fromIntegral randInt / 100.0
          stab | move.moveType == attacker.type1 = 1.5
               | Just move.moveType == attacker.type2 = 1.5
               | otherwise = 1.0
          typeEffect = effectiveness move.moveType defender.type1 defender.type2
          crit       = if isCrit then 1.5 else 1.0
          modifier   = randFactor * stab * typeEffect * crit
      in floor (fromIntegral baseDamage * modifier)

--------------------------------------------------------------------------------
-- Type Effectiveness Calculation
--------------------------------------------------------------------------------

-- | Computes the overall effectiveness multiplier for a given move type
--   against a defenderender with primary type `defenderType1` and optional secondary type `defenderType2`.
effectiveness :: PType -> PType -> Maybe PType -> Double
effectiveness mType defenderType1 defenderType2 =
    let eff1 = singleEffectiveness mType defenderType1
        eff2 = maybe 1.0 (singleEffectiveness mType) defenderType2
    in eff1 * eff2

-- | Computes the effectiveness multiplier for a single type matchup.
--   This is a simplified version; extend it with the full type chart.
singleEffectiveness :: PType -> PType -> Double
-- Normal
singleEffectiveness Normal defender =
  case defender of
    Rock  -> 0.5
    Steel -> 0.5
    Ghost -> 0.0
    _     -> 1.0
-- Fire
singleEffectiveness Fire defender =
  case defender of
    Fire   -> 0.5
    Water  -> 0.5
    Grass  -> 2.0
    Ice    -> 2.0
    Bug    -> 2.0
    Rock   -> 0.5
    Dragon -> 0.5
    Steel  -> 2.0
    _      -> 1.0
-- Water
singleEffectiveness Water defender =
  case defender of
    Fire   -> 2.0
    Water  -> 0.5
    Grass  -> 0.5
    Ground -> 2.0
    Rock   -> 2.0
    Dragon -> 0.5
    _      -> 1.0
-- Electric
singleEffectiveness Electric defender =
  case defender of
    Water    -> 2.0
    Electric -> 0.5
    Grass    -> 0.5
    Ground   -> 0.0
    Flying   -> 2.0
    Dragon   -> 0.5
    _        -> 1.0
-- Grass
singleEffectiveness Grass defender =
  case defender of
    Fire   -> 0.5
    Water  -> 2.0
    Grass  -> 0.5
    Poison -> 0.5
    Ground -> 2.0
    Flying -> 0.5
    Bug    -> 0.5
    Rock   -> 2.0
    Dragon -> 0.5
    Steel  -> 0.5
    _      -> 1.0
-- Ice
singleEffectiveness Ice defender =
  case defender of
    Grass  -> 2.0
    Ground -> 2.0
    Flying -> 2.0
    Dragon -> 2.0
    Fire   -> 0.5
    Water  -> 0.5
    Ice    -> 0.5
    Steel  -> 0.5
    _      -> 1.0
-- Fighting
singleEffectiveness Fighting defender =
  case defender of
    Normal  -> 2.0
    Ice     -> 2.0
    Rock    -> 2.0
    Dark    -> 2.0
    Steel   -> 2.0
    Poison  -> 0.5
    Flying  -> 0.5
    Psychic -> 0.5
    Bug     -> 0.5
    Fairy   -> 0.5
    Ghost   -> 0.0
    _       -> 1.0
-- Poison
singleEffectiveness Poison defender =
  case defender of
    Grass  -> 2.0
    Fairy  -> 2.0
    Poison -> 0.5
    Ground -> 0.5
    Rock   -> 0.5
    Ghost  -> 0.5
    Steel  -> 0.0
    _      -> 1.0
-- Ground
singleEffectiveness Ground defender =
  case defender of
    Fire     -> 2.0
    Electric -> 2.0
    Poison   -> 2.0
    Rock     -> 2.0
    Steel    -> 2.0
    Grass    -> 0.5
    Bug      -> 0.5
    Flying   -> 0.0
    _        -> 1.0
-- Flying
singleEffectiveness Flying defender =
  case defender of
    Grass    -> 2.0
    Fighting -> 2.0
    Bug      -> 2.0
    Electric -> 0.5
    Rock     -> 0.5
    _        -> 1.0
-- Psychic
singleEffectiveness Psychic defender =
  case defender of
    Fighting -> 2.0
    Poison   -> 2.0
    Psychic  -> 0.5
    Dark     -> 0.0
    Steel    -> 0.5
    _        -> 1.0
-- Bug
singleEffectiveness Bug defender =
  case defender of
    Grass    -> 2.0
    Psychic  -> 2.0
    Dark     -> 2.0
    Fire     -> 0.5
    Fighting -> 0.5
    Poison   -> 0.5
    Flying   -> 0.5
    Ghost    -> 0.5
    Steel    -> 0.5
    Fairy    -> 0.5
    _        -> 1.0
-- Rock
singleEffectiveness Rock defender =
  case defender of
    Fire     -> 2.0
    Ice      -> 2.0
    Flying   -> 2.0
    Bug      -> 2.0
    Fighting -> 0.5
    Ground   -> 0.5
    Steel    -> 0.5
    _        -> 1.0
-- Ghost
singleEffectiveness Ghost defender =
  case defender of
    Psychic -> 2.0
    Ghost   -> 2.0
    Normal  -> 0.0
    _       -> 1.0
-- Dragon
singleEffectiveness Dragon defender =
  case defender of
    Dragon -> 2.0
    Steel  -> 0.5
    Fairy  -> 0.0
    _      -> 1.0
-- Dark
singleEffectiveness Dark defender =
  case defender of
    Psychic  -> 2.0
    Ghost    -> 2.0
    Fighting -> 0.5
    Dark     -> 0.5
    Fairy    -> 0.5
    _        -> 1.0
-- Steel
singleEffectiveness Steel defender =
  case defender of
    Ice      -> 2.0
    Rock     -> 2.0
    Fairy    -> 2.0
    Fire     -> 0.5
    Water    -> 0.5
    Electric -> 0.5
    Steel    -> 0.5
    _        -> 1.0
-- Fairy
singleEffectiveness Fairy defender =
  case defender of
    Fighting -> 2.0
    Dragon   -> 2.0
    Dark     -> 2.0
    Fire     -> 0.5
    Poison   -> 0.5
    Steel    -> 0.5
    _        -> 1.0
