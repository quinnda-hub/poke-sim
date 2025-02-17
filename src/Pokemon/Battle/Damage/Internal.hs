{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Damage.Internal where

import           Control.Monad.Trans.State           (get, put)
import           Data.Maybe                          (fromMaybe)
import           Pokemon.Battle.BattleState.Internal (BattleM, rng)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move, MoveCategory (..),
                                                      basePower, category, moveType)
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
--     - Critical: assumed 1.0 (no critical) for now.
--
--   The function uses the BattleM monad to retrieve and update the random number
--   generator stored in BattleState.

calculateDamage :: Move -> Pokemon -> Pokemon -> BattleM Int
calculateDamage move attacker defender = do
    let lvl   = attacker.level
        power = fromMaybe 0 move.basePower
        (aStat, dStat) = case move.category of
                           Physical -> (attacker.atk, defender.def)
                           Special  -> (attacker.spa, defender.spd)
                           Status   -> (0, 0)

        -- Compute base damage.
        baseDamage = (((2 * lvl) `div` 5 + 2) * power * aStat) `div` dStat `div` 50 + 2

    -- Add randomness for random damage rolls. 
    rng0 <- getRNG
    let (randInt, newRng) = randomR (85, 100) rng0 :: (Int, StdGen)
    updateRNG newRng
    let randFactor :: Double 
        randFactor  = fromIntegral randInt / 100.0

    -- Same Type Attack Bonus (STAB)
    let stab = if move.moveType == attacker.type1 ||
                  Just move.moveType == attacker.type2
                  then 1.5
                  else 1.0

    -- For now assume type effectiveness and critical multiplier are normal. 
    let typeEffect = 1.0
        crit       = 1.0

    let modifier = randFactor * stab * typeEffect * crit
        damage   = floor (fromIntegral baseDamage * modifier)

    return damage

--------------------------------------------------------------------------------
-- RNG Helpers
--------------------------------------------------------------------------------

-- | Retrieves the current random number generator from the BattleState.
getRNG :: BattleM StdGen
getRNG = do
    bs <- get
    return bs.rng

-- | Updates the random number generator stored in the BattleState.
updateRNG :: StdGen -> BattleM ()
updateRNG newGen = do
    bs <- get
    put bs { rng = newGen }

