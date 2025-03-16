{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.MultiHitDamage where

import           Control.Monad                       (forM_)
import           Pokemon.Battle.BattleState.Internal (BattleM, getRNG,
                                                      pokemonName, updateRNG)
import           Pokemon.Battle.BattleState.Types    (PlayerSlot (..))
import           Pokemon.Battle.Damage.Internal      (calculateDamage)
import           Pokemon.Battle.Logging              (logDamage)
import           Pokemon.Battle.MoveEffect.Damage    (applyDamage)
import           Pokemon.Battle.MoveEffect.Internal  (applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (..), Multihit (..))
import           System.Random                       (randomR)

--------------------------------------------------------------------------------
-- Generic Multi-Hit Damage Effect Step
--------------------------------------------------------------------------------

-- | Applies multihit damage for moves without a dynamic base power callback.
applyMultiHitDamage :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyMultiHitDamage mv attacker defender targetSlots = do
  hitCount <- determineHitCount mv.multihit
  forM_ [1 :: Int .. hitCount] $ \_ -> do
    applyEffectForMove mv attacker defender targetSlots
      (\mv' attackerPokemon defenderPokemon _ _ -> calculateDamage mv' attackerPokemon defenderPokemon)
      (flip applyDamage)
      (\dmg slot bs -> logDamage dmg (pokemonName bs slot))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

determineHitCount :: Maybe Multihit -> BattleM Int
determineHitCount Nothing          = return 1
determineHitCount (Just (Fixed n)) = return n
determineHitCount (Just (Range low high))
  | low == 2 && high == 5 = do
      rng <- getRNG
      let (r, newRng) = randomR (1 :: Int, 100) rng
      updateRNG newRng
      if r <= 35 then return 2
      else if r <= 70 then return 3
      else if r <= 85 then return 4
      else return 5
  | otherwise = do
      rng <- getRNG
      let (n, newRng) = randomR (low, high) rng
      updateRNG newRng
      return n

