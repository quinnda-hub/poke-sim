{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.MultiHitDamage where

import           Control.Monad                       (forM_)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState,
                                                      getRNG, pokemonName,
                                                      updateRNG)
import           Pokemon.Battle.BattleState.Types    (PlayerSlot (..))
import           Pokemon.Battle.Damage.Internal      (calculateDamage)
import           Pokemon.Battle.Logging              (logDamage)
import           Pokemon.Battle.MoveEffect.Damage    (applyDamage,
                                                      applyDamageToActive)
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
  forM_ [1 .. hitCount] $ \_ -> do
    applyEffectForMove mv attacker defender targetSlots
      (\mv' atk def slot bs -> calculateDamage mv' atk def)
      (flip applyDamage)
      (\dmg slot bs -> logDamage dmg (pokemonName bs slot))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

determineHitCount = undefined


