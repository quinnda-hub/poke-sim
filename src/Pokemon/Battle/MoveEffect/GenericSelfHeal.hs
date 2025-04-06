{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.GenericSelfHeal
  ( gatherGenericSelfHeal
  , applyGenericHealForMove
  )
where

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      player1, player2)
import           Pokemon.Battle.BattleState.Types    (Player (..),
                                                      PlayerSlot (..))
import           Pokemon.Battle.Logging              (logSelfHeal)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (GenericSelfHeal),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Heal (..), Move (heal))

-- | If the move has the field heal, produce a generic heal effect step.
gatherGenericSelfHeal :: Move -> Maybe EffectStep
gatherGenericSelfHeal mv =
  case mv.heal of
    Nothing -> Nothing
    Just _  -> Just GenericSelfHeal

-- Calculate heal amount based on max HP of the Pokemon.
calculateSelfHeal :: Int -> Int -> Int -> Int
calculateSelfHeal mHP num denom = (mHP * num) `div` denom

-- | Helper function to update a player's active Pokemon with
--   its new HP after healing.
applyHealToActive :: Int -> Player BattleM -> Player BattleM
applyHealToActive healAmount p =
  let oldHP = p.active.hp
      newHP = max p.active.maxHP (oldHP + healAmount)
  in p { active = p.active { hp = newHP } }

-- | Applies healing to the appropriate player.
applySelfHeal :: PlayerSlot -> Int -> BattleState -> BattleState
applySelfHeal Player1 healAmount bs = bs { player1 = applyHealToActive healAmount bs.player1 }
applySelfHeal Player2 healAmount bs = bs { player2 = applyHealToActive healAmount bs.player2 }

applyGenericHealForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyGenericHealForMove mv attackerPokemon defenderPokemon targetSlots =
  applyEffectForMove mv attackerPokemon defenderPokemon targetSlots
    (\mv' attackerPokemon' _ _ _ ->
      case mv'.heal of
        Just (Heal num denom) -> return (calculateSelfHeal attackerPokemon'.maxHP num denom)
        Nothing -> return 0)
    (flip applySelfHeal)
    (\selfHeal slot bs ->
      let attackerName = case slot of
                           Player1 -> bs.player1.active.name
                           Player2 -> bs.player2.active.name
      in logSelfHeal selfHeal attackerName)

