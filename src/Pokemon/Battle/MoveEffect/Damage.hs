{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Damage where

import           Pokemon.Battle.BattleState.Types    (Player (..),
                                                      PlayerSlot (..),
                                                      lastDamage)
import           Pokemon.Battle.Logging              (logDamage)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (DoDamage),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (basePower, name),
                                                      multihit)

import           Data.Maybe                          (isJust)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..))
import           Pokemon.Battle.Damage.Internal      (calculateDamage)

-- | If the move has a base power, produce a damage step.
gatherDamage :: Move -> Maybe EffectStep
gatherDamage mv
  | isJust mv.multihit        = Nothing -- Multi-hit moves are handled elsewhere.
  | mv.name == "Sucker Punch" = Nothing
  | mv.name == "Thunderclap"  = Nothing
  | otherwise =
      case basePower mv of
        Just 0  -> Nothing
        Just _  -> Just DoDamage
        Nothing -> Nothing

-- | Applies damage to the specified player slot.
applyDamage :: PlayerSlot -> Int -> BattleState -> BattleState
applyDamage Player1 dmg bs = bs { player1 = applyDamageToActive dmg bs.player1 }
applyDamage Player2 dmg bs = bs { player2 = applyDamageToActive dmg bs.player2 }

-- | Helper function to apply damage and update the lastDamage field.
applyDamageAndUpdateLast :: PlayerSlot -> Int -> BattleState -> BattleState
applyDamageAndUpdateLast slot dmg bs =
  let bs' = applyDamage slot dmg bs
  in  bs' { turnContext  = bs'.turnContext { lastDamage = dmg } }

-- | Updates a player's active Pokémon with new HP after damage.
applyDamageToActive :: Int -> Player BattleM -> Player BattleM
applyDamageToActive dmg p =
  let oldHP = p.active.hp
      newHP = max 0 (oldHP - dmg)
  in p { active = p.active { hp = newHP } }

-- | Calculate and apply damage for a move.
applyDamageForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyDamageForMove mv attackerPokemon defenderPokemon targetSlots =
  applyEffectForMove mv attackerPokemon defenderPokemon targetSlots
    (\mv' attackerPokemon' defenderPokemon' _ _ -> calculateDamage mv' attackerPokemon' defenderPokemon')
    (flip applyDamageAndUpdateLast)
    (\dmg slot bs ->
        let targetName = case slot of
                           Player1 -> bs.player1.active.name
                           Player2 -> bs.player2.active.name
        in logDamage dmg targetName)

