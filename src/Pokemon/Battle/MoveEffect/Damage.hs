{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Damage where

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState,
                                                      Player (..),
                                                      PlayerSlot (..), player1,
                                                      player2)
import           Pokemon.Battle.Logging              (logDamage)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (DoDamage),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (basePower))

import           Pokemon.Battle.Damage.Internal      (calculateDamage)

-- | If the move has a base power, produce a damage step.
gatherDamage :: Move -> Maybe EffectStep
gatherDamage mv =
  case basePower mv of
    Just _  -> Just DoDamage
    Nothing -> Nothing

-- | Applies damage to the specified player slot.
applyDamage :: PlayerSlot -> Int -> BattleState -> BattleState
applyDamage Player1 dmg bs = bs { player1 = applyDamageToActive dmg bs.player1 }
applyDamage Player2 dmg bs = bs { player2 = applyDamageToActive dmg bs.player2 }

-- | Updates a player's active Pokémon with new HP after damage.
applyDamageToActive :: Int -> Player -> Player
applyDamageToActive dmg p =
  let oldHP = p.active.hp
      newHP = max 0 (oldHP - dmg)
  in p { active = p.active { hp = newHP } }

-- | Calculate and apply damage for a move.
applyDamageForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyDamageForMove mv attackerPokemon defenderPokemon targetSlots =
  applyEffectForMove mv attackerPokemon defenderPokemon targetSlots
    (\mv' attackerPokemon' defenderPokemon' _ _ -> calculateDamage mv' attackerPokemon' defenderPokemon')
    (flip applyDamage)
    (\dmg slot bs ->
        let targetName = case slot of
                           Player1 -> bs.player1.active.name
                           Player2 -> bs.player2.active.name
        in logDamage dmg targetName)

