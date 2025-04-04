{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.RecoilDamage where

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      player1, player2)
import           Pokemon.Battle.BattleState.Types    (Player (..),
                                                      PlayerSlot (..),
                                                      TurnContext (..))
import           Pokemon.Battle.Logging              (logRecoil)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (RecoilDamage),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (recoil),
                                                      Recoil (..))

-- | If the move has recoil, produce a recoil effect step.
gatherRecoil :: Move -> Maybe EffectStep
gatherRecoil mv =
  case mv.recoil of
    Nothing -> Nothing
    Just _  -> Just RecoilDamage

-- Calculate recoil based on damage dealt.
calculateRecoil :: Int -> Int -> Int -> Int
calculateRecoil dmg num denom = (dmg * num) `div` denom

-- | Helper function to update a player's active Pokemon with recoil damage.
applyRecoilToActive :: Int -> Player BattleM -> Player BattleM
applyRecoilToActive recoilDmg p =
  let oldHP = p.active.hp
      newHP = max 0 (oldHP - recoilDmg)
  in p { active = p.active { hp = newHP } }

-- | Applies recoil damage to the appropriate player.
applyRecoil :: PlayerSlot -> Int -> BattleState -> BattleState
applyRecoil Player1 recoilDmg bs = bs { player1 = applyRecoilToActive recoilDmg bs.player1 }
applyRecoil Player2 recoilDmg bs = bs { player2 = applyRecoilToActive recoilDmg bs.player2 }

applyRecoilForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyRecoilForMove mv attackerPokemon defenderPokemon targetSlots =
  applyEffectForMove mv attackerPokemon defenderPokemon targetSlots
    (\mv' _ _ _ bs ->
      case mv'.recoil of
        Just (Recoil num denom) -> return (calculateRecoil bs.turnContext.lastDamage num denom)
        Nothing -> return 0)
    (flip applyRecoil)
    (\recoilDmg slot bs ->
      let attackerName = case slot of
                           Player1 -> bs.player1.active.name
                           Player2 -> bs.player2.active.name
      in logRecoil recoilDmg attackerName)

