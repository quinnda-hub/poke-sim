{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Boosts where

import           Data.Maybe                          (fromMaybe)
import           Pokemon.Battle.BattleState          (BattleState (..),
                                                      PlayerSlot (..))
import           Pokemon.Battle.BattleState.Internal (Player (..))
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (..))
import qualified Pokemon.Battle.Pokemon.Internal     as Poke
import           Pokemon.Data.Move.Internal          (Boosts, Move)
import qualified Pokemon.Data.Move.Internal          as Move

gatherBoosts :: Move -> Maybe EffectStep
gatherBoosts mv = case mv.boosts of
  Just _  -> Just BoostStats
  Nothing -> Nothing

clampBoost :: Int -> Int
clampBoost n
  | n < (-6)  = -6
  | n > 6     = 6
  | otherwise = 6

applyBoosts :: PlayerSlot -> Boosts -> BattleState -> BattleState
applyBoosts Player1 b bs = bs { player1 = applyBoostsToActive b bs.player1 }
applyBoosts Player2 b bs = bs { player2 = applyBoostsToActive b bs.player2 }

applyBoostsToActive :: Boosts -> Player -> Player
applyBoostsToActive moveBoosts player =
  let  poke = player.active
       currentBoosts = poke.boosts
       newBoosts = Poke.PokemonBoosts
         { Poke.acc = clampBoost $ currentBoosts.acc + fromMaybe 0 moveBoosts.acc
         , Poke.atk = clampBoost $ currentBoosts.atk + fromMaybe 0 moveBoosts.atk
         , Poke.def = clampBoost $ currentBoosts.def + fromMaybe 0 moveBoosts.def
         , Poke.eva = clampBoost $ currentBoosts.eva + fromMaybe 0 moveBoosts.eva
         , Poke.spa = clampBoost $ currentBoosts.spa + fromMaybe 0 moveBoosts.spa
         , Poke.spd = clampBoost $ currentBoosts.spd + fromMaybe 0 moveBoosts.spd
         , Poke.spe = clampBoost $ currentBoosts.spe + fromMaybe 0 moveBoosts.spe
         }
       newPokemon = poke { Poke.boosts = newBoosts }
  in player { active = newPokemon }
