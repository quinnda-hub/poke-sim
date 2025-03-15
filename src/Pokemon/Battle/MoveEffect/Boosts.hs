{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Boosts where

import           Data.Maybe                          (fromMaybe)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      PlayerSlot (..))
import           Pokemon.Battle.BattleState.Types    (Player (..))
import           Pokemon.Battle.Logging              (logBoosts)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (..),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon)
import qualified Pokemon.Battle.Pokemon.Internal     as Poke
import           Pokemon.Data.Move.Internal          (Boosts, Move)
import qualified Pokemon.Data.Move.Internal          as Move

-- | If the move boosts the users stats, produce an EffectStep.
gatherBoosts :: Move -> Maybe EffectStep
gatherBoosts mv = case mv.boosts of
  Just _  -> Just BoostStats
  Nothing -> Nothing

-- | Make sure stat boosts don't go out of bounds.
clampBoost :: Int -> Int
clampBoost n
  | n < (-6)  = -6
  | n > 6     = 6
  | otherwise = n

-- | Applies boosts to the specified PlayerSlot.
applyBoosts :: PlayerSlot -> Boosts -> BattleState -> BattleState
applyBoosts Player1 b bs = bs { player1 = applyBoostsToActive b bs.player1 }
applyBoosts Player2 b bs = bs { player2 = applyBoostsToActive b bs.player2 }

-- | Apply boosts to the active Pokemon of the Player.
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

-- | Applies a boost effect to target players by updating their active Pokémon's boosts.
applyBoostsForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyBoostsForMove mv attacker defender targetSlots = do
  applyEffectForMove mv attacker defender targetSlots
    -- Effect function: extract the boost data from the move.
    (\mv' _ _ _ _ -> pure (Move.boosts mv'))
    -- Modifier: update the active Pokémon's boosts for each target slot.
    (\mBoosts slot bs ->
       case mBoosts of
         Just b ->
           case slot of
             Player1 ->
               let updatedPlayer = applyBoostsToActive b bs.player1
               in bs { player1 = updatedPlayer }
             Player2 ->
               let updatedPlayer = applyBoostsToActive b bs.player2
               in bs { player2 = updatedPlayer }
         Nothing -> bs)
    -- Logger: log the applied boost (if any) for each target.
    (\mBoosts slot bs ->
       case mBoosts of
         Just b ->
           let (targetName, newBoosts) = case slot of
                                           Player1 -> (bs.player1.active.name, bs.player1.active.boosts)
                                           Player2 -> (bs.player2.active.name, bs.player2.active.boosts)
           in logBoosts targetName b newBoosts
         Nothing -> pure ())

