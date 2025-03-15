{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Action.MoveAction
  ( validMoveActions
  , applyMoveActionWithActor
  ) where

import           Control.Monad.Trans.State           (get)

import           Pokemon.Battle.BattleState.Internal (BattleM,
                                                      getPokemonForSlot,
                                                      pokemonName)

import           Data.Maybe                          (catMaybes)
import           Pokemon.Battle.Ability              (triggerAbilityOnMoveUse)
import           Pokemon.Battle.Action.Internal      (Action (..),
                                                      ActionType (..))
import           Pokemon.Battle.BattleState.Types    (PlayerSlot (..))
import           Pokemon.Battle.Logging              (logAction)
import           Pokemon.Battle.MoveEffect           (applyMoveWithContext)
import           Pokemon.Battle.Pokemon.Internal     (MoveSet (..),
                                                      Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (name))

-- | Returns valid move actions for the given player slot.
validMoveActions :: PlayerSlot -> BattleM [Action]
validMoveActions p = do
  bs <- get
  let activeMoves    = moves (getPokemonForSlot p bs)
      availableMoves = catMaybes [ Just activeMoves.move1
                                 , activeMoves.move2
                                 , activeMoves.move3
                                 , activeMoves.move4
                                 ]
  return [ Action { actor = p, actionType = MoveAction m } | m <- availableMoves ]

-- | Applies a move action given the actor’s slot.
applyMoveActionWithActor :: PlayerSlot -> Move -> BattleM ()
applyMoveActionWithActor actorSlot mv = do
  -- Determine attacker and defender slots.
  bs <- get
  let (attackerSlot, defenderSlot) = case actorSlot of
        Player1 -> (Player1, Player2)
        Player2 -> (Player2, Player1)
  -- Log the move, trigger abilities, and apply the move effects.
  logAction mv.name (pokemonName bs attackerSlot)
  triggerAbilityOnMoveUse attackerSlot mv
  applyMoveWithContext mv attackerSlot defenderSlot

