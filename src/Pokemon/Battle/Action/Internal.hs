{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Action.Internal (actorName, applyActions, resolveActions) where

import           Control.Monad.Trans.State           (get)
import qualified Data.Text                           as T

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (..))

import           Data.List                           (sortBy)
import           Pokemon.Battle.Logging              (logAction)
import           Pokemon.Battle.MoveEffect.Internal  (applyMoveWithContext)
import           Pokemon.Battle.Pokemon.Internal     (MoveSet (..),
                                                      Pokemon (..), isFainted)
import           Pokemon.Data.Move.Internal          (Move (name), priority)

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

-- | Represents a battle-level action. In this design each action
--   is associated with the PlayerSlot that is "owning" the action.
data Action = Action
  { actor      :: PlayerSlot  -- ^ The player from whose perspective the move is used.
  , actionType :: ActionType
  } deriving (Show)

-- Different types of actions. For now, only moves.
newtype ActionType = MoveAction Move deriving (Show)

-- | Resolves actions for the turn, associating each move with its originating PlayerSlot.
resolveActions :: BattleState -> [Action]
resolveActions bs =
  let act1 = Action { actor = Player1, actionType = MoveAction bs.player1.active.moves.move1 }
      act2 = Action { actor = Player2, actionType = MoveAction bs.player2.active.moves.move1 }
      actions = [act1, act2]
  in sortBy compareActions actions
  where
    compareActions :: Action -> Action -> Ordering
    compareActions (Action { actionType = MoveAction m1, actor = a1 })
                   (Action { actionType = MoveAction m2, actor = a2 }) =
      let p1 = m1.priority
          p2 = m2.priority
      in if p1 /= p2 then compare (negate p1) (negate p2)
         else
           let spd1 = case a1 of
                        Player1 -> bs.player1.active.spe
                        Player2 -> bs.player2.active.spe
               spd2 = case a2 of
                        Player1 -> bs.player1.active.spe
                        Player2 -> bs.player2.active.spe
           in compare (negate spd1) (negate spd2)

--------------------------------------------------------------------------------
-- Applying Actions
--------------------------------------------------------------------------------

applyActions :: [Action] -> BattleM ()
applyActions [] = return ()
applyActions (act:acts) = do
  applyAction act
  bs <- get
  if isFainted bs.player1.active || isFainted bs.player2.active
     then return ()  -- Stop processing further actions.
     else applyActions acts

-- | Applies a simulation-level action.
applyAction :: Action -> BattleM ()
applyAction (Action { actor = a, actionType = MoveAction mv }) =
  applyMoveActionWithActor a mv

-- | Applies a move action given the actor’s slot. This function sets up a local context:
--   the attacker is the given slot and the defender is the opponent.
applyMoveActionWithActor :: PlayerSlot -> Move -> BattleM ()
applyMoveActionWithActor actorSlot mv = do
  bs <- get
  let (attackerSlot, defenderSlot) = case actorSlot of
        Player1 -> (Player1, Player2)
        Player2 -> (Player2, Player1)
  logAction mv.name (actorName bs attackerSlot)
  applyMoveWithContext mv attackerSlot defenderSlot

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Retrieves the active Pokémon's name for a given PlayerSlot.
actorName :: BattleState -> PlayerSlot -> T.Text
actorName bs Player1 = bs.player1.active.name
actorName bs Player2 = bs.player2.active.name
