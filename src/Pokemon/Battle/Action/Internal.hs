{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Action.Internal
  ( Action(..)
  , ActionType(..)
  , actorName
  , uniformStrategy
  ) where

import qualified Data.Text                           as T

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (..), getRNG,
                                                      updateRNG)

import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move)
import           System.Random                       (randomR)

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

-- | Represents a battle-level action. In this design each action
--   is associated with the PlayerSlot that is "owning" the action.
data Action = Action
  { actor      :: PlayerSlot  -- ^ The player from whose perspective the move is used.
  , actionType :: ActionType
  } deriving (Show, Eq)

-- Different types of actions. For now, only moves.
data ActionType = MoveAction Move
                | SwitchAction Pokemon deriving (Show, Eq)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Retrieves the active Pokémon's name for a given PlayerSlot.
actorName :: BattleState -> PlayerSlot -> T.Text
actorName bs Player1 = bs.player1.active.name
actorName bs Player2 = bs.player2.active.name

-- | Choose a random action from a list of actions.
-- (Doesn't really belong in this module but can live here for now.)
uniformStrategy :: [Action] -> BattleM Action
uniformStrategy actions = do
  rng0 <- getRNG
  let (idx, newRng) = randomR (0, length actions - 1) rng0
  updateRNG newRng
  return (actions !! idx)

