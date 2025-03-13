{-# LANGUAGE DuplicateRecordFields #-}


module Pokemon.Battle.Action.Internal
  ( Action(..)
  , ActionType(..)
  ) where


import           Pokemon.Battle.BattleState.Internal (PlayerSlot (..))

import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move)

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

