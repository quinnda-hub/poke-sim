{-# LANGUAGE InstanceSigs #-}
module Pokemon.Battle.BattleState.Types
  ( TurnContext(..)
  , Player(..)
  , PlayerSlot(..)
  , Team(..)
  , MoveModifier
  , applyModifiers
  )
where
import           Pokemon.Battle.Pokemon.Internal (Pokemon)
import           Pokemon.Data.Move.Internal      (Move)

-- State of the turn.
data TurnContext = TurnContext
  { attacker      :: PlayerSlot
  , defender      :: PlayerSlot
  , moveModifiers :: [MoveModifier]
  }

instance Show TurnContext where
  show :: TurnContext -> String
  show (TurnContext a d _) =
    "TurnContext { attacker = " ++ show a ++ ", triggers = " ++ show d ++ " }"

-- A Player has a team of Pokemon and an active Pokemon.
data Player = Player
  { team   :: Team
  , active :: Pokemon
  } deriving (Show)

-- Identifier for players.
data PlayerSlot
  = Player1
  | Player2
  deriving (Show, Eq)

-- Standard team in which you need at least one Pokemon
-- but no more than five (plus the active Pokemon).
data Team = Team
  { pokemon1 :: Maybe Pokemon
  , pokemon2 :: Maybe Pokemon
  , pokemon3 :: Maybe Pokemon
  , pokemon4 :: Maybe Pokemon
  , pokemon5 :: Maybe Pokemon
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Move Modifiers
--------------------------------------------------------------------------------

type MoveModifier = Move -> Move

applyModifiers :: [MoveModifier] -> Move -> Move
applyModifiers mods move = foldr ($) move mods

