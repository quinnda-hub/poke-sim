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

import           Pokemon.AI.Types                      (Strategy)
import           Pokemon.Battle.BattleState.PlayerSlot (PlayerSlot (..))
import           Pokemon.Battle.Pokemon.Internal       (Pokemon)
import           Pokemon.Data.Move.Internal            (Move)


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
data Player m = Player
  { team     :: Team
  , active   :: Pokemon
  , strategy :: Strategy m
  }

instance Show (Player m) where
  show (Player { team = t, active = a, strategy = _ }) =
    "Player { team = " ++ show t ++
    ", active = " ++ show a ++
    ", strategy = <strategy> }"

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

