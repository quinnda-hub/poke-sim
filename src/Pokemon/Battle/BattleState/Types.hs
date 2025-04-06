{-# LANGUAGE InstanceSigs #-}

module Pokemon.Battle.BattleState.Types
  ( TurnContext(..)
  , Player(..)
  , PlayerSlot(..)
  , Team(..)
  , MoveModifier
  , applyModifiers
  , buildTeamFromList
  , createPlayer
  )
where

import           Data.Maybe                            (listToMaybe)
import           Pokemon.AI.Types                      (Strategy)
import           Pokemon.Battle.Action.Internal        (Action)
import           Pokemon.Battle.BattleState.PlayerSlot (PlayerSlot (..))
import           Pokemon.Battle.Pokemon.Internal       (Pokemon)
import           Pokemon.Data.Move.Internal            (Move)

-- State of the turn.
data TurnContext = TurnContext
  { attacker      :: PlayerSlot
  , defender      :: PlayerSlot
  , lastDamage    :: Int
  , moveModifiers :: [MoveModifier]
  , player1Action :: Maybe Action
  , player2Action :: Maybe Action
  }

instance Show TurnContext where
  show :: TurnContext -> String
  show (TurnContext a d dmg _ a1 a2) =
    "TurnContext { attacker = "
    ++ show a
    ++ ", defender = "
    ++ show d
    ++ ", lastDamage = "
    ++ show dmg
    ++ ", player1Action = "
    ++ show a1
    ++ ", player2Action = "
    ++ show a2
    ++ " }"

-- A Player has a team of Pokemon and an active Pokemon.
data Player m = Player
  { team     :: Team
  , active   :: Pokemon
  , strategy :: Strategy m
  }

-- | Create a Player from a lead Pokemon, a team, and a strategy.
createPlayer :: Pokemon -> Team -> Strategy m -> Player m
createPlayer lead t strat = Player
  { team   = t
  , active = lead
  , strategy = strat
  }

buildTeamFromList :: [Pokemon] -> Team
buildTeamFromList pokes = Team
  { pokemon1 = listToMaybe pokes
  , pokemon2 = listToMaybe (drop 1 pokes)
  , pokemon3 = listToMaybe (drop 2 pokes)
  , pokemon4 = listToMaybe (drop 3 pokes)
  , pokemon5 = listToMaybe (drop 4 pokes)
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

