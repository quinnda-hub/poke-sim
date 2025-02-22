{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.BattleState.Internal (BattleM,
                                           BattleState(..),
                                           Player(..),
                                           PlayerSlot(..),
                                           TurnContext(..),
                                           Team(..),
                                           getPokemonForSlot) where

import           Control.Monad.Trans.State       (StateT)
import           Control.Monad.Trans.Writer      (WriterT)
import           Pokemon.Battle.Logging          (LogMessage)
import           Pokemon.Battle.Pokemon.Internal (Pokemon)
import           Pokemon.Mechanics.Weather       (Weather)
import           System.Random                   (StdGen)

type BattleM a = StateT BattleState (WriterT [LogMessage] IO) a

data BattleState = BattleState
  { turnContext :: TurnContext
  , player1     :: Player
  , player2     :: Player
  , turnNumber  :: Int
  , weather     :: Weather
  , rng         :: StdGen
  } deriving (Show)

-- State of the turn.
data TurnContext = TurnContext
  { attacker :: PlayerSlot
  , defender :: PlayerSlot
  } deriving (Show)

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

-- Helper that looks up the Pokemon for a given player slot
-- from the BattleState.
getPokemonForSlot :: PlayerSlot -> BattleState -> Pokemon 
getPokemonForSlot playerslot bs
  | playerslot == Player1 = bs.player1.active 
  | otherwise             = bs.player2.active 
