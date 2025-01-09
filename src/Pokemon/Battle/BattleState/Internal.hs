module Pokemon.Battle.BattleState.Internal where

import           Data.Vector               (Vector)
import           Pokemon.Battle.Pokemon    (Pokemon)
import           Pokemon.Mechanics.Weather (Weather)
import           System.Random             (StdGen)

data BattleState = BattleState
  { player1    :: Player
  , player2    :: Player
  , turnNumber :: Int
  , weather    :: Weather
  , rng        :: StdGen
  } deriving (Show)

data Player = Player
  { team   :: Vector Pokemon
  , active :: Pokemon
  } deriving (Show)
