module Pokemon.Battle.BattleState.Internal where

import           Control.Monad.Trans.State (StateT)
import           Control.Monad.Trans.Writer (WriterT)
import           Data.Vector               (Vector)
import           Data.Text (Text)
import           Pokemon.Battle.Pokemon    (Pokemon)
import           Pokemon.Mechanics.Weather (Weather)
import           System.Random             (StdGen)

type BattleM a = StateT BattleState (WriterT [Text] IO) a 

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

data PlayerSlot 
  = Player1Active 
  | Player2Active
  deriving (Show, Eq)
