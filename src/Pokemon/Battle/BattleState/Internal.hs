module Pokemon.Battle.BattleState.Internal (BattleM,
                                           BattleState(..),
                                           Player(..),
                                           PlayerSlot(..),
                                           TurnContext(..)) where

import           Control.Monad.Trans.State       (StateT)
import           Control.Monad.Trans.Writer      (WriterT)
import           Data.Vector                     (Vector)
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

data TurnContext = TurnContext
  { attacker :: PlayerSlot
  , defender :: PlayerSlot
  } deriving (Show)

data Player = Player
  { team   :: Vector Pokemon
  , active :: Pokemon
  } deriving (Show)

data PlayerSlot
  = Player1
  | Player2
  deriving (Show, Eq)
