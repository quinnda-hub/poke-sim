{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot        #-}

module Pokemon.Battle.BattleState.Internal (BattleM(..),
                                           BattleState(..),
                                           getPokemonForSlot,
                                           pokemonName,
                                           getRNG,
                                           updateRNG) where

import           Control.Monad.State                   (MonadState, get, put)
import           Control.Monad.Trans                   (MonadIO)
import           Control.Monad.Trans.State             (StateT)
import           Control.Monad.Trans.Writer            (WriterT)
import           Control.Monad.Writer.Class            (MonadWriter)
import           Data.Text                             (Text)
import           Pokemon.Battle.BattleState.PlayerSlot (PlayerSlot (..))
import           Pokemon.Battle.BattleState.Types      (Player (..),
                                                        TurnContext)
import           Pokemon.Battle.Logging                (LogMessage)
import           Pokemon.Battle.Pokemon.Internal       (Pokemon (..))
import           Pokemon.Mechanics.Weather             (Weather)
import           System.Random                         (StdGen)

-- A newtype for the BattleM monad.
newtype BattleM a = BattleM
  { runBattleM :: StateT BattleState (WriterT [LogMessage] IO) a
  } deriving (Functor,
              Applicative,
              Monad,
              MonadState BattleState,
              MonadWriter [LogMessage],
              MonadIO)

data BattleState = BattleState
  { turnContext :: TurnContext
  , player1     :: Player BattleM
  , player2     :: Player BattleM
  , turnNumber  :: Int
  , weather     :: Weather
  , rng         :: StdGen
  } deriving (Show)

-- Helper that looks up the Pokemon for a given player slot
-- from the BattleState.
getPokemonForSlot :: PlayerSlot -> BattleState -> Pokemon
getPokemonForSlot playerslot bs
  | playerslot == Player1 = bs.player1.active
  | otherwise             = bs.player2.active

-- | Retrieves the active Pokémon's name for a given PlayerSlot.
pokemonName :: BattleState -> PlayerSlot -> Text
pokemonName bs Player1 = bs.player1.active.name
pokemonName bs Player2 = bs.player2.active.name

--------------------------------------------------------------------------------
-- RNG Helpers
--------------------------------------------------------------------------------

-- | Retrieves the current random number generator from the BattleState.
getRNG :: BattleM StdGen
getRNG = do
    bs <- get
    return bs.rng

-- | Updates the random number generator stored in the BattleState.
updateRNG :: StdGen -> BattleM ()
updateRNG newGen = do
    bs <- get
    put bs { rng = newGen }

