module Pokemon.Battle.Simulation.Modes where

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      runBattleM)

import           Pokemon.Battle.BattleState.Types    (Player, PlayerSlot (..),
                                                      TurnContext (..))

import           Control.Monad.Trans.State           (evalStateT)
import           Control.Monad.Trans.Writer          (runWriterT)
import           Pokemon.Battle.Simulation.Internal  (simulateBattle)
import           System.Random                       (newStdGen)

import           Pokemon.Mechanics.Weather           (Weather (..))

-- | Simulate a battle given two pre-constructed players and their team of Pokemon.
simulateBattleFromPlayers :: Player BattleM -> Player BattleM -> IO ()
simulateBattleFromPlayers p1 p2 = do
  rng0 <- newStdGen
  let initialBattleState = BattleState
        { turnContext = TurnContext { attacker = Player1, defender = Player2, moveModifiers = [] }
        , player1     = p1
        , player2     = p2
        , turnNumber  = 1
        , weather     = Clear
        , rng         = rng0
        }
  ((), _) <- runWriterT (evalStateT (runBattleM (simulateBattle initialBattleState)) initialBattleState)
  return ()

