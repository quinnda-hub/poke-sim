module Pokemon.Battle.Simulation.Modes
  ( simulateBattleFromPlayers
  , simulateBattleFromFiles
  ) where

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      runBattleM)

import           Pokemon.Battle.BattleState.Types    (Player, PlayerSlot (..),
                                                      TurnContext (..),
                                                      buildTeamFromList,
                                                      createPlayer)

import           Control.Monad.Trans.State           (evalStateT)
import           Control.Monad.Trans.Writer          (runWriterT)
import           Pokemon.Battle.Simulation.Internal  (simulateBattle)
import           System.Random                       (newStdGen)

import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Except          (runExceptT, throwE)
import           Data.Map.Strict                     (Map)
import           Data.Text                           (Text)
import           Pokemon.AI.Internal                 (preferAttackStrategy)
import           Pokemon.Convert                     (convertTeam)
import           Pokemon.Data.Move                   (Move)
import           Pokemon.Data.Pokedex                (PokedexMon)
import           Pokemon.Mechanics.Weather           (Weather (..))
import           Pokemon.Parse.PokePaste             (parsePokePasteFile)
import           System.IO

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

-- | Given moves and pokédex maps (with lowercase keys), plus file paths for two teams,
-- parse and convert the teams, create players, and simulate the battle.
simulateBattleFromFiles
  :: Map Text Move       -- ^ Moves map.
  -> Map Text PokedexMon -- ^ Pokédex map.
  -> FilePath            -- ^ File path for Player 1's team.
  -> FilePath            -- ^ File path for Player 2's team.
  -> IO ()
simulateBattleFromFiles movesMap pokedexMap team1File team2File = do
  result <- runExceptT $ do
    parsedTeam1Res <- liftIO $ parsePokePasteFile team1File
    parsedTeam1 <- either throwE return parsedTeam1Res
    parsedTeam2Res <- liftIO $ parsePokePasteFile team2File
    parsedTeam2 <- either throwE return parsedTeam2Res
    team1 <- either (throwE . show) return $ convertTeam pokedexMap movesMap parsedTeam1
    team2 <- either (throwE . show) return $ convertTeam pokedexMap movesMap parsedTeam2
    let p1 = createPlayer (head team1) (buildTeamFromList $ drop 1 team1) preferAttackStrategy
        p2 = createPlayer (head team2) (buildTeamFromList $ drop 1 team2) preferAttackStrategy
    return (p1, p2)
  case result of
    Left err       -> hPutStrLn stderr err
    Right (p1, p2) -> simulateBattleFromPlayers p1 p2

