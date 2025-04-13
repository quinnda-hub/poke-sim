{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module MyLib
  ( someFunc
  , someFoo
  , someDemoBattle
  , someDemoBattle2) where

import           Control.Monad.Trans.State           (evalStateT)
import           Control.Monad.Trans.Writer          (runWriterT)
import qualified Data.Map.Strict                     as Map
import qualified Data.Text                           as T
import           System.IO                           (hPutStrLn, stderr)
import           System.Random                       (newStdGen)

import           Pokemon.Battle.Pokemon.Internal     (MoveSet (..), Nature (..),
                                                      mkEVs, mkIVs, mkPokemon)

import           Pokemon.Data.Move.Internal          (loadMoves,
                                                      showLoadMovesError)

import           Pokemon.Data.Pokedex.Internal       (loadPokedex,
                                                      showLoadPokedexError)

import           Pokemon.Battle.BattleState.Types    (Player (Player, active, strategy, team),
                                                      PlayerSlot (Player1, Player2),
                                                      Team (Team, pokemon1, pokemon2, pokemon3, pokemon4, pokemon5),
                                                      TurnContext (..))
-- Battle state types and simulation
import           Pokemon.AI.Internal                 (preferAttackStrategy)
import           Pokemon.Battle.BattleState.Internal (BattleState (..),
                                                      runBattleM)
import           Pokemon.Battle.Simulation.Internal  (simulateBattle)

import           Pokemon.Battle.Simulation.Modes     (simulateBattleFromFiles)
import           Pokemon.Mechanics.Weather           (Weather (..))
import           Pokemon.Pretty                      (printLogMessages,
                                                      printPokemon)

--------------------------------------------------------------------------------
-- someFunc and someFoo (existing demos)
--------------------------------------------------------------------------------

someFunc :: IO ()
someFunc = do
  putStrLn "Reading moves.json..."
  result <- loadMoves "data/moves.json"
  case result of
    Left err -> hPutStrLn stderr $ showLoadMovesError err
    Right movesMap -> do
      let movesMapLower = Map.mapKeys T.toLower movesMap
      case Map.lookup "lowkick" movesMapLower of
        Just move -> print move
        Nothing   -> putStrLn "Move 'lowkick' not found."

someFoo :: IO ()
someFoo = do
  putStrLn "Reading pokedex.json..."
  result <- loadPokedex "data/pokedex.json"
  case result of
    Left err -> hPutStrLn stderr $ showLoadPokedexError err
    Right pokedexMap -> do
      let pokedexMapLower = Map.mapKeys T.toLower pokedexMap
      case Map.lookup "ogerpon" pokedexMapLower of
        Just pmon -> print pmon
        Nothing   -> putStrLn "Pokemon 'ogerpon' not found."

--------------------------------------------------------------------------------
-- someDemoBattle: A Demo 1v1 Battle Between Fuecoco and Sprigatito
--------------------------------------------------------------------------------

-- | Simulates a battle between a level 5 Fuecoco and a level 5 Sprigatito.
--   Both Pokémon have perfect IVs (31 in all stats) and no EVs.
someDemoBattle :: IO ()
someDemoBattle = do
  putStrLn "Loading moves.json..."
  movesResult <- loadMoves "data/moves.json"
  case movesResult of
    Left err -> hPutStrLn stderr $ "Error loading moves: " <> showLoadMovesError err
    Right movesMapRaw -> do
      let movesMap = Map.mapKeys T.toLower movesMapRaw

      putStrLn "Loading pokedex.json..."
      pokedexResult <- loadPokedex "data/pokedex.json"
      case pokedexResult of
        Left err -> hPutStrLn stderr $ "Error loading pokedex: " <> showLoadPokedexError err
        Right pokedexMapRaw -> do
          let pokedexMap = Map.mapKeys T.toLower pokedexMapRaw

          -- Lookup Fuecoco, Sprigatito, and Wattrel in the pokedex.
          case ( Map.lookup "fuecoco" pokedexMap
               , Map.lookup "sprigatito" pokedexMap
               , Map.lookup "wattrel" pokedexMap
               ) of
            (Nothing, _, _) ->
              putStrLn "Fuecoco not found in pokedex."
            (_, Nothing, _) ->
              putStrLn "Sprigatito not found in pokedex."
            (_, _, Nothing) ->
              putStrLn "Wattrel not found in pokedex."
            (Just fuecocoData, Just sprigatitoData, Just wattrelData) -> do

              -- Define desired moves for each Pokémon.
              let fuecocoMovesDesired    = ["leer", "bite", "willowisp", "tackle"]
                  sprigatitoMovesDesired = ["honeclaws", "growl", "scratch", "leafage"]
                  wattrelMovesDesired    = ["peck", "growl"]

              -- Lookup moves for Fuecoco.
              fuecocoMaybeMoves <- mapM (\mname -> pure (mname, Map.lookup mname movesMap))
                                          fuecocoMovesDesired
              let missingFuecoco = [ mn | (mn, Nothing) <- fuecocoMaybeMoves ]
              if not (null missingFuecoco)
                then putStrLn $ "Missing moves for Fuecoco: " <> show missingFuecoco
                else do
                  let [fuecocoMove1, fuecocoMove2, fuecocoMove3, fuecocoMove4] =
                        [ m | (_, Just m) <- fuecocoMaybeMoves ]
                      fuecocoMoveSet = MoveSet
                        { move1 = fuecocoMove1
                        , move2 = Just fuecocoMove2
                        , move3 = Just fuecocoMove3
                        , move4 = Just fuecocoMove4
                        }

                  -- Lookup moves for Sprigatito.
                  sprigatitoMaybeMoves <- mapM (\mname -> pure (mname, Map.lookup mname movesMap))
                                                  sprigatitoMovesDesired
                  let missingSprigatito = [ mn | (mn, Nothing) <- sprigatitoMaybeMoves ]
                  if not (null missingSprigatito)
                    then putStrLn $ "Missing moves for Sprigatito: " <> show missingSprigatito
                    else do
                      let [sprigatitoMove1, sprigatitoMove2, sprigatitoMove3, sprigatitoMove4] =
                            [ m | (_, Just m) <- sprigatitoMaybeMoves ]
                          sprigatitoMoveSet = MoveSet
                            { move1 = sprigatitoMove1
                            , move2 = Just sprigatitoMove2
                            , move3 = Just sprigatitoMove3
                            , move4 = Just sprigatitoMove4
                            }

                      -- Lookup moves for Wattrel.
                      wattrelMaybeMoves <- mapM (\mname -> pure (mname, Map.lookup mname movesMap))
                                                  wattrelMovesDesired
                      let missingWattrel = [ mn | (mn, Nothing) <- wattrelMaybeMoves ]
                      if not (null missingWattrel)
                        then putStrLn $ "Missing moves for Wattrel: " <> show missingWattrel
                        else do
                          let [wattrelMove1, wattrelMove2] =
                                [ m | (_, Just m) <- wattrelMaybeMoves ]
                              wattrelMoveSet = MoveSet
                                { move1 = wattrelMove1
                                , move2 = Just wattrelMove2
                                , move3 = Nothing
                                , move4 = Nothing
                                }

                          -- Perfect IVs (all 31) and no EVs.
                          let ivs = case mkIVs 31 31 31 31 31 31 of
                                      Just ivs' -> ivs'
                                      Nothing   -> error "Invalid IVs configuration."
                              evs = case mkEVs 0 0 0 0 0 0 of
                                      Just evs' -> evs'
                                      Nothing   -> error "Invalid EVs configuration."

                          -- Choose a nature (for demo, we use Hardy).
                          let chosenNature = Hardy

                          -- Level 5 for both.
                          let lvl = 5

                          -- Construct the Pokémon.
                          let fuecocoPokemon    = mkPokemon fuecocoData lvl "Blaze" fuecocoMoveSet chosenNature ivs evs
                              sprigatitoPokemon = mkPokemon sprigatitoData lvl "Overgrow" sprigatitoMoveSet chosenNature ivs evs
                              wattrelPokemon    = mkPokemon wattrelData 3 "Volt Absorb" wattrelMoveSet chosenNature ivs evs

                          putStrLn "Constructed Pokémon:"
                          putStrLn "Fuecoco:"
                          printPokemon fuecocoPokemon
                          putStrLn "Sprigatito:"
                          printPokemon sprigatitoPokemon
                          putStrLn "Wattrel:"
                          printPokemon wattrelPokemon

                          -- Construct an initial BattleState.
                          rng0 <- newStdGen
                          let initBattleState = BattleState
                                { turnContext = TurnContext { attacker = Player1
                                                            , defender = Player2
                                                            , lastDamage = 0
                                                            , moveModifiers = []
                                                            , player1Action = Nothing
                                                            , player2Action = Nothing}
                                , player1     = Player  { team = Team { pokemon1 = Nothing, pokemon2 = Nothing, pokemon3 = Nothing, pokemon4 = Nothing, pokemon5 = Nothing }, active = fuecocoPokemon, strategy = preferAttackStrategy }
                                , player2     = Player { team = Team { pokemon1 = Just wattrelPokemon, pokemon2 = Nothing, pokemon3 = Nothing, pokemon4 = Nothing, pokemon5 = Nothing }, active = sprigatitoPokemon, strategy = preferAttackStrategy }
                                , turnNumber  = 1
                                , weather     = Clear
                                , rng         = rng0
                                , delayedMoves = []
                                }

                          putStrLn "Starting battle simulation..."
                          ((), finalLogs) <- runWriterT (evalStateT (runBattleM (simulateBattle initBattleState)) initBattleState)
                          printLogMessages finalLogs

-- | Run a battle from a PokePaste file.
someDemoBattle2 :: IO ()
someDemoBattle2 = do
  movesResult <- loadMoves "data/moves.json"
  case movesResult of
    Left err -> hPutStrLn stderr $ "Error loading moves: " <> showLoadMovesError err
    Right movesMap -> do

      putStrLn "Loading pokedex.json..."
      pokedexResult <- loadPokedex "data/pokedex.json"
      case pokedexResult of
        Left err -> hPutStrLn stderr $ "Error loading pokedex: " <> showLoadPokedexError err
        Right pokedexMap -> do
          simulateBattleFromFiles movesMap pokedexMap "data/rillaboom.txt" "data/kingambit.txt"

