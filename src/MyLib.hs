{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module MyLib (someFunc, someFoo, someDemo) where

import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as T
import           System.IO                       (hPutStrLn, stderr)

import           Pokemon.Battle.Pokemon.Internal (MoveSet (..), Nature (..),
                                                  mkEVs, mkIVs, mkPokemon)
import           Pokemon.Data.Move.Internal      (loadMoves, showLoadMovesError)
import           Pokemon.Data.Pokedex.Internal   (loadPokedex,
                                                  showLoadPokedexError)

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
        Nothing   -> putStrLn "Move 'closecombat' not found."

someFoo :: IO ()
someFoo = do
  putStrLn "Reading pokedex.json..."
  result <- loadPokedex "data/pokedex.json"
  case result of
    Left err -> hPutStrLn stderr $ showLoadPokedexError err
    Right movesMap -> do
      let movesMapLower = Map.mapKeys T.toLower movesMap
      case Map.lookup "ogerpon" movesMapLower of
        Just move -> print move
        Nothing   -> putStrLn "Move 'closecombat' not found."

someDemo :: IO ()
someDemo = do
  putStrLn "Reading moves.json..."
  movesResult <- loadMoves "data/moves.json"
  case movesResult of
    Left err -> hPutStrLn stderr $ "Error loading moves: " <> showLoadMovesError err
    Right movesMapRaw -> do
      let movesMapLower = Map.mapKeys T.toLower movesMapRaw

      putStrLn "Reading pokedex.json..."
      pokedexResult <- loadPokedex "data/pokedex.json"
      case pokedexResult of
        Left err -> hPutStrLn stderr $ "Error loading pokedex: " <> showLoadPokedexError err
        Right pokedexMapRaw -> do
          let pokedexMapLower = Map.mapKeys T.toLower pokedexMapRaw

          -- We'll try to construct a level 100 Ogerpon with four arbitrary moves.
          let pokemonName = "garchomp"
              desiredMoves = ["earthquake", "stoneedge", "scaleshot", "swordsdance"]

          case Map.lookup pokemonName pokedexMapLower of
            Nothing -> putStrLn $ "Pokemon '" <> T.unpack pokemonName <> "' not found."
            Just pmon -> do
              -- Attempt to find the four moves we want:
              maybeMoves <- mapM (\mname -> pure (mname, Map.lookup mname movesMapLower)) desiredMoves

              let missingMoves = [mn | (mn, Nothing) <- maybeMoves]
              if not (null missingMoves)
                then putStrLn $ "Some moves not found: " <> show missingMoves
                else case [m | (_, Just m) <- maybeMoves] of
                  [move1, move2, move3, move4] -> do
                    -- Arbitrary IVs: max IVs
                    let ivs = case mkIVs 31 31 31 31 31 31 of
                          Just ivs' -> ivs'
                          Nothing   -> error "Invalid IVs configuration."

                    -- Arbitrary EVs: a common competitive spread
                    let evs = case mkEVs 252 0 252 0 4 0 of
                          Just evs' -> evs'
                          Nothing   -> error "Invalid EVs configuration."

                    -- Arbitrary nature: Jolly
                    let chosenNature = Bold
                    -- Level 100
                    let lvl = 100

                    let moveset = MoveSet
                          { move1 = move1
                          , move2 = Just move2
                          , move3 = Just move3
                          , move4 = Just move4
                          }

                    -- Construct the Pokemon
                    let pokemon = mkPokemon pmon lvl moveset chosenNature ivs evs

                    putStrLn "Constructed the following Pokemon:"
                    print pokemon
                  _ -> putStrLn "Unexpected error: move lookup resulted in an incomplete set of moves."
