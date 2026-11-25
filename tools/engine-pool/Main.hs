{-# LANGUAGE OverloadedStrings #-}

-- | Prints the pools the engine accepts as JSON, @{"moves":[ids],"species":[ids],"abilities":[ids],"items":[ids]}@,
-- each sorted (the abilities include @noability@, the items @\"\"@ for none). This is the input of
-- tools/oracle/gen-slice1.js and gen-slice2.js, so the traces are drawn from exactly what the engine supports.
--
-- Usage: engine-pool [dexDir=data/gen9]
--        engine-pool --reasons [dexDir]   how many moves, abilities and items each rejection reason removes (diagnostic)
module Main (main) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (sort, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Engine.Dex
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--reasons" : rest) -> do
      dex <- loadDex (dirOf rest)
      let counts = M.fromListWith (+) [(why, 1 :: Int) | m <- V.toList (dxMoves dex), Just why <- [mvReject m]]
          notExec = length [() | m <- V.toList (dxMoves dex), mvReject m == Nothing, not (mvSupported m)]
      mapM_ (\(why, n) -> TIO.putStrLn (T.pack (show n) <> "\t" <> why)) (sortBy (comparing (Down . snd)) (M.toList counts))
      putStrLn (show notExec ++ "\ttranslated but not executable in this phase")
      putStrLn (show (length (supportedMoves dex)) ++ "\tmoves supported")
      let effectCounts what table = do
            let cs = M.fromListWith (+) [(why, 1 :: Int) | c <- V.toList table, Just why <- [cnReject c]]
            mapM_ (\(why, n) -> TIO.putStrLn (T.pack (show n) <> "\t" <> what <> ": " <> why)) (sortBy (comparing (Down . snd)) (M.toList cs))
      effectCounts "abilities" (dxAbilities dex)
      putStrLn (show (length (supportedAbilities dex)) ++ "\tabilities supported")
      effectCounts "items" (dxItems dex)
      putStrLn (show (length (supportedItems dex) - 1) ++ "\titems supported (besides none)")
    _ -> do
      dex <- loadDex (dirOf args)
      BL.putStrLn (A.encode (object
        [ "moves" .= sort (supportedMoves dex), "species" .= sort (supportedSpecies dex)
        , "abilities" .= sort (supportedAbilities dex), "items" .= sort (supportedItems dex) ]))
  where
    dirOf (d : _) = d
    dirOf [] = "data/gen9"
