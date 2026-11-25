{-# LANGUAGE OverloadedStrings #-}

-- | Prints the pools the engine accepts as JSON, @{"moves":[ids],"species":[ids]}@, both sorted. This is
-- the input of tools/oracle/gen-slice1.js, so the traces are drawn from exactly what the engine supports.
--
-- Usage: engine-pool [dexDir=data/gen9]
--        engine-pool --reasons [dexDir]   how many moves each rejection reason removes (diagnostic)
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
      putStrLn (show (length (supportedMoves dex)) ++ "\tsupported")
    _ -> do
      dex <- loadDex (dirOf args)
      BL.putStrLn (A.encode (object ["moves" .= sort (supportedMoves dex), "species" .= sort (supportedSpecies dex)]))
  where
    dirOf (d : _) = d
    dirOf [] = "data/gen9"
