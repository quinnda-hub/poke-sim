{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Throughput benchmark: random playouts (uniformly random legal choices) under the sampling
-- interpreter, single-threaded. Teams are taken from trace files.
--
-- Environment: @POKESIM_TRACES@ (default @local/traces/slice0@), @POKESIM_DEX@ (default @data/gen9@),
-- @POKESIM_BENCH_SECONDS@ (default 5, CPU seconds per measurement).
--
-- The teams are those of the set's first 20 traces by name. A packed set (trace-pack) has them in @teams.json@, which is
-- read when present; otherwise they are read from the trace files, v1 JSON or v2.
module Main (main) where

import Data.Aeson (Value (..), eitherDecodeFileStrict')
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import System.CPUTime (getCPUTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import System.Random.SplitMix (mkSMGen)
import Text.Printf (printf)

import Engine
import Engine.Chance (Sample, runSample)
import Trace (Trace (..))
import Trace.V2 (decodeV2)

-- | Play one battle to the end with uniformly random legal choices; returns the decision steps taken.
playout :: Dex -> Team -> Team -> Sample Int
playout dx t1 t2 = do
  b0 <- newBattle dx gen9ou t1 t2
  go b0 0
  where
    go :: Battle -> Int -> Sample Int
    go b !n = case request b of
      Finished _ -> pure n
      Awaiting _ a1 a2 -> do
        c1 <- pick b P1 a1
        c2 <- pick b P2 a2
        r <- step dx b (c1, c2)
        case r of
          Right b' -> go b' (n + 1)
          Left e -> error ("bench: step rejected its input: " ++ show e)
    pick :: Battle -> Player -> Bool -> Sample (Maybe Choice)
    pick _ _ False = pure Nothing
    pick b p True = do
      let cs = legalChoices b p
      i <- roll 0 (length cs)
      pure (case drop i cs of (c : _) -> Just c; [] -> Nothing)

main :: IO ()
main = do
  dir <- fromMaybe "local/traces/slice0" <$> lookupEnv "POKESIM_TRACES"
  dexDir <- fromMaybe "data/gen9" <$> lookupEnv "POKESIM_DEX"
  secs <- maybe 5 read <$> lookupEnv "POKESIM_BENCH_SECONDS"
  exists <- doesDirectoryExist dir
  names <- if exists then listDirectory dir else pure []
  let packedTeams = dir </> "teams.json"
      files = take 20 (sort [dir </> n | n <- names, isTraceFile n])
  hasPacked <- doesFileExist packedTeams
  sources <- if hasPacked then packedSources packedTeams else pure (map (\f -> (f, loadTeamsOf f)) files)
  if null sources then die ("bench: no trace files in " ++ dir) else pure ()
  dex <- loadDex dexDir
  teams <- mapM (\(f, load) -> load >>= \ts -> teamsOf dex f ts) sources
  let matchups = V.fromList teams
      budget = round (secs * 1e12 :: Double) :: Integer
  genRef <- newIORef (mkSMGen 20240601)
  start <- getCPUTime
  let loop :: Int -> Int -> IO (Int, Int)
      loop !i !steps = do
        now <- getCPUTime
        if now - start >= budget
          then pure (i, steps)
          else do
            g <- readIORef genRef
            let (t1, t2) = matchups V.! (i `mod` V.length matchups)
                (n, g') = runSample g (playout dex t1 t2)
            writeIORef genRef g'
            loop (i + 1) (steps + n)
  (battles, steps) <- loop 0 0
  end <- getCPUTime
  let cpu = fromIntegral (end - start) / 1e12 :: Double
  printf "random playouts, 1 core, %d trace team pairs, -O2\n" (length sources)
  printf "battles: %d, decision steps: %d, cpu: %.2fs\n" battles steps cpu
  printf "battles/s: %.1f   decision-steps/s: %.0f\n" (fromIntegral battles / cpu) (fromIntegral steps / cpu)
  where
    -- v1 traces (<name>.json, not the set's list files) and v2 traces
    isTraceFile n = (".json" `isSuffixOf` n && not ("-moves.json" `isSuffixOf` n)) || ".trace" `isSuffixOf` n
    -- the first 20 entries of teams.json: an array of {"trace": name, "teams": teams}
    packedSources f = do
      r <- eitherDecodeFileStrict' f
      case r of
        Left e -> die (f ++ ": " ++ e)
        Right (Array es) -> pure [(f ++ " (" ++ show i ++ ")", pure (teamsEntry e)) | (i, e) <- take 20 (zip [0 :: Int ..] (V.toList es))]
        Right _ -> die (f ++ ": not an array")
    teamsEntry (Object o) = maybe Null id (KM.lookup (K.fromString "teams") o)
    teamsEntry _ = Null
    -- the teams of one trace file, as a JSON array of two
    loadTeamsOf f
      | ".trace" `isSuffixOf` f = do
          r <- decodeV2 <$> BL.readFile f
          either (die . ((f ++ ": ") ++)) (pure . trTeams) r
      | otherwise = do
          r <- eitherDecodeFileStrict' f
          case r of
            Left e -> die (f ++ ": " ++ e)
            Right (Object o) -> pure (maybe Null id (KM.lookup (K.fromString "teams") o))
            Right _ -> die (f ++ ": not an object")
    teamsOf dex f ts = case ts of
      Array xs | [a, b] <- V.toList xs ->
        either (die . ((f ++ ": ") ++)) pure ((,) <$> parseTeam dex a <*> parseTeam dex b)
      _ -> die (f ++ ": no teams")
