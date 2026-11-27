{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Parity test: replay recorded Pokemon Showdown battles (traces) through the engine and require the
-- same random draws (count, ranges, order) and the same state at every decision point.
--
-- Environment: @POKESIM_TRACES@ (default @local/traces@, a directory of trace-set directories), @POKESIM_DEX@ (default @data/gen9@).
module Main (main) where

import Control.Concurrent (forkIO, getNumCapabilities, setNumCapabilities)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (filterM, forM, forM_, replicateM, replicateM_, when)
import Data.Aeson (Value (..), eitherDecodeFileStrict')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.IORef
import Data.List (isSuffixOf, nub, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import GHC.Conc (getNumProcessors)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import System.IO (hFlush, hPutStrLn, stderr)

import Engine
import Engine.Chance
import Engine.Dex
import Engine.State

import Compare
import Trace
import Trace.V2
-- | A parity failure: where it happened, turn, draw index, message.
data Failure = Failure String (Maybe Int) Int String

main :: IO ()
main = do
  root <- fromMaybe "local/traces" <$> lookupEnv "POKESIM_TRACES"
  dexDir <- fromMaybe "data/gen9" <$> lookupEnv "POKESIM_DEX"
  exists <- doesDirectoryExist root
  entries <- if exists then sort <$> listDirectory root else pure []
  sets <- filterM (doesDirectoryExist . (root </>)) entries
  when (null sets) $ do
    hPutStrLn stderr $
      "parity: no trace sets found in " ++ show root ++ " (one directory per set).\n"
        ++ "Generate them with:  node tools/oracle/gen-slice0.js local/traces/slice0 500\n"
        ++ "and:  cabal run -v0 engine-pool > local/traces/slice1-pool.json\n"
        ++ "      node tools/oracle/gen-slice1.js local/traces/slice1 600 local/traces/slice1-pool.json"
    exitFailure
  tier <- lookupEnv "POKESIM_TIER" >>= \t -> case t of
    Nothing -> pure "long"
    Just "long" -> pure "long"
    Just "fast" -> pure "fast"
    Just o -> hPutStrLn stderr ("parity: POKESIM_TIER must be fast or long, got " ++ show o) >> exitFailure
  dex <- loadDex dexDir
  -- Every set lists the moves, species, abilities and items it draws from (<set>-moves.json,
  -- <set>-species.json, <set>-abilities.json, <set>-items.json). A set without the last two draws only "No
  -- Ability" and no item. Each list must be accepted by the engine, and together the lists must cover
  -- everything the engine accepts, so that every accepted move, species, ability and item has been tested.
  lists <- forM sets $ \set -> do
    ms <- readIdList (root </> set </> (set ++ "-moves.json"))
    ss <- readIdList (root </> set </> (set ++ "-species.json"))
    as <- readIdListOr ["noability"] (root </> set </> (set ++ "-abilities.json"))
    is <- readIdListOr [""] (root </> set </> (set ++ "-items.json"))
    pure (set, ms, ss, as, is)
  checkPool "move" (supportedMoves dex) [(set, ms) | (set, ms, _, _, _) <- lists]
  checkPool "species" (supportedSpecies dex) [(set, ss) | (set, _, ss, _, _) <- lists]
  checkPool "ability" (supportedAbilities dex) [(set, as) | (set, _, _, as, _) <- lists]
  checkPool "item" (supportedItems dex) [(set, is) | (set, _, _, _, is) <- lists]
  counts <- newIORef []
  work <- fmap concat $ forM sets $ \set -> do
    names <- listDirectory (root </> set)
    let jsons = [n | n <- names, isTraceName ".json" n]
        packed = [n | n <- names, isTraceName ".trace" n]
    when (not (null jsons) && not (null packed)) $ do
      hPutStrLn stderr
        ("parity: trace set " ++ show set ++ " holds both v1 .json traces (" ++ show (length jsons) ++ ") and v2 .trace traces ("
          ++ show (length packed) ++ "); a set must hold one format")
      exitFailure
    -- sorted by name without its extension, so the fast tier samples the same traces in either format
    let files = [root </> set </> n | (_, n) <- sort [(dropExtension n, n) | n <- jsons ++ packed]]
    when (null files) $ do
      hPutStrLn stderr ("parity: trace set " ++ show set ++ " has no traces")
      exitFailure
    -- fast tier: the traces at index i with i mod 10 == 0 of the sorted list; long: all of them
    let sampled = if tier == "fast" then [f | (i, f) <- zip [0 :: Int ..] files, i `mod` 10 == 0] else files
    modifyIORef' counts ((set, (length sampled, length files)) :)
    pure [(set, f) | f <- sampled]
  -- Traces are independent, so they are checked on several threads (POKESIM_JOBS; see 'parityJobs');
  -- the results, and any failure messages, come back in the same order as a sequential run.
  jobs <- parityJobs
  putStrLn ("parity: checking traces on " ++ show jobs ++ " thread(s)")
  results <- inOrder jobs (\(_, f) -> runFile dex f) work $ \(set, f) (r, used) -> do
    case r of
      Left e -> do
        hPutStrLn stderr (render f e)
        hFlush stderr
      Right _ -> pure ()
    pure (set, r, used)
  -- Long tier: every move, species, ability and item the engine accepts must appear in some trace's teams, not only in a
  -- pool list. This is a check over the whole corpus, so the fast tier, which reads a sample, cannot make it.
  coverageOk <- if tier == "fast"
    then putStrLn "parity: trace coverage of the pool is checked in the long tier only" >> pure True
    else do
      let used f = Set.unions [f u | (_, _, u) <- results]
          usedMoves = used usedMovesOf
          usedSpecies = used usedSpeciesOf
          usedAbilities = used usedAbilitiesOf
          usedItems = used usedItemsOf
          unusedMoves = Set.toList (Set.fromList (supportedMoves dex) `Set.difference` usedMoves)
          unusedSpecies = Set.toList (Set.fromList (supportedSpecies dex) `Set.difference` usedSpecies)
          unusedAbilities = Set.toList (Set.fromList (supportedAbilities dex) `Set.difference` usedAbilities)
          unusedItems = Set.toList (Set.fromList (supportedItems dex) `Set.difference` usedItems)
      forM_ [("moves", unusedMoves), ("species", unusedSpecies), ("abilities", unusedAbilities), ("items", unusedItems)] $ \(w, xs) ->
        when (not (null xs)) $
          hPutStrLn stderr ("parity: the engine accepts " ++ w ++ " no trace's teams use (untested): " ++ show xs)
      putStrLn ("parity: trace coverage: " ++ show (Set.size usedMoves) ++ " moves, " ++ show (Set.size usedSpecies)
        ++ " species, " ++ show (Set.size usedAbilities) ++ " abilities and " ++ show (Set.size usedItems)
        ++ " items used by the traces' teams")
      pure (all null [unusedMoves, unusedSpecies, unusedAbilities, unusedItems])
  forM_ sets $ \set -> do
    let rs = [r | (s, r, _) <- results, s == set]
        oks = [x | Right x <- rs]
    putStrLn $
      "parity: set " ++ set ++ ": " ++ show (length oks) ++ "/" ++ show (length rs) ++ " traces passed, "
        ++ show (sum (map fst oks)) ++ " decision points, " ++ show (sum (map snd oks)) ++ " draws checked"
  cs <- reverse <$> readIORef counts
  let oks = [x | (_, Right x, _) <- results]
      bad = length [() | (_, Left _, _) <- results]
  putStrLn $
    "parity: tier " ++ tier ++ ", sampled/total per set: "
      ++ unwords [set ++ " " ++ show a ++ "/" ++ show t | (set, (a, t)) <- cs] ++ "; "
      ++ show (length oks) ++ "/" ++ show (length results) ++ " traces passed, "
      ++ show (sum (map fst oks)) ++ " decision points, "
      ++ show (sum (map snd oks)) ++ " draws checked"
  when (bad > 0) $ do
    putStrLn ("parity: " ++ show bad ++ " trace(s) FAILED")
    exitFailure
  when (not coverageOk) $ do
    putStrLn "parity: trace coverage FAILED"
    exitFailure
  where
    -- traces are named <profile>-<seed>.json (v1) or <profile>-<seed>.trace (v2); the list files (slice0-moves.json, ...)
    -- and teams.json are not traces
    isTraceName ext n = ext `isSuffixOf` n && case reverse (take (length n - length ext) n) of
      c : _ -> isDigit c
      [] -> False
    render f (Failure w t d m) =
      "FAIL " ++ takeFileName (takeDirectory f) ++ "/" ++ takeFileName f ++ " | " ++ w ++ maybe "" (\x -> " | turn " ++ show x) t
        ++ " | draw #" ++ show d ++ "\n    " ++ m

-- | Read a trace set's list of ids.
readIdList :: FilePath -> IO [T.Text]
readIdList = readIdListOr' Nothing

-- | Read a trace set's list of ids, or the default when the set has no such file.
readIdListOr :: [T.Text] -> FilePath -> IO [T.Text]
readIdListOr = readIdListOr' . Just

readIdListOr' :: Maybe [T.Text] -> FilePath -> IO [T.Text]
readIdListOr' dflt f = do
  ok <- doesFileExist f
  case (ok, dflt) of
    (False, Just d) -> pure d
    (False, Nothing) -> do
      hPutStrLn stderr ("parity: " ++ f ++ " is missing (the generator writes it)")
      exitFailure
    _ -> readIds f

readIds :: FilePath -> IO [T.Text]
readIds f = do
  r <- eitherDecodeFileStrict' f
  case r of
    Right (Array xs) | Just ids <- traverse str (V.toList xs) -> pure ids
    _ -> hPutStrLn stderr ("parity: cannot read " ++ f) >> exitFailure
  where
    str (String t) = Just t
    str _ = Nothing

-- | The engine's accepted ids against the sets' lists: every list is a subset of what the engine accepts,
-- and the union of the lists is all of it.
checkPool :: String -> [T.Text] -> [(String, [T.Text])] -> IO ()
checkPool what actual sets = do
  let actualSet = Set.fromList actual
      unions = Set.unions [Set.fromList l | (_, l) <- sets]
      outside = [(set, Set.toList (Set.fromList l `Set.difference` actualSet)) | (set, l) <- sets, not (Set.null (Set.fromList l `Set.difference` actualSet))]
      untested = Set.toList (actualSet `Set.difference` unions)
  when (not (null outside) || not (null untested)) $ do
    mapM_ (\(set, xs) -> hPutStrLn stderr ("parity: trace set " ++ set ++ " lists " ++ what ++ "s the engine does not accept: " ++ show xs)) outside
    when (not (null untested)) $
      hPutStrLn stderr ("parity: the engine accepts " ++ what ++ "s no trace set lists (untested): " ++ show untested)
    exitFailure

-- | The number of threads to check traces on: @POKESIM_JOBS@, or half the processors (at least 1, at most 8). Each
-- thread holds one decoded trace (about 7 MB) at a time and has its own 128 MB allocation area (@-A128m@, set in the
-- cabal file: the default 4 MB spends half the run in the collector).
parityJobs :: IO Int
parityJobs = do
  procs <- getNumProcessors
  env <- lookupEnv "POKESIM_JOBS"
  n <- case env of
    Nothing -> pure (max 1 (min 8 (procs `div` 2)))
    Just t | not (null t), all isDigit t, read t > (0 :: Int) -> pure (read t)
    Just t -> hPutStrLn stderr ("parity: POKESIM_JOBS must be a positive number, got " ++ show t) >> exitFailure
  caps <- getNumCapabilities
  when (caps < n) (setNumCapabilities n)
  pure n

-- | Run @act@ over the items on @n@ threads, and hand each result to @done@ in the items' order as soon as it and
-- every earlier one are ready. An exception in @act@ is rethrown here, at that item's turn, as a sequential run would.
inOrder :: forall a b c. Int -> (a -> IO b) -> [a] -> (a -> b -> IO c) -> IO [c]
inOrder n act xs done
  | n <= 1 = mapM (\x -> act x >>= done x) xs
  | otherwise = do
      slots <- replicateM (length xs) (newEmptyMVar :: IO (MVar (Either SomeException b)))
      queue <- newMVar (zip xs slots)
      let worker = do
            next <- modifyMVar queue (\q -> pure (case q of [] -> ([], Nothing); j : js -> (js, Just j)))
            case next of
              Nothing -> pure ()
              Just (x, slot) -> do
                r <- try (act x)
                putMVar slot r
                worker
      replicateM_ n (forkIO worker)
      mapM (\(x, slot) -> takeMVar slot >>= either throwIO (done x)) (zip xs slots)

-- | The ids a trace's teams use (as ids, whatever the trace's spelling), for the coverage check.
-- The fields are strict, and 'runFile' forces every element, so that a trace's decoded JSON is freed once the
-- trace is checked. Lazy sets would each keep their whole trace alive until the coverage check at the end
-- (about 7 MB a trace, more than this laptop's memory over the long tier).
data Used = Used
  { usedMovesOf :: !(Set.Set T.Text)
  , usedSpeciesOf :: !(Set.Set T.Text)
  , usedAbilitiesOf :: !(Set.Set T.Text)
  , usedItemsOf :: !(Set.Set T.Text)
  }

-- | Load a trace, as v1 JSON or as a v2 file by its extension.
loadTrace :: FilePath -> IO (Either String Trace)
loadTrace f
  | ".trace" `isSuffixOf` f = decodeV2 <$> BL.readFile f
  | otherwise = (>>= fromV1) <$> eitherDecodeFileStrict' f

-- | Load and check one trace; on success returns (decision points, draws). Also returns the ids its teams use.
runFile :: Dex -> FilePath -> IO (Either Failure (Int, Int), Used)
runFile dex f = do
  r <- loadTrace f
  case r of
    Left e -> pure (Left (Failure "load" Nothing 0 e), Used Set.empty Set.empty Set.empty Set.empty)
    Right t -> do
      res <- checkTrace dex t
      let sets' = concatMap arr (arr (trTeams t))
          ids k = Set.fromList [toId x | s <- sets', String x <- [s .: k]]
          moveIds = Set.fromList [toId x | s <- sets', String x <- arr (s .: "moves")]
      let forced xs = Set.foldr' seq () xs `seq` xs
      u <- evaluate (Used (forced moveIds) (forced (ids "species")) (forced (ids "ability")) (forced (ids "item")))
      res' <- evaluate (case res of Right (a, b) -> a `seq` b `seq` res; Left _ -> res)
      pure (res', u)

-- ---------------------------------------------------------------------------
-- JSON helpers, for the teams (kept as JSON)

(.:) :: Value -> T.Text -> Value
(.:) (Object o) k = fromMaybe Null (KM.lookup (K.fromText k) o)
(.:) _ _ = Null

arr :: Value -> [Value]
arr (Array xs) = V.toList xs
arr _ = []

-- ---------------------------------------------------------------------------
-- Replaying a trace

-- | Structural requirements on a trace beyond the schema 'fromV1' and 'decodeV2' enforce.
checkShape :: Trace -> Either Failure ()
checkShape tr = do
  expect "traceVersion" (trVersion tr == 1) "traceVersion must be 1"
  expect "format" (trFormat tr == "gen9ou") "format must be gen9ou"
  where
    expect w ok msg = if ok then Right () else Left (Failure ("trace shape (" ++ w ++ ")") Nothing 0 msg)

checkTrace :: Dex -> Trace -> IO (Either Failure (Int, Int))
checkTrace dex tr = case prepare of
  Left e -> pure (Left e)
  Right (steps, teams, allDraws, cumulative) -> do
    ref <- newIORef ("setup" :: String)
    r <- try (drive ref steps teams allDraws cumulative)
    case r of
      Right x -> pure x
      Left ex -> do
        w <- readIORef ref
        pure (Left (Failure w Nothing 0 ("Haskell exception: " ++ show (ex :: SomeException))))
  where
    prepare = do
      checkShape tr
      teams <- case arr (trTeams tr) of
        [a, b] -> either (Left . Failure "teams" Nothing 0) Right ((,) <$> parseTeam dex a <*> parseTeam dex b)
        _ -> Left (Failure "teams" Nothing 0 "trace does not have two teams")
      let steps = trSteps tr
          toDraw (TDraw lo hi r) = Draw lo hi r
          stepDraws = map (map toDraw . stDraws) steps
      pure (steps, teams, concat stepDraws ++ map toDraw (trFinalDraws tr), scanl1 (+) (map length stepDraws))

    drive ref steps teams allDraws cumulative = do
      let total = length allDraws
          rs0 = replayStateOf allDraws
      writeIORef ref "newBattle"
      case replayed "newBattle" Nothing rs0 (newBattle dex gen9ou (fst teams) (snd teams)) of
        Left e -> pure (Left e)
        Right (b0, rs1) -> loop total b0 rs1 (0 :: Int) (zip steps cumulative) False
      where
        loop total b rs _ [] _ = finish total b rs
        loop total b rs i ((s, cum) : rest) sawUnasked = do
          writeIORef ref ("step " ++ show i)
          let one = stepOne b rs i s cum sawUnasked
          r <- evaluate (forceE one)
          case r of
            Left e -> pure (Left e)
            Right (b', rs', saw') -> loop total b' rs' (i + 1) rest saw'

        -- compare the decision point, self-check step errors, apply the recorded choices
        stepOne b rs i s cum sawUnasked = do
          let turn = bTurn b
              here = "step " ++ show i
              failWith msg = Left (Failure here (Just turn) (rsConsumed rs) msg)
          when (rsConsumed rs /= cum) $
            failWith ("draws consumed before this decision point: expected " ++ show cum ++ ", engine made " ++ show (rsConsumed rs))
          case diffState dex b (stState s) ++ unmodelled (stState s) of
            (d : _) -> failWith ("state differs at " ++ showDiff d)
            [] -> pure ()
          let legalExpected = [map T.unpack (fst (stLegal s)), map T.unpack (snd (stLegal s))]
              -- record.js lists `move N terastallize` only when the trace was recorded with `tera: true`; without it the
              -- policy was never offered Tera (Showdown still allowed it), so the recorded list is the engine's without those
              legalActual = [nub (map renderLegal (filter offered (legalChoices b p))) | p <- [P1, P2]]
              offered c = teraOffered || not (isTera c)
          when (legalExpected /= legalActual) $
            failWith ("legal choices differ: expected " ++ show legalExpected ++ ", engine " ++ show legalActual)
          chosen <- case [parseChoice (fst (stChosen s)), parseChoice (snd (stChosen s))] of
            [Right c1, Right c2] -> Right (c1, c2)
            parsed -> failWith ("cannot parse chosen " ++ show [e | Left e <- parsed])
          -- step must reject bad input without drawing
          let unasked = [p | (p, l) <- zip [P1, P2] legalExpected, null l]
              checks =
                [ ((Nothing, Nothing), MissingChoice P1) | i == 0 ]
                  ++ [ ((Just (Team [1, 1]), snd chosen), IllegalChoice P1 (Team [1, 1])) | i == 0 ]
                  ++ [ (inputFor p, UnexpectedChoice p (Move 1)) | not sawUnasked, p <- take 1 unasked ]
              inputFor p = if p == P1 then (Just (Move 1), snd chosen) else (fst chosen, Just (Move 1))
          mapM_ (\(inp, expected) -> case runReplay rs (step dex b inp) of
                   Right (Left e, rs') | e == expected, rsConsumed rs' == rsConsumed rs -> Right ()
                   res -> failWith ("step error self-check: expected " ++ show expected ++ ", got " ++ showStepResult res)) checks
          (res, rs') <- replayed (here ++ " (applying choices)") (Just turn) rs (step dex b chosen)
          b' <- either (\e -> failWith ("step rejected the recorded choices: " ++ show e)) Right res
          Right (b', rs', sawUnasked || not (null unasked))

        finish total bEnd rsEnd = do
          writeIORef ref "final"
          let endFail msg = Left (Failure "final" (Just (bTurn bEnd)) (rsConsumed rsEnd) msg)
              check = do
                case diffState dex bEnd (trFinal tr) ++ unmodelled (trFinal tr) of
                  (d : _) -> endFail ("final state differs at " ++ showDiff d)
                  [] -> pure ()
                when (rsConsumed rsEnd /= total) $
                  endFail ("draws consumed at the end: expected " ++ show total ++ ", engine made " ++ show (rsConsumed rsEnd))
                -- A trace the recorder cut off at its turn limit ends at a decision point (record.js stops at the top of
                -- the loop, before the choices): the battle is not over (`ended` is compared above), so an empty step
                -- must be refused for the missing choice, without a draw. Otherwise the battle is over and the step is
                -- refused as BattleFinished.
                case (trTruncated tr, runReplay rsEnd (step dex bEnd (Nothing, Nothing))) of
                  (True, Right (Left e, rs'))
                    | isMissing e, rsConsumed rs' == rsConsumed rsEnd -> Right ()
                  (True, res) -> endFail ("empty step on a truncated battle: expected MissingChoice without a draw, got " ++ showStepResult res)
                  (_, Right (Left BattleFinished, _)) -> Right ()
                  (_, res) -> endFail ("step on a finished battle: expected BattleFinished, got " ++ showStepResult res)
                Right (length (trSteps tr), total)
          evaluate (forceE check)

    isMissing e = case e of
      MissingChoice _ -> True
      _ -> False

    forceE e = case e of
      Left (Failure a t d m) -> length a + length m + d + maybe 0 id t `seq` e
      Right _ -> e

    showStepResult r = case r of
      Right (Left e, _) -> show e
      Right (Right _, _) -> "success"
      Left d -> show d

    replayed what turn rs m = case runReplay rs m of
      Right r -> Right r
      Left d -> Left (Failure what turn (divIx d) (showDivergence d))
    divIx (RangeMismatch i _ _) = i
    divIx (Exhausted i _) = i
    showDivergence (RangeMismatch i rec req) =
      "draw " ++ show i ++ ": Showdown drew from " ++ showRange rec ++ " but the engine requested " ++ showRange req
    showDivergence (Exhausted i req) =
      "draw " ++ show i ++ ": the trace has no more draws but the engine requested " ++ showRange req
    showRange (lo, hi) = "[" ++ show lo ++ "," ++ show hi ++ ")"
    renderLegal c = case c of
      Team _ -> "team"
      _ -> renderChoice c
    teraOffered = trTera tr == Just True
    isTera c = case c of
      MoveTera _ -> True
      _ -> False

parseChoice :: Maybe T.Text -> Either String (Maybe Choice)
parseChoice Nothing = Right Nothing
parseChoice (Just t) = case words (T.unpack t) of
  ["team", ds] | all isDigit ds -> Right (Just (Team (map (\c -> fromEnum c - fromEnum '0') ds)))
  ["move", n] | all isDigit n -> Right (Just (Move (read n)))
  ["move", n, "terastallize"] | all isDigit n -> Right (Just (MoveTera (read n)))
  ["switch", n] | all isDigit n -> Right (Just (Switch (read n)))
  _ -> Left (T.unpack t)
