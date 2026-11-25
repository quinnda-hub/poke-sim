{-# LANGUAGE OverloadedStrings #-}
-- | Parity test: replay recorded Pokemon Showdown battles (traces) through the engine and require the
-- same random draws (count, ranges, order) and the same state at every decision point.
--
-- Environment: @POKESIM_TRACES@ (default @local/traces/slice0@), @POKESIM_DEX@ (default @data/gen9@).
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, when)
import Data.Aeson (Value (..), eitherDecodeFileStrict')
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.IORef
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import System.IO (hFlush, hPutStrLn, stderr)

import Engine
import Engine.Chance
import Engine.Dex
import Engine.State

-- | A parity failure: where it happened, turn, draw index, message.
data Failure = Failure String (Maybe Int) Int String

-- | A difference between expected (trace) and actual (engine) values: field path, expected, actual.
type Diff = (String, String, String)

main :: IO ()
main = do
  dir <- fromMaybe "local/traces/slice0" <$> lookupEnv "POKESIM_TRACES"
  dexDir <- fromMaybe "data/gen9" <$> lookupEnv "POKESIM_DEX"
  exists <- doesDirectoryExist dir
  names <- if exists then listDirectory dir else pure []
  let files = sort [dir </> n | n <- names, isTraceName n]
  when (null files) $ do
    hPutStrLn stderr $
      "parity: no traces found in " ++ show dir ++ ".\n"
        ++ "Generate them with:  node tools/oracle/gen-slice0.js local/traces/slice0 500\n"
        ++ "or point POKESIM_TRACES at a directory of slice0-<n>.json files."
    exitFailure
  dex <- loadDex dexDir
  checkSupported "move" (supportedMoves dex) (dir </> "slice0-moves.json")
  checkSupported "species" (supportedSpecies dex) (dir </> "slice0-species.json")
  results <- forM files $ \f -> do
    r <- runFile dex f
    case r of
      Left e -> do
        hPutStrLn stderr (render f e)
        hFlush stderr
      Right _ -> pure ()
    pure r
  let oks = [x | Right x <- results]
      bad = length [() | Left _ <- results]
  putStrLn $
    "parity: " ++ show (length oks) ++ "/" ++ show (length files) ++ " traces passed, "
      ++ show (sum (map fst oks)) ++ " decision points, "
      ++ show (sum (map snd oks)) ++ " draws checked"
  when (bad > 0) $ do
    putStrLn ("parity: " ++ show bad ++ " trace(s) FAILED")
    exitFailure
  where
    -- traces are named <profile>-<seed>.json; the list files (slice0-moves.json, ...) are not traces
    isTraceName n = case reverse n of
      ('n' : 'o' : 's' : 'j' : '.' : c : _) -> isDigit c
      _ -> False
    render f (Failure w t d m) =
      "FAIL " ++ takeFileName f ++ " | " ++ w ++ maybe "" (\x -> " | turn " ++ show x) t
        ++ " | draw #" ++ show d ++ "\n    " ++ m

-- | What the engine accepts (moves, species) must equal the pool the generator draws from, so that
-- every set the engine accepts is one it has been tested on.
checkSupported :: String -> [T.Text] -> FilePath -> IO ()
checkSupported what actual f = do
  ok <- doesFileExist f
  when (not ok) $ do
    hPutStrLn stderr ("parity: " ++ f ++ " is missing (node tools/oracle/gen-slice0.js writes it)")
    exitFailure
  r <- eitherDecodeFileStrict' f
  case r of
    Right (Array xs) | Just expected <- traverse str (V.toList xs) ->
      when (sort expected /= sort actual) $ do
        let expectedSet = Set.fromList expected
            actualSet = Set.fromList actual
        hPutStrLn stderr $
          "parity: supported " ++ what ++ " set differs from " ++ f ++ ": only in trace list "
            ++ show (Set.toList (expectedSet `Set.difference` actualSet)) ++ ", only in engine "
            ++ show (Set.toList (actualSet `Set.difference` expectedSet))
        exitFailure
    _ -> hPutStrLn stderr ("parity: cannot read " ++ f) >> exitFailure
  where
    str (String t) = Just t
    str _ = Nothing

-- | Load and check one trace; on success returns (decision points, draws).
runFile :: Dex -> FilePath -> IO (Either Failure (Int, Int))
runFile dex f = do
  r <- eitherDecodeFileStrict' f
  case r of
    Left e -> pure (Left (Failure "load" Nothing 0 e))
    Right v -> checkTrace dex v

-- ---------------------------------------------------------------------------
-- JSON helpers (missing data yields Null, which then shows up as a mismatch)

(.:) :: Value -> T.Text -> Value
(.:) (Object o) k = fromMaybe Null (KM.lookup (K.fromText k) o)
(.:) _ _ = Null

arr :: Value -> [Value]
arr (Array xs) = V.toList xs
arr _ = []

int :: Value -> Maybe Int
int (Number n) = toBoundedInteger n
int _ = Nothing

txt :: Value -> String
txt (String t) = T.unpack t
txt Null = "<null>"
txt v = show v

-- ---------------------------------------------------------------------------
-- Replaying a trace

-- | Strictly parse a draw list: every entry must be three integers with lo < hi.
parseDraws :: String -> Value -> Either Failure [Draw]
parseDraws what v = traverse one (zip [0 :: Int ..] (arr v))
  where
    one (i, d) = case map int (arr d) of
      [Just lo, Just hi, Just r]
        | lo < hi -> Right (Draw lo hi r)
        | otherwise -> bad i ("draw has an empty range [" ++ show lo ++ "," ++ show hi ++ ")")
      _ -> bad i ("draw is not three integers: " ++ show d)
    bad i msg = Left (Failure ("trace shape (" ++ what ++ " draw " ++ show i ++ ")") Nothing 0 msg)

-- | Structural requirements on a trace.
checkShape :: Value -> Either Failure ()
checkShape tr = do
  expect "traceVersion" (int (tr .: "traceVersion") == Just 1) "traceVersion must be 1"
  expect "format" (tr .: "format" == String "gen9ou") "format must be gen9ou"
  mapM_ sides (zip [0 :: Int ..] (map (.: "state") (arr (tr .: "steps"))))
  expect "final" (length (arr (tr .: "final" .: "sides")) == 2) "final snapshot must have two sides"
  where
    expect w ok msg = if ok then Right () else Left (Failure ("trace shape (" ++ w ++ ")") Nothing 0 msg)
    sides (i, st) = expect ("step " ++ show i) (length (arr (st .: "sides")) == 2) "snapshot must have exactly two sides"

checkTrace :: Dex -> Value -> IO (Either Failure (Int, Int))
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
      teams <- case arr (tr .: "teams") of
        [a, b] -> either (Left . Failure "teams" Nothing 0) Right ((,) <$> parseTeam dex a <*> parseTeam dex b)
        _ -> Left (Failure "teams" Nothing 0 "trace does not have two teams")
      let steps = arr (tr .: "steps")
      stepDraws <- traverse (\(i, s) -> parseDraws ("step " ++ show (i :: Int)) (s .: "draws")) (zip [0 ..] steps)
      finalDraws <- parseDraws "final" (tr .: "finalDraws")
      pure (steps, teams, concat stepDraws ++ finalDraws, scanl1 (+) (map length stepDraws))

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
          case diffState dex b (s .: "state") ++ unmodelled (s .: "state") of
            (d : _) -> failWith ("state differs at " ++ showDiff d)
            [] -> pure ()
          let legalExpected = map (map txt . arr) (arr (s .: "legal"))
              legalActual = [nub (map renderLegal (legalChoices b p)) | p <- [P1, P2]]
          when (legalExpected /= legalActual) $
            failWith ("legal choices differ: expected " ++ show legalExpected ++ ", engine " ++ show legalActual)
          chosen <- case map parseChoice (arr (s .: "chosen")) of
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
                case diffState dex bEnd (tr .: "final") ++ unmodelled (tr .: "final") of
                  (d : _) -> endFail ("final state differs at " ++ showDiff d)
                  [] -> pure ()
                when (rsConsumed rsEnd /= total) $
                  endFail ("draws consumed at the end: expected " ++ show total ++ ", engine made " ++ show (rsConsumed rsEnd))
                case runReplay rsEnd (step dex bEnd (Nothing, Nothing)) of
                  Right (Left BattleFinished, _) -> Right ()
                  res -> endFail ("step on a finished battle: expected BattleFinished, got " ++ showStepResult res)
                Right (length (arr (tr .: "steps")), total)
          evaluate (forceE check)

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

parseChoice :: Value -> Either String (Maybe Choice)
parseChoice Null = Right Nothing
parseChoice (String t) = case words (T.unpack t) of
  ["team", ds] | all isDigit ds -> Right (Just (Team (map (\c -> fromEnum c - fromEnum '0') ds)))
  ["move", n] | all isDigit n -> Right (Just (Move (read n)))
  ["switch", n] | all isDigit n -> Right (Just (Switch (read n)))
  _ -> Left (T.unpack t)
parseChoice v = Left (show v)

-- ---------------------------------------------------------------------------
-- State comparison

showDiff :: Diff -> String
showDiff (path, e, a) = path ++ ": expected " ++ e ++ ", engine has " ++ a

-- | Compare a field.
cmp :: (Eq a, Show a) => String -> a -> a -> [Diff]
cmp path e a = [(path, show e, show a) | e /= a]

-- | All differences between the engine's battle and a trace snapshot, for every field slice 0 models.
diffState :: Dex -> Battle -> Value -> [Diff]
diffState dex b st =
  cmp "turn" (int (st .: "turn")) (Just (bTurn b))
    ++ cmp "request" (txt (st .: "request")) requestName
    ++ cmp "ended" (st .: "ended") (Bool ended)
    ++ cmp "winner" (st .: "winner") winnerValue
    ++ concat
      [ diffSide dex p (sideOf p b) sd
      | (p, sd) <- zip [P1, P2] (arr (st .: "sides"))
      ]
  where
    (requestName, ended, winnerValue) = case request b of
      Finished w -> ("", True, case w of Winner P1 -> String "p1"; Winner P2 -> String "p2"; Tie -> String "tie")
      Awaiting TeamPreview _ _ -> ("teampreview", False, Null)
      Awaiting MoveRequest _ _ -> ("move", False, Null)
      Awaiting SwitchRequest _ _ -> ("switch", False, Null)

diffSide :: Dex -> Player -> Side -> Value -> [Diff]
diffSide dex p s sd =
  cmp (name ++ ".pokemonLeft") (int (sd .: "pokemonLeft")) (Just (sLeft s))
    ++ cmp (name ++ ".pokemon count") (length expected) (U.length (sSlots s))
    ++ concat
      [ diffMon dex (name ++ ".pokemon[" ++ show slot ++ "]") orig m ev
      | (slot, ev, orig) <- zip3 [0 :: Int ..] expected (U.toList (sSlots s))
      , let m = sMons s SV.! orig
      ]
  where
    name = "sides[" ++ show (playerIx p) ++ "]"
    expected = arr (sd .: "pokemon")

diffMon :: Dex -> String -> Int -> Mon -> Value -> [Diff]
diffMon dex path orig m ev =
  cmp (path ++ ".orig") (int (ev .: "orig")) (Just orig)
    ++ cmp (path ++ ".species") (txt (ev .: "species")) (T.unpack (spId (speciesData dex (monSpecies m))))
    ++ cmp (path ++ ".level") (int (ev .: "level")) (Just (monLevel m))
    ++ cmp (path ++ ".gender") (txt (ev .: "gender")) (genderName (monGender m))
    ++ cmp (path ++ ".hp") (int (ev .: "hp")) (Just (monHp m))
    ++ cmp (path ++ ".maxhp") (int (ev .: "maxhp")) (Just (monMaxHp m))
    ++ cmp (path ++ ".status") (txt (ev .: "status")) (case monStatus m of StatusNone -> ""; StatusFainted -> "fnt")
    ++ cmp (path ++ ".fainted") (ev .: "fainted") (Bool (monFainted m))
    ++ cmp (path ++ ".active") (ev .: "active") (Bool (monActive m))
    ++ concat
      [ cmp (path ++ ".stats." ++ T.unpack k) (int (ev .: "stats" .: k)) (Just v)
      | (k, v) <- [("atk", stAtk st), ("def", stDef st), ("spa", stSpa st), ("spd", stSpd st), ("spe", stSpe st)]
      ]
    ++ cmp (path ++ ".speed") (int (ev .: "speed")) (Just (monSpeed m))
    ++ cmp (path ++ ".types") (map txt (arr (ev .: "types"))) types
    ++ cmp (path ++ ".moves.count") (length (arr (ev .: "moves"))) (SV.length (monMoves m))
    ++ concat
      [ cmp (mp ++ ".id") (txt (mv .: "id")) (T.unpack (mvId (moveData dex (msMove ms))))
          ++ cmp (mp ++ ".pp") (int (mv .: "pp")) (Just (msPP ms))
          ++ cmp (mp ++ ".maxpp") (int (mv .: "maxpp")) (Just (msMaxPP ms))
      | (i, mv, ms) <- zip3 [0 :: Int ..] (arr (ev .: "moves")) (SV.toList (monMoves m))
      , let mp = path ++ ".moves[" ++ show i ++ "]"
      ]
  where
    st = monStats m
    typeName t = T.unpack (dxTypeNames dex V.! t)
    types = typeName (monType1 m) : [typeName (monType2 m) | monType2 m /= noType]
    genderName g = case g of GenderM -> "M"; GenderF -> "F"; GenderNone -> ""

-- | Fields of a snapshot that the engine does not model must hold their defaults, so a trace that uses an
-- out-of-slice mechanic fails loudly instead of going unchecked.
unmodelled :: Value -> [Diff]
unmodelled st =
  isEmptyStr "field.weather" (st .: "field" .: "weather")
    ++ isEmptyStr "field.terrain" (st .: "field" .: "terrain")
    ++ isEmptyObj "field.pseudoWeather" (st .: "field" .: "pseudoWeather")
    ++ concat
      [ isEmptyObj (sp ++ ".sideConditions") (sd .: "sideConditions")
          ++ concat
            [ let pp = sp ++ ".pokemon[" ++ show j ++ "]"
               in isEmptyObj (pp ++ ".volatiles") (pk .: "volatiles")
                    ++ isEmptyObj (pp ++ ".statusState") (pk .: "statusState")
                    ++ isEmptyStr (pp ++ ".terastallized") (pk .: "terastallized")
                    ++ cmp (pp ++ ".ability") (txt (pk .: "ability")) "noability"
                    ++ isEmptyStr (pp ++ ".item") (pk .: "item")
                    ++ concat
                      [ cmp (pp ++ ".boosts." ++ T.unpack k) (int (pk .: "boosts" .: k)) (Just 0)
                      | k <- ["atk", "def", "spa", "spd", "spe", "accuracy", "evasion"]
                      ]
                    ++ cmp (pp ++ ".boosts key count") (length (objEntries (pk .: "boosts"))) 7
                    ++ concat [cmp (pp ++ ".moves[" ++ show m ++ "].disabled") (mv .: "disabled") (Bool False) | (m, mv) <- zip [0 :: Int ..] (arr (pk .: "moves"))]
            | (j, pk) <- zip [0 :: Int ..] (arr (sd .: "pokemon"))
            ]
      | (i, sd) <- zip [0 :: Int ..] (arr (st .: "sides"))
      , let sp = "sides[" ++ show i ++ "]"
      ]
  where
    isEmptyStr path v = cmp path (txt v) ""
    isEmptyObj path v = case v of
      Object o | KM.null o -> []
      _ -> [(path, "{}", txt v)]
    objEntries (Object o) = KM.toList o
    objEntries _ = []
