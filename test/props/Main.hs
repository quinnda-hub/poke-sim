{-# LANGUAGE OverloadedStrings #-}
-- | Property suite (hedgehog). Properties check invariants and agreement with JavaScript arithmetic; the
-- expected result of a battle comes only from the Showdown traces (see the parity test).
--
-- Environment: @POKESIM_TIER@ = @fast@ (about 100 tests per property) or @long@ (about 2000; the default),
-- @POKESIM_DEX@ (default @data/gen9@).
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (Value (..), eitherDecodeFileStrict')
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as H
import qualified Hedgehog.Range as Range
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import System.Random.SplitMix (mkSMGen)

import Engine
import Engine.Chance (runSample)
import Engine.Dex (supportedMoves)
import Engine.Format (trunc, truncBits)
import Engine.Rule (executable, moveRule)
import Engine.State
import Engine.Stats (boostedStat)
import Engine.Turn (roundDiv, stagedAccuracy)
import Playout

main :: IO ()
main = do
  tier <- lookupEnv "POKESIM_TIER" >>= \t -> case t of
    Nothing -> pure "long"
    Just "long" -> pure "long"
    Just "fast" -> pure "fast"
    Just o -> die ("props: POKESIM_TIER must be fast or long, got " ++ show o)
  dexDir <- fromMaybe "data/gen9" <$> lookupEnv "POKESIM_DEX"
  dex <- loadDex dexDir
  movesJson <- eitherDecodeFileStrict' (dexDir </> "moves.json") >>= either die pure
  let n = if tier == ("fast" :: String) then 100 else 2000 :: Int
  putStrLn ("props: tier " ++ tier ++ ", " ++ show n ++ " tests per property")
  H.defaultMain
    [ checkSequential $ Group "engine"
        [ ("random playouts keep the decision-point and state invariants", withTests (fromIntegral n) (playouts dex))
        , ("bad input is rejected without draws", withTests (fromIntegral n) (badInput dex))
        , ("every move translates to Right or Left and supported moves are executable",
            withTests 1 (translation dex movesJson))
        ]
    , checkSequential $ Group "arithmetic (against JavaScript semantics)"
        [ ("roundDiv is Math.round(n / d)", withTests (fromIntegral n) propRoundDiv)
        , ("boostedStat is Math.floor(stat * stage multiplier)", withTests (fromIntegral n) propBoostedStat)
        , ("stagedAccuracy is trunc(acc * stage multiplier)", withTests (fromIntegral n) propAccuracy)
        , ("trunc and truncBits are >>> 0 and its masks", withTests (fromIntegral n) propTrunc)
        ]
    ]

-- ---------------------------------------------------------------------------
-- Engine properties

playouts :: Dex -> Property
playouts dex = property $ do
  (t1, t2) <- forAll (genTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  case fst (runSample (mkSMGen seed) (playChecked dex t1 t2)) of
    Left e -> footnote e >> failure
    Right b -> case request b of
      Finished _ -> success
      Awaiting {} -> footnote "playout ended without a result" >> failure

data BadKind = Illegal | Missing | Unexpected
  deriving (Eq, Show, Enum, Bounded)

badInput :: Dex -> Property
badInput dex = property $ do
  (t1, t2) <- forAll (genTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  steps <- forAll (Gen.int (Range.linear 0 400))
  kind <- forAll Gen.enumBounded
  pickSide <- forAll Gen.bool
  let oneSided b = case request b of
        Awaiting _ a1 a2 -> a1 /= a2
        Finished _ -> True
      stop = if kind == Unexpected then oneSided else const True
      reached = fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex steps stop))
  b <- evalEither reached
  let legal p = case legalChoices b p of (c : _) -> Just c; [] -> Nothing
      candidates p = [c | c <- [Move 0, Move 1, Move 4, Move 5, Switch 0, Switch 1, Switch 2, Switch 6, Switch 7, Team [], Team [1, 1], Team [1 .. 7]]
                        , not (isLegal b p c)]
  inputs <- case request b of
    Finished _ -> pure (Just (Just (Move 1), Just (Move 1)))
    Awaiting _ a1 a2 -> case kind of
      Illegal -> do
        let p = if a1 && (pickSide || not a2) then P1 else P2
        c <- forAll (Gen.element (candidates p))
        pure (Just (if p == P1 then (Just c, legal P2) else (legal P1, Just c)))
      Missing ->
        let p = if a1 && (pickSide || not a2) then P1 else P2
         in pure (Just (if p == P1 then (Nothing, legal P2) else (legal P1, Nothing)))
      Unexpected
        | a1 && not a2 -> pure (Just (legal P1, Just (Move 1)))
        | a2 && not a1 -> pure (Just (Just (Move 1), legal P2))
        | otherwise -> pure Nothing
  case inputs of
    Nothing -> success
    Just cs -> do
      let (r, draws) = runCounter (step dex b cs)
      footnote ("inputs " ++ show cs ++ " in request " ++ show (request b))
      assert (isLeft r)
      draws === 0

translation :: Dex -> Value -> Property
translation dex movesJson = withTests 1 $ property $ do
  entries <- case movesJson of
    Object o -> pure (KM.toList o)
    _ -> footnote "moves.json is not an object" >> failure
  mapM_ (\(k, v) -> do
    r <- evalIO (try (evaluate (forceResult (moveRule v)))) :: PropertyT IO (Either SomeException Int)
    case r of
      Left ex -> footnote (show (K.toText k) ++ ": " ++ show ex) >> failure
      Right _ -> success) entries
  mapM_ (\name -> case [v | (k, v) <- entries, K.toText k == name] of
    [v] -> case moveRule v of
      Right rule -> assert (executable rule) >> pure ()
      Left why -> footnote (T.unpack name ++ " is supported but does not translate: " ++ T.unpack why) >> failure
    _ -> footnote (T.unpack name ++ ": not exactly one entry in moves.json") >> failure) (supportedMoves dex)
  where
    forceResult = either (length . T.unpack) (length . show)

-- ---------------------------------------------------------------------------
-- Arithmetic properties, each against a Rational reference written from the JavaScript expression

-- | A mix of small operands (where ties are common) and large ones.
genNonNeg :: Int -> Gen Int
genNonNeg hi = Gen.choice [Gen.int (Range.linear 0 200), Gen.int (Range.linear 0 hi)]

propRoundDiv :: Property
propRoundDiv = property $ do
  n <- forAll (genNonNeg (2 ^ (40 :: Int)))
  d <- forAll (Gen.choice [Gen.int (Range.linear 1 20), Gen.int (Range.linear 1 (2 ^ (20 :: Int)))])
  -- Math.round(n / d) = floor(n / d + 1/2) for n / d >= 0
  roundDiv n d === floor (toRational n / toRational d + 1 / 2)

propBoostedStat :: Property
propBoostedStat = property $ do
  stat <- forAll (genNonNeg (2 ^ (31 :: Int)))
  s <- forAll (Gen.int (Range.linearFrom 0 (-6) 6))
  -- Math.floor(stat * [1, 1.5, 2, 2.5, 3, 3.5, 4][s]) and, for a negative stage, stat / that
  let table = [1, 3 / 2, 2, 5 / 2, 3, 7 / 2, 4] :: [Rational]
      m = table !! abs s
      expected = if s >= 0 then floor (toRational stat * m) else floor (toRational stat / m) :: Int
  boostedStat stat s === expected

propAccuracy :: Property
propAccuracy = property $ do
  acc <- forAll (Gen.choice [Gen.int (Range.linear 1 100), Gen.element [35, 70, 85, 90, 95]])
  ua <- forAll (Gen.int (Range.linearFrom 0 (-8) 8))
  ev <- forAll (Gen.int (Range.linearFrom 0 (-8) 8))
  -- boost = clamp(accuracy stage, -6, 6); boost = clamp(boost - evasion stage, -6, 6)
  let clampI = max (-6) . min 6
      boost = clampI (clampI ua - ev)
      a = toRational acc
      expected
        | boost > 0 = truncate (a * toRational (3 + boost) / 3)
        | boost < 0 = truncate (a * 3 / toRational (3 - boost))
        | otherwise = acc
  stagedAccuracy gen9ou acc noBoosts {bsAccuracy = ua} noBoosts {bsEvasion = ev} === (expected :: Int)

propTrunc :: Property
propTrunc = property $ do
  -- JavaScript doubles are exact integers up to 2^53; values above 2^32 are the interesting ones
  x <- forAll (Gen.choice
    [ Gen.integral (Range.linearFrom 0 (-(2 ^ (53 :: Int))) (2 ^ (53 :: Int)))
    , Gen.integral (Range.linear 0 (2 ^ (34 :: Int)))
    , Gen.integral (Range.linear (2 ^ (32 :: Int) - 5) (2 ^ (32 :: Int) + 5))
    ]) :: PropertyT IO Int
  bits <- forAll (Gen.element [13, 16])
  let u32 = toInteger x `mod` (2 ^ (32 :: Int)) -- x >>> 0
  toInteger (trunc gen9ou x) === u32
  toInteger (truncBits gen9ou bits x) === u32 `mod` (2 ^ (bits :: Int)) -- (x >>> 0) % 2**bits
  pure ()
