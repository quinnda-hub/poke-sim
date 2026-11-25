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
import qualified Data.Vector as V
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
import Engine.Dex (Condition (..), Dex (..), supportedMoves)
import Engine.Effects (abilityBodies, itemBodies)
import Engine.Event (CondId (..), HandlerSpec (..), condName)
import Engine.Format (trunc, truncBits)
import Engine.Rule (executable, moveRule)
import Engine.State
import Engine.Modifier (baseMod, chain, chainModify, finalModify, modify, modifyBy)
import Engine.Stats (boostedStat, finalSpeed, parSpe)
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
        , ("every condition the engine runs is supported: a body for each handler the export lists",
            withTests 1 (conditionsSupported dex))
        , ("an ability or item is supported exactly when it has bodies, one for each handler the export lists and no others",
            withTests 1 (effectsSupported dex))
        ]
    , checkSequential $ Group "arithmetic (against JavaScript semantics)"
        [ ("roundDiv is Math.round(n / d)", withTests (fromIntegral n) propRoundDiv)
        , ("boostedStat is Math.floor(stat * stage multiplier)", withTests (fromIntegral n) propBoostedStat)
        , ("a paralysed action speed is floor(boosted speed * 50 / 100)", withTests (fromIntegral n) propParalysedSpeed)
        , ("stagedAccuracy is trunc(acc * stage multiplier)", withTests (fromIntegral n) propAccuracy)
        , ("trunc and truncBits are >>> 0 and its masks", withTests (fromIntegral n) propTrunc)
        , ("chain and chainModify are ((a * b + 2048) >> 12) of the scaled modifiers", withTests (fromIntegral n) propChain)
        , ("modify, modifyBy and finalModify are tr((tr(value * modifier) + 2047) / 4096)", withTests (fromIntegral n) propModify)
        , ("the STAB and burn modifiers are modify(value, 1.5) and modify(value, 0.5)", withTests (fromIntegral n) propDamageModifiers)
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

propParalysedSpeed :: Property
propParalysedSpeed = property $ do
  -- Spe stats of level <= 100 Pokemon stay under 1000; a stage up to 6 multiplies by at most 4
  spe <- forAll (Gen.int (Range.linear 1 999))
  s <- forAll (Gen.int (Range.linearFrom 0 (-6) 6))
  let table = [1, 3 / 2, 2, 5 / 2, 3, 7 / 2, 4] :: [Rational]
      m = table !! abs s
      boosted = if s >= 0 then floor (toRational spe * m) else floor (toRational spe / m) :: Int
  -- par.onModifySpe: Math.floor(finalModify(spe) * 50 / 100) after the stage (finalModify is the identity with no
  -- other modifier); the Spe stat is then capped at 10000 (out of reach here) and getActionSpeed truncates to 13 bits
  finalSpeed gen9ou (fst (parSpe gen9ou baseMod boosted)) === boosted * 50 `div` 100 `mod` 8192
  finalSpeed gen9ou boosted === boosted `mod` 8192

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

-- | @tr@ of a non-negative value (@>>> 0@): floor, modulo 2^32.
tr32 :: Rational -> Integer
tr32 x = floor x `mod` (2 ^ (32 :: Int))

-- | JavaScript's @x >> 12@ of a number that went through ToInt32: wrap to a signed 32-bit integer, then floor.
sar12 :: Integer -> Integer
sar12 x = (((x + 2 ^ (31 :: Int)) `mod` 2 ^ (32 :: Int)) - 2 ^ (31 :: Int)) `div` 4096

propChain :: Property
propChain = property $ do
  -- modifiers are small multiples of 4096, but the product may pass 2^31, where >> wraps
  a <- forAll (Gen.choice [Gen.int (Range.linear 0 40000), Gen.int (Range.linear 0 (2 ^ (17 :: Int)))])
  b <- forAll (Gen.choice [Gen.int (Range.linear 0 40000), Gen.int (Range.linear 0 (2 ^ (17 :: Int)))])
  -- chain: M'' = ((M * M') + 0x800) >> 12
  toInteger (chain a b) === sar12 (toInteger a * toInteger b + 2048)
  n <- forAll (Gen.int (Range.linear 0 60))
  d <- forAll (Gen.int (Range.linear 1 20))
  -- chainModify: previousMod = tr(event.modifier * 4096), nextMod = tr(n * 4096 / d), then the same step
  let next = tr32 (toRational n * 4096 / toRational d)
  toInteger (chainModify gen9ou a (n, d)) === sar12 (toInteger a * next + 2048)

propModify :: Property
propModify = property $ do
  v <- forAll (Gen.choice [Gen.int (Range.linear 0 2000), Gen.int (Range.linear 0 (2 ^ (40 :: Int)))])
  m <- forAll (Gen.choice [Gen.element [2048, 4096, 6144, 8192], Gen.int (Range.linear 0 40000)])
  -- modify: tr((tr(value * modifier) + 2048 - 1) / 4096)
  let ref x md = tr32 ((toRational (tr32 (toRational x * toRational md)) + 2047) / 4096)
  toInteger (modify gen9ou v m) === ref v m
  n <- forAll (Gen.int (Range.linear 0 60))
  d <- forAll (Gen.int (Range.linear 1 20))
  -- modify(value, n, d): the modifier is tr(n * 4096 / d)
  toInteger (modifyBy gen9ou v (n, d)) === ref v (tr32 (toRational n * 4096 / toRational d))
  -- finalModify: modify by the accumulated modifier, then reset it to 1
  finalModify gen9ou v m === (modify gen9ou v m, 4096)

propDamageModifiers :: Property
propDamageModifiers = property $ do
  v <- forAll (Gen.choice [Gen.int (Range.linear 0 2000), Gen.int (Range.linear 0 (2 ^ (24 :: Int)))])
  -- modifyDamage: modify(baseDamage, 1.5) for STAB and modify(baseDamage, 0.5) for a burn; the scaled
  -- modifiers are tr(1.5 * 4096) and tr(0.5 * 4096)
  modify gen9ou v 6144 === modifyBy gen9ou v (3, 2)
  modify gen9ou v 2048 === modifyBy gen9ou v (1, 2)
  toInteger (modify gen9ou v 6144) === tr32 ((toRational (tr32 (toRational v * 6144)) + 2047) / 4096)

-- | Every condition the engine runs has a body for each handler the export lists for it (the support rule of
-- docs/handlers.md section 2), and a duration only where Showdown has one.
conditionsSupported :: Dex -> Property
conditionsSupported dex = withTests 1 $ property $ do
  mapM_ (\c -> do
    let cn = dxConditions dex V.! fromEnum c
    footnote (T.unpack (condName c) ++ " has a handler without a body")
    assert (cnSupported cn)) [minBound .. maxBound :: CondId]
  -- flinch has duration 1 (conditions.ts); the statuses have none
  map (\c -> cnDuration (dxConditions dex V.! fromEnum c)) [minBound .. maxBound :: CondId] === [0, 0, 0, 0, 0, 1]

-- | An ability or item is supported exactly when "Engine.Effects" has bodies for it, and then the handlers the
-- engine runs are exactly those bodies: one for each handler the export lists, and no body for a handler the export
-- does not list (which would be dead code under a name Showdown never calls).
effectsSupported :: Dex -> Property
effectsSupported dex = withTests 1 $ property $ do
  let ty name = V.findIndex (== name) (dxTypeNames dex)
      checkTable what bodiesOf table = mapM_ (\c -> case bodiesOf ty (cnId c) of
        Nothing -> do
          footnote (what ++ " " ++ T.unpack (cnId c) ++ " is supported without bodies")
          assert (not (cnSupported c))
        Just bs -> do
          footnote (what ++ " " ++ T.unpack (cnId c) ++ " has bodies but is not supported: "
            ++ maybe "" T.unpack (cnReject c))
          assert (cnSupported c)
          let run = [(p, e) | (p, e, spec) <- cnHandlers c, Just _ <- [hsBody spec]]
              written = [(p, e) | (p, e, _) <- bs]
          footnote (what ++ " " ++ T.unpack (cnId c) ++ ": bodies " ++ show written ++ ", run " ++ show run)
          assert (all (`elem` run) written && all (`elem` written) run && length run == length written))
        (V.toList table)
  checkTable "ability" abilityBodies (dxAbilities dex)
  checkTable "item" itemBodies (dxItems dex)
