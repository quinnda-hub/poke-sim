{-# LANGUAGE OverloadedStrings #-}
-- | Property suite (hedgehog). Properties check invariants and agreement with JavaScript arithmetic; the
-- expected result of a battle comes only from the Showdown traces (see the parity test).
--
-- Environment: @POKESIM_TIER@ = @fast@ (about 100 tests per property) or @long@ (about 2000; the default),
-- @POKESIM_DEX@ (default @data/gen9@).
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (Value (..), eitherDecodeFileStrict')
import Data.Bits (shiftR, testBit)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (State, evalStateT, modify', runState, runStateT, state)
import Data.Functor.Identity (runIdentity)
import Data.Either (isLeft)
import Data.List (group, sort, sortOn, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
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
import Engine.Chance (runSample, speedSortWith)
import Engine.Dex (Condition (..), Dex (..), MoveData (..), SpeciesData (..), abilityId, dexCondLookup, itemId, supportedAbilities, supportedItems, supportedMoves, supportedSpecies)
import Engine.Dispatch (HitResult (..), addSideCondition, basePowerCallback, moveAfterHit, moveFieldHit, moveOwnHit)
import Engine.Effects (abilityBodies, fieldCondBodies, itemBodies, moveBodies)
import Engine.Event (CondId (..), EventId (..), HandlerSpec (..), Prefix (..), condName, parseHandlerName)
import Engine.Format (trunc, truncBits)
import Engine.Rule (DamageKind (..), MoveRule (..), executable, moveRule)
import Engine.State
import Engine.Modifier (baseMod, chain, chainModify, finalModify, modify, modifyBy)
import Engine.Stats (actionSpeed, boostedStat, finalSpeed, parSpe)
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
  conditionsJson <- eitherDecodeFileStrict' (dexDir </> "conditions.json") >>= either die pure
  abilitiesJson <- eitherDecodeFileStrict' (dexDir </> "abilities.json") >>= either die pure
  let n = if tier == ("fast" :: String) then 100 else 2000 :: Int
  putStrLn ("props: tier " ++ tier ++ ", " ++ show n ++ " tests per property")
  H.defaultMain
    [ checkSequential $ Group "engine"
        [ ("random playouts keep the decision-point and state invariants", withTests (fromIntegral n) (playouts dex))
        , ("random playouts over teams heavy in self-switch moves, Rocky Helmet and Shedinja keep the invariants, switch flags and mid-turn requests included",
            withTests (fromIntegral n) (pivotPlayouts dex))
        , ("bad input is rejected without draws", withTests (fromIntegral n) (badInput dex))
        , ("every move translates to Right or Left and supported moves are executable",
            withTests 1 (translation dex movesJson))
        , ("every condition the engine runs is supported: a body for each handler the export lists",
            withTests 1 (conditionsSupported dex conditionsJson abilitiesJson movesJson))
        , ("a field condition is supported exactly when it has bodies, one for each handler the export lists and no others",
            withTests 1 (fieldConditionsSupported dex))
        , ("an ability or item is supported exactly when it has bodies, one for each handler the export lists and no others",
            withTests 1 (effectsSupported dex))
        , ("a move is supported only with bodies, one for each handler of its own that the export lists and no others",
            withTests 1 (movesSupported dex movesJson))
        , ("no ability, item, condition or field condition listens to an event only moves' own handlers run",
            withTests 1 (moveEventsOnlyOfMoves dex))
        , ("only the Roost volatile listens to the Type event, so the types the state holds are getTypes()",
            withTests 1 (onlyRoostListensToType dex))
        , ("a fixed-damage move has no base power, and the damage-formula fields and the self boost belong to damaging moves",
            withTests 1 (damageFields dex))
        , ("a move's own BasePower handler sorts after every other BasePower handler, so it never ties with one",
            withTests 1 (moveBasePowerSortsLast dex))
        , ("speedSort: a sorted permutation, drawing exactly Showdown's shuffle ranges over each tie run in order",
            withTests (fromIntegral n) speedSortDraws)
        , ("every effect's event mask is exactly the events its handler list names, and every event fits the 64-bit mask",
            withTests 1 (eventMasksExact dex))
        , ("Court Change and Haze: the sides exchange the listed conditions with their states and nothing else changes; Haze clears the stages in play",
            withTests (fromIntegral n) (fieldMoves dex))
        , ("Knock Off, Thief and Trick: an item removal never creates an item, a refusal keeps it, and a swap hands each item over whole",
            withTests (fromIntegral n) (itemMoves dex))
        , ("a weight-based base power never decreases as the target gets heavier or the user lighter",
            withTests (fromIntegral n) (weightBands dex))
        , ("a base-power callback never lowers the move's own base power below what its condition adds",
            withTests (fromIntegral n) (callbacksRaise dex))
        ]
    , checkSequential $ Group "arithmetic (against JavaScript semantics)"
        [ ("roundDiv is Math.round(n / d)", withTests (fromIntegral n) propRoundDiv)
        , ("boostedStat is Math.floor(stat * stage multiplier)", withTests (fromIntegral n) propBoostedStat)
        , ("a paralysed action speed is floor(boosted speed * 50 / 100)", withTests (fromIntegral n) propParalysedSpeed)
        , ("an action speed is a 13-bit value, and Trick Room reverses the order of speeds", withTests (fromIntegral n) propActionSpeed)
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

-- | 'playouts' over 'genPivotTeams': the switch-flag invariants of 'checkBattle' and the mid-turn ones of 'checkStep' (slice 6a) are checked at
-- every state and step, and these teams reach the requests they are about at every few turns.
pivotPlayouts :: Dex -> Property
pivotPlayouts dex = property $ do
  (t1, t2) <- forAll (genPivotTeams dex)
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
  -- the move's own handlers rejection, as "Engine.Dex" builds it
  let ownReject name = case V.find ((== name) . mvId) (dxMoves dex) of
        Just m -> cnReject (mvEffect m)
        Nothing -> Nothing
  mapM_ (\(k, v) -> do
    r <- evalIO (try (evaluate (forceResult (moveRule (dexCondLookup dex) (ownReject (K.toText k)) v)))) :: PropertyT IO (Either SomeException Int)
    case r of
      Left ex -> footnote (show (K.toText k) ++ ": " ++ show ex) >> failure
      Right _ -> success) entries
  mapM_ (\name -> case [v | (k, v) <- entries, K.toText k == name] of
    [v] -> case moveRule (dexCondLookup dex) (ownReject name) v of
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

-- | Invariants of 'actionSpeed' that do not restate its formula: whatever the Speed stat, the result is a 13-bit value
-- (battle.trunc(speed, 13), with and without Trick Room); and where no wrap or cap intervenes (a Speed of at least 1809 and
-- at most 8191, so that neither the Speed nor its Trick Room value leaves 0..8191) a faster Pokemon is faster
-- without Trick Room and slower under it, which is what Trick Room is.
propActionSpeed :: Property
propActionSpeed = property $ do
  spe <- forAll (Gen.choice [Gen.int (Range.linear 0 999), Gen.int (Range.linear 0 20000)])
  assert (all (\tr -> let v = actionSpeed gen9ou tr spe in v >= 0 && v <= 8191) [False, True])
  a <- forAll (Gen.int (Range.linear 1809 8191))
  b <- forAll (Gen.int (Range.linear 1809 8191))
  let f tr = actionSpeed gen9ou tr
  if a == b
    then f True a === f True b
    else do
      (f False a < f False b) === (a < b)
      (f True a > f True b) === (a < b)

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
conditionsSupported :: Dex -> Value -> Value -> Value -> Property
conditionsSupported dex conditionsV abilitiesV movesV = withTests 1 $ property $ do
  mapM_ (\c -> do
    let cn = dxConditions dex V.! fromEnum c
    footnote (T.unpack (condName c) ++ " has a handler without a body")
    assert (cnSupported cn)) [minBound .. maxBound :: CondId]
  -- the duration of each condition is the export's: `duration` of its entry in conditions.json, or of the `condition`
  -- block of its ability in abilities.json or of its move in moves.json (flinch and roost have 1; the statuses and the
  -- abilities' volatiles have none)
  let exported c = case [d | Object o <- [entry c], Just (Number d) <- [KM.lookup "duration" o]] of
        (d : _) -> round d
        [] -> 0 :: Int
      entry c = case c of
        CProtosynthesis -> abilityCondition "protosynthesis"
        CQuarkDrive -> abilityCondition "quarkdrive"
        CUnburden -> abilityCondition "unburden"
        CRoost -> lookupKey "condition" (lookupKey "roost" movesV)
        CChillyReception -> lookupKey "condition" (lookupKey "chillyreception" movesV)
        _ -> lookupKey (condName c) conditionsV
      abilityCondition name = lookupKey "condition" (lookupKey name abilitiesV)
      lookupKey k v = case v of
        Object o -> fromMaybe Null (KM.lookup (K.fromText k) o)
        _ -> Null
  map (\c -> cnDuration (dxConditions dex V.! fromEnum c)) [minBound .. maxBound :: CondId] === map exported [minBound .. maxBound]

-- | An ability or item is supported exactly when "Engine.Effects" has bodies for it, and then the handlers the
-- engine runs are exactly those bodies: one for each handler the export lists, and no body for a handler the export
-- does not list (which would be dead code under a name Showdown never calls).
effectsSupported :: Dex -> Property
effectsSupported dex = withTests 1 $ property $ do
  let ty name = V.findIndex (== name) (dxTypeNames dex)
      wx name = V.findIndex ((== name) . cnId) (dxFieldConds dex)
      checkTable what bodiesOf table = mapM_ (\c -> case bodiesOf ty (cnId c) of
        Nothing -> do
          footnote (what ++ " " ++ T.unpack (cnId c) ++ " is supported without bodies")
          assert (not (cnSupported c))
        Just bs -> do
          footnote (what ++ " " ++ T.unpack (cnId c) ++ " has bodies but is not supported: "
            ++ maybe "" T.unpack (cnReject c))
          assert (cnSupported c)
          let run = [(p, e) | (p, e, spec) <- cnHandlers c, Just _ <- [hsBody spec]]
              -- an onStart is also run as the onSwitchIn (battle.ts getCallback 1025-1036)
              written = [(p, e) | (p, e, _) <- bs]
              expected = written ++ [(POwn, EvSwitchIn) | (POwn, EvStart) <- written, (POwn, EvSwitchIn) `notElem` written]
          footnote (what ++ " " ++ T.unpack (cnId c) ++ ": bodies " ++ show written ++ ", run " ++ show run)
          assert (all (`elem` run) expected && all (`elem` expected) run && length run == length expected))
        (V.toList table)
  checkTable "ability" (\t -> abilityBodies t wx) (dxAbilities dex)
  checkTable "item" (\t -> itemBodies t wx) (dxItems dex)

-- | A field condition (side condition, weather, terrain, pseudo-weather) is supported exactly when "Engine.Effects"
-- has bodies for it, and then the handlers the engine runs are exactly those bodies: one for each handler the export
-- lists (a move's @condition.on...@ entries, or the entry of @conditions.json@), and none the export does not list.
-- A move that names a supported condition translates, and one that names an unsupported condition does not.
fieldConditionsSupported :: Dex -> Property
fieldConditionsSupported dex = withTests 1 $ property $ do
  assert (not (V.null (dxFieldConds dex)))
  mapM_ (\c -> case fieldCondBodies (cnId c) of
    Nothing -> do
      footnote ("field condition " ++ T.unpack (cnId c) ++ " is supported without bodies")
      assert (not (cnSupported c))
    Just bs -> do
      footnote ("field condition " ++ T.unpack (cnId c) ++ " has bodies but is not supported: " ++ maybe "" T.unpack (cnReject c))
      assert (cnSupported c)
      let run = [(p, e) | (p, e, spec) <- cnHandlers c, Just _ <- [hsBody spec]]
          written = [(p, e) | (p, e, _) <- bs]
      footnote ("field condition " ++ T.unpack (cnId c) ++ ": bodies " ++ show written ++ ", run " ++ show run)
      assert (all (`elem` run) written && all (`elem` written) run && length run == length written))
    (V.toList (dxFieldConds dex))

-- | A move's own handlers (the export's @handlers@ except the @condition.*@ ones, which belong to the field condition
-- the move defines): the move is supported only with a body for each, and then the handlers the engine runs are exactly
-- the bodies written: one for each handler the export lists (and none the export does not list). A move with none has no
-- bodies, and a move with some but no entry in "Engine.Effects" has none run.
movesSupported :: Dex -> Value -> Property
movesSupported dex movesV = withTests 1 $ property $ do
  entries <- case movesV of
    Object o -> pure (KM.toList o)
    _ -> footnote "moves.json is not an object" >> failure
  let ownNames v = case v of
        Object o | Just (Array hs) <- KM.lookup "handlers" o -> [h | String h <- V.toList hs, not ("condition." `T.isPrefixOf` h)]
        _ -> []
      byKey = sortOn (\(p, e) -> (fromEnum p, fromEnum e))
  mapM_ (\(k, v) -> do
    let name = K.toText k
        names = ownNames v
        listed = byKey [pe | n <- names, Just pe <- [parseHandlerName n]]
    case V.find ((== name) . mvId) (dxMoves dex) of
      Nothing -> footnote (T.unpack name ++ " is not in the dex") >> failure
      Just m -> do
        let run = byKey [(p, e) | (p, e, spec) <- cnHandlers (mvEffect m), Just _ <- [hsBody spec]]
        footnote (T.unpack name ++ ": listed " ++ show names ++ ", run " ++ show run)
        case (names, moveBodies name) of
          ([], _) -> assert (null run)
          (_, Nothing) -> assert (not (cnSupported (mvEffect m)) && null run && not (mvSupported m))
          (_, Just bs) -> do
            let written = byKey [(p, e) | (p, e, _) <- bs]
            -- every handler the export lists has exactly one body, and no body is written for another
            assert (length listed == length names && written == listed)
            assert (run == written)
            assert (cnSupported (mvEffect m))) entries

-- | The fast path of "Engine.Damage".moveTypeMod (no @Effectiveness@ handler: the pure type chart) and the priority and
-- base-power calls are sound only while nothing but a move's own handler listens to these events: an effect is supported
-- only when every handler the export lists for it has a body, so none may have a body for these.
moveEventsOnlyOfMoves :: Dex -> Property
moveEventsOnlyOfMoves dex = withTests 1 $ property $ do
  let moveOnly = [EvEffectiveness, EvModifyPriority, EvModifyType, EvBasePowerCallback, EvDamageCallback, EvTry, EvAfterHit, EvAfterSubDamage, EvHitField, EvTryImmunity]
      tables = [("ability", dxAbilities dex), ("item", dxItems dex), ("condition", dxConditions dex), ("field condition", dxFieldConds dex)]
  mapM_ (\(what, table) -> mapM_ (\c -> do
    footnote (what ++ " " ++ T.unpack (cnId c) ++ " listens to an event only moves run")
    assert (null [() | (_, e, spec) <- cnHandlers c, e `elem` moveOnly, Just _ <- [hsBody spec]])) (V.toList table)) tables

-- | @getTypes()@ is @runEvent('Type', ...)@. The engine keeps its result in 'monType1' and 'monType2' (applied when the Roost
-- volatile starts, undone when it ends), which is exact only while the Roost volatile is the one effect with a handler for
-- the event: an effect is supported only when every handler the export lists for it has a body, so no ability, item, field
-- condition or other volatile may have one, and no move of its own. If one does, the types become a function of more state
-- than the volatile and this fails.
-- | A 'MonadChance' that answers from a supplied list of numbers (each reduced into the requested range) and records
-- the ranges asked for.
newtype Tape a = Tape (State ([Int], [(Int, Int)]) a)

instance Functor Tape where
  fmap f (Tape m) = Tape (fmap f m)

instance Applicative Tape where
  pure = Tape . pure
  Tape f <*> Tape a = Tape (f <*> a)

instance Monad Tape where
  Tape m >>= f = Tape (m >>= \a -> let Tape r = f a in r)

instance MonadChance Tape where
  roll lo hi = Tape $ do
    modify' (\(seeds, seen) -> (seeds, (lo, hi) : seen))
    state (\(seeds, seen) -> case seeds of
      (x : xs) -> (lo + x `mod` (hi - lo), (xs, seen))
      [] -> (lo, ([], seen)))

runTape :: [Int] -> Tape a -> (a, [(Int, Int)])
runTape seeds (Tape m) = case runState m (seeds, []) of (a, (_, seen)) -> (a, reverse seen)

-- | Showdown's @speedSort@ (battle.ts) on a list with ties: selection sort, then @prng.shuffle(list, start, end)@ over
-- each run of equal elements, which draws @random(s, end)@ for @s = start .. end-2@ (prng.ts @shuffle@). So, whatever
-- the draws are: the output is a permutation of the input, sorted under the comparator, and the ranges asked for are
-- exactly those of the tie runs of the sorted keys, in order.
speedSortDraws :: Property
speedSortDraws = property $ do
  keys <- forAll (Gen.list (Range.linear 0 12) (Gen.int (Range.linear 0 4)))
  seeds <- forAll (Gen.list (Range.singleton 80) (Gen.int (Range.linear 0 1000)))
  let items = zip keys [0 :: Int ..]
      cmp (a, _) (b, _) = compare a b
      runs = map length (group (sort keys))
      expected = concat [[(s + i, s + k) | i <- [0 .. k - 2]] | (s, k) <- zip (scanl (+) 0 runs) runs, k > 1]
      (out, ranges) = runTape seeds (speedSortWith cmp items)
  sort out === sort items
  let ks = map fst out
  ks === sort ks
  ranges === expected

-- | 'cnEvents' is what 'condSpec' and the dispatcher's empty-event skip test before reading 'cnHandlers': it must name
-- exactly the events the list has an entry for, and every 'EventId' must have a bit.
eventMasksExact :: Dex -> Property
eventMasksExact dex = withTests 1 $ property $ do
  footnote ("EventId has " ++ show (fromEnum (maxBound :: EventId) + 1) ++ " constructors")
  assert (fromEnum (maxBound :: EventId) < 64)
  let tables =
        [("condition", dxConditions dex), ("ability", dxAbilities dex), ("item", dxItems dex), ("field condition", dxFieldConds dex)]
      -- bit by bit against the handler list, independently of 'eventBits'
      checkMask what c = do
        let named = [e | (_, e, _) <- cnHandlers c]
            wrong = [e | e <- [minBound .. maxBound :: EventId], testBit (cnEvents c) (fromEnum e) /= (e `elem` named)]
        footnote (what ++ " " ++ T.unpack (cnId c) ++ ": events whose bit is wrong: " ++ show wrong)
        wrong === []
        -- no bit beyond the last event
        assert (cnEvents c `shiftR` (fromEnum (maxBound :: EventId) + 1) == 0)
  mapM_ (\(what, table) -> mapM_ (checkMask what) (V.toList table)) tables
  mapM_ (checkMask "move" . mvEffect) (V.toList (dxMoves dex))

onlyRoostListensToType :: Dex -> Property
onlyRoostListensToType dex = withTests 1 $ property $ do
  let typeListeners cn = [(p, e) | (p, e, spec) <- cnHandlers cn, e == EvType, Just _ <- [hsBody spec]]
      tables = [("ability", dxAbilities dex), ("item", dxItems dex), ("field condition", dxFieldConds dex)]
  mapM_ (\(what, table) -> mapM_ (\c -> do
    footnote (what ++ " " ++ T.unpack (cnId c) ++ " listens to the Type event")
    assert (null (typeListeners c))) (V.toList table)) tables
  mapM_ (\mv -> do
    footnote ("the move " ++ T.unpack (mvId mv) ++ " has a Type handler")
    assert (null (typeListeners (mvEffect mv)))) (V.toList (dxMoves dex))
  -- among the conditions, only Roost's volatile
  let listening = [c | c <- [minBound .. maxBound :: CondId], not (null (typeListeners (dxConditions dex V.! fromEnum c)))]
  footnote ("conditions with a Type handler: " ++ show (map condName listening))
  listening === [CRoost]

-- | The fields of "Engine.Rule" that slice 4b added are read by the damage formula, which only a damaging move reaches: a
-- supported move with fixed damage (Seismic Toss, Night Shade, Dragon Rage, Sonic Boom) has no base power in the export (its
-- damage does not depend on it) and is damaging; an override of a stat, a defensive stage ignored or an evasion ignored, and a
-- self boost, belong to damaging moves too.
damageFields :: Dex -> Property
damageFields dex = withTests 1 $ property $
  mapM_ (\mv -> do
    let r = mvRule mv
        damageOnly = mrDamage r /= Nothing || mrOverrideOffensiveStat r /= Nothing || mrOverrideDefensiveStat r /= Nothing
          || mrOverrideOffensivePokemon r || mrIgnoreDefensive r || mrIgnoreEvasion r || not (null (mrSelfBoost r))
    footnote (T.unpack (mvId mv))
    assert (not damageOnly || mrDamaging r)
    assert (case mrDamage r of Nothing -> True; Just DamageLevel -> mvPower mv == 0; Just (DamageFixed _) -> mvPower mv == 0)) (filter mvSupported (V.toList (dxMoves dex)))

-- | @runEvent('BasePower', ..., onEffect)@ puts the move's own @onBasePower@ in the list with order absent, priority 0 and subOrder
-- 0 (docs/handlers.md, "A move's own handlers"). Every other supported BasePower handler has an explicit order or a positive
-- priority, so it sorts strictly before the move's: there is no tie to shuffle (no draw for it) and neither the move's subOrder
-- nor the position at which its @randomChance@ (Fickle Beam) is drawn can change. If a handler with priority 0 and no order is
-- ever supported, this fails and the tie-break (speed of the attacker, subOrder, a shuffle) becomes live and must be recorded.
moveBasePowerSortsLast :: Dex -> Property
moveBasePowerSortsLast dex = withTests 1 $ property $ do
  let tables = [("ability", dxAbilities dex), ("item", dxItems dex), ("condition", dxConditions dex), ("field condition", dxFieldConds dex)]
  mapM_ (\(what, table) -> mapM_ (\c ->
    mapM_ (\(_, e, spec) -> case hsBody spec of
      Just _ | e == EvBasePower -> do
        footnote (what ++ " " ++ T.unpack (cnId c) ++ " has a BasePower handler with no order and no priority")
        assert (hsOrder spec /= 0 || hsPriority spec > 0)
      _ -> pure ()) (cnHandlers c)) (V.toList table)) tables

-- | Two teams of one Pokemon: a battle at team preview whose Pokemon are the given species.
oneVsOne :: Dex -> T.Text -> T.Text -> Maybe Battle
oneVsOne dex a b = do
  t1 <- either (const Nothing) Just (team a)
  t2 <- either (const Nothing) Just (team b)
  pure (fst (runSample (mkSMGen 1) (startBattle dex t1 t2)))
  where
    team sp = parseTeam dex (Array (V.fromList
      [ Object (KM.fromList
          [ ("species", String sp), ("moves", Array (V.fromList [String "tackle"])), ("nature", String "hardy")
          , ("level", Number 100), ("ability", String "noability"), ("item", String "")
          , ("evs", Object (KM.fromList [(K.fromText k, Number 0) | k <- ["hp", "atk", "def", "spa", "spd", "spe"]])) ]) ]))

-- | The base power a callback gives a move used by the first species against the second (team preview state).
callbackPower :: Dex -> T.Text -> Battle -> Int
callbackPower dex move b = case V.find ((== move) . mvId) (dxMoves dex) of
  Just mv -> runIdentity (evalStateT (basePowerCallback dex (Actor P1 0) (Actor P2 0) mv) b)
  Nothing -> 0

-- | Low Kick, Grass Knot and Heavy Slam compare weights (@weighthg@, tenths of a kilogram): the base power never decreases as
-- the target gets heavier (Low Kick, Grass Knot) and never increases with the target's weight, nor decreases with the user's,
-- for Heavy Slam; and it is one of the move's bands.
weightBands :: Dex -> Property
weightBands dex = property $ do
  let species = [spId sp | sp <- V.toList (dxSpecies dex), spSupported sp]
      weightOf name = maybe 0 spWeight (V.find ((== name) . spId) (dxSpecies dex))
  user <- forAll (Gen.element species)
  t1 <- forAll (Gen.element species)
  t2 <- forAll (Gen.element species)
  let (light, heavy) = if weightOf t1 <= weightOf t2 then (t1, t2) else (t2, t1)
  case (oneVsOne dex user light, oneVsOne dex user heavy) of
    (Just bl, Just bh) -> do
      mapM_ (\mv -> do
        assert (callbackPower dex mv bl <= callbackPower dex mv bh)
        assert (callbackPower dex mv bl `elem` [20, 40, 60, 80, 100, 120])) ["lowkick", "grassknot"]
      -- the user is fixed, the target varies: a heavier target never raises Heavy Slam
      assert (callbackPower dex "heavyslam" bl >= callbackPower dex "heavyslam" bh)
      assert (callbackPower dex "heavyslam" bl `elem` [40, 60, 80, 100, 120])
    _ -> footnote "a supported species was not accepted as a team" >> failure
  -- a heavier user never lowers Heavy Slam against a fixed target
  case (oneVsOne dex light t1, oneVsOne dex heavy t1) of
    (Just bl, Just bh) -> assert (callbackPower dex "heavyslam" bl <= callbackPower dex "heavyslam" bh)
    _ -> footnote "a supported species was not accepted as a team" >> failure

-- | The callbacks that add to a base power (Hex against a status, Acrobatics with no item, Stored Power and Power Trip with
-- boosts, Rising Voltage on a grounded target in Electric Terrain) give at least the move's own power, and give exactly that
-- when nothing of the kind is present (team preview state: no status, a boost of 0, the user's item as given, no terrain).
callbacksRaise :: Dex -> Property
callbacksRaise dex = property $ do
  let species = [spId sp | sp <- V.toList (dxSpecies dex), spSupported sp]
  a <- forAll (Gen.element species)
  b <- forAll (Gen.element species)
  case oneVsOne dex a b of
    Nothing -> footnote "a supported species was not accepted as a team" >> failure
    Just battle -> mapM_ (\name -> case V.find ((== name) . mvId) (dxMoves dex) of
      Just mv -> do
        footnote (T.unpack name)
        -- no status, no boost and no terrain: Hex, Stored Power, Power Trip and Rising Voltage give the move's own power
        -- (Acrobatics holds no item here, so it gives more)
        let got = callbackPower dex name battle
        assert (got >= mvPower mv)
        assert (name == "acrobatics" || got == mvPower mv)
      Nothing -> footnote (T.unpack name ++ " is not in the dex") >> failure) ["hex", "storedpower", "powertrip", "risingvoltage", "acrobatics"]

-- | Court Change (data/moves.ts 3035-3101) and Haze (8162-8181) on states reached by random playouts, with side conditions
-- added through 'addSideCondition' and random stages.
--
-- Court Change takes the conditions of its list out of each side (`delete`), then appends the user's to the foe's side and the foe's
-- to the user's, as the same state objects. So, for each side, the result is the conditions it keeps (those not in the list, in their
-- order) followed by the other side's listed conditions in theirs, every state unchanged (duration, layers and effectOrder, so the
-- battle's effect-order counter is untouched), and the move fails exactly when neither side held a listed condition. It makes no
-- draw and changes nothing else. The list is the one in the source; a supported condition missing from it would stay behind.
-- Haze calls `clearBoosts()` on every Pokemon in play that has not fainted (`getAllActive()`): their stages are 0 afterwards and
-- nothing else, nor any other Pokemon, changes.
fieldMoves :: Dex -> Property
fieldMoves dex = property $ do
  (t1, t2) <- forAll (genTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  steps <- forAll (Gen.int (Range.linear 0 120))
  let sideNames = ["stealthrock", "spikes", "toxicspikes", "stickyweb", "reflect", "lightscreen", "tailwind", "auroraveil"]
      listed = [ "mist", "lightscreen", "reflect", "spikes", "safeguard", "tailwind", "toxicspikes", "stealthrock", "waterpledge", "firepledge"
               , "grasspledge", "stickyweb", "auroraveil", "luckychant", "gmaxsteelsurge", "gmaxcannonade", "gmaxvinelash", "gmaxwildfire"
               , "gmaxvolcalith" ] :: [T.Text]
      ids = [i | n <- sideNames, Just i <- [M.lookup n (dxFieldIx dex)]]
  adds <- forAll (Gen.list (Range.linear 0 14) ((,) <$> Gen.element [P1, P2] <*> Gen.element ids))
  stages <- forAll (Gen.list (Range.singleton 84) (Gen.int (Range.linear (-6) 6)))
  who <- forAll Gen.enumBounded
  b0 <- evalEither (fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex steps (const True))))
  let moveNamed name = V.find ((== name) . mvId) (dxMoves dex)
      -- stages for every Pokemon of both sides, from the generated list (7 each, up to 6 per side)
      withStages b = foldr (\p -> modifySide p (\sd -> sd {sMons = SV.imap (\i m -> m {monBoosts = boostsAt (7 * (i + 6 * playerIx p))}) (sMons sd)})) b [P1, P2]
      boostsAt k = case take 7 (drop k stages) of
        [a, b', c, d, e, f, g] -> Boosts a b' c d e f g
        _ -> noBoosts
      user = activeActor who b0
  let ((_, bs), _) = runCounter (runStateT (foldM (\() (p, i) -> () <$ addSideCondition dex p i (activeActor p b0)) () adds) b0)
  let b = withStages bs
      conds p bb = [(i, st) | CondEntry i st <- SV.toList (sConds (sideOf p bb))]
      listedHere p bb = [e | e@(i, _) <- conds p bb, maybe False (\c -> cnId c `elem` listed) (dxFieldConds dex V.!? i)]
      kept p bb = [e | e <- conds p bb, e `notElem` listedHere p bb]
      foe = other who
  -- Court Change
  case moveNamed "courtchange" of
    Nothing -> footnote "courtchange is not in the dex" >> failure
    Just mv -> do
      let ((res, b'), draws) = runCounter (runStateT (moveFieldHit dex user mv) b)
      draws === 0
      conds who b' === kept who b ++ listedHere foe b
      conds foe b' === kept foe b ++ listedHere who b
      bEffectOrder b' === bEffectOrder b
      assert ((res == HitFail) == (null (listedHere who b) && null (listedHere foe b)))
      -- nothing else changed
      let reset p src = modifySide p (\sd -> sd {sConds = sConds (sideOf p src)})
      reset P1 b (reset P2 b b') === b
  -- Haze
  case moveNamed "haze" of
    Nothing -> footnote "haze is not in the dex" >> failure
    Just mv -> do
      let ((_, b'), draws) = runCounter (runStateT (moveFieldHit dex user mv) b)
          inPlay = [(a, True) | p <- [P1, P2], sHasActive (sideOf p b), let a = activeActor p b, not (monFainted (monOf b a))]
          cleared bb = foldr (\(a, _) -> modifyMon a (\m -> m {monBoosts = noBoosts})) bb inPlay
      draws === 0
      mapM_ (\(a, _) -> monBoosts (monOf b' a) === noBoosts) inPlay
      -- only those stages: with them cleared, the states are equal
      cleared b' === cleared b
      -- (a Pokemon that is not in play keeps its stages)
      mapM_ (\p -> mapM_ (\i -> let a = Actor p i in
                if a `elem` map fst inPlay then success else monBoosts (monOf b' a) === monBoosts (monOf b a))
              [0 .. SV.length (sMons (sideOf p b)) - 1]) [P1, P2]

-- | Knock Off's @onAfterHit@ (data/moves.ts 9966), Thief's (19309) and Trick's @onHit@ (19872) on states reached by random
-- playouts, with the two Pokemon in play given random species (Paradox ones among them), abilities (Unburden among them) and
-- items (Booster Energy among them). Each is applied alone, from the same state.
--
-- Invariants, from the source and not from any run: none draws; no item is created (the items in play afterwards are among
-- those before); Booster Energy never stays out of, or arrives in, a Paradox Pokemon's hand (@onTakeItem@: it refuses both
-- being taken from one, and, in Trick and Thief's second @TakeItem@, being given to one); a refused removal keeps the item
-- and an allowed one takes it; Knock Off touches nothing of the user, and the target only in its item, the item state and
-- the Unburden volatile (an Unburden holder gets it whenever it had an item: the ability's @onTakeItem@ runs before the
-- refusal is known); Thief does nothing with a full hand; Trick fails exactly when both hands are empty, a take is refused
-- or the receiver refuses, and otherwise each Pokemon holds the other's old item (or nothing, if it used it on arrival).
itemMoves :: Dex -> Property
itemMoves dex = property $ do
  (t1, t2) <- forAll (genTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  steps <- forAll (Gen.int (Range.linear 4 120))
  who <- forAll Gen.enumBounded
  let pickSpecies = Gen.frequency [(1, Gen.element [spId s | s <- V.toList (dxSpecies dex), spParadox s, spSupported s]), (3, Gen.element (supportedSpecies dex))]
      pickItem = Gen.frequency [(1, pure "boosterenergy"), (3, Gen.element (supportedItems dex))]
  (spU, spT) <- forAll ((,) <$> pickSpecies <*> pickSpecies)
  (abU, abT) <- forAll ((,) <$> Gen.frequency [(1, pure "unburden"), (3, Gen.element (supportedAbilities dex))] <*> Gen.frequency [(1, pure "unburden"), (3, Gen.element (supportedAbilities dex))])
  (itU, itT) <- forAll ((,) <$> pickItem <*> pickItem)
  b0 <- evalEither (fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex steps (const True))))
  let user = activeActor who b0
      target = activeActor (other who) b0
      ix m name = fromMaybe (-1) (M.lookup name (m dex))
      setup a sp ab it = modifyMon a (\m -> m {monSpecies = ix dxSpeciesIx sp, monAbility = ix dxAbilityIx ab, monItem = if T.null it then noItem else ix dxItemIx it, monItemState = noEffectState})
      b = setup target spT abT itT (setup user spU abU itU b0)
      moveNamed name = V.find ((== name) . mvId) (dxMoves dex)
      itemOf bb a = monItem (monOf bb a)
      isParadox bb a = spParadox (dxSpecies dex V.! monSpecies (monOf bb a))
      booster bb a = itemOf bb a /= noItem && itemId dex (itemOf bb a) == "boosterenergy"
      holdsBoosterParadox bb a = isParadox bb a && booster bb a
      present bb = [itemOf bb a | a <- [user, target], itemOf bb a /= noItem]
      sameButItem a bb = (monOf bb a) {monItem = 0, monItemState = noEffectState, monVolatiles = SV.empty}
      hasUnburden bb a = abilityId dex (monAbility (monOf bb a)) == "unburden"
      unburdenOrder bb a = head ([esOrder st | Volatile i st <- SV.toList (monVolatiles (monOf bb a)), i == fromEnum CUnburden] ++ [-1])
      unburdenVolatile bb a = any (\(Volatile i _) -> i == fromEnum CUnburden) (SV.toList (monVolatiles (monOf bb a)))
      active = monActive (monOf b user) && monActive (monOf b target) && monHp (monOf b user) > 0 && monHp (monOf b target) > 0
  case request b0 of
    Finished _ -> success
    Awaiting {} | not active -> success
    Awaiting {} -> do
      -- Knock Off
      case moveNamed "knockoff" of
        Nothing -> footnote "knockoff is not in the dex" >> failure
        Just mv -> do
          let (((), b'), draws) = runCounter (runStateT (moveAfterHit dex user target mv) b)
          draws === 0
          monOf b' user === monOf b user
          assert ((present b' \\ present b) == [])
          if itemOf b target == noItem
            then monOf b' target === monOf b target
            else do
              sameButItem target b' === sameButItem target b
              if hasUnburden b target then assert (unburdenVolatile b' target) else pure ()
              if holdsBoosterParadox b target
                then itemOf b' target === itemOf b target
                else itemOf b' target === noItem
              if itemOf b' target == noItem then monItemState (monOf b' target) === noEffectState else pure ()
      -- Thief
      case moveNamed "thief" of
        Nothing -> footnote "thief is not in the dex" >> failure
        Just mv -> do
          let (((), b'), draws) = runCounter (runStateT (moveAfterHit dex user target mv) b)
          draws === 0
          assert ((present b' \\ present b) == [])
          if itemOf b user /= noItem
            then b' === b
            else do
              assert (itemOf b' target `elem` [itemOf b target, noItem])
              assert (itemOf b' user `elem` [itemOf b target, noItem])
              -- taken whole: an item is never in both hands
              assert (itemOf b' user == noItem || itemOf b' target == noItem)
              -- refused by the holder or by the receiver: nothing moves
              if holdsBoosterParadox b target || (booster b target && isParadox b user)
                then (itemOf b' user, itemOf b' target) === (noItem, itemOf b target)
                else pure ()
      -- Trick and Switcheroo
      mapM_ (\name -> case moveNamed name of
        Nothing -> footnote (T.unpack name ++ " is not in the dex") >> failure
        Just mv -> do
          let ((res, b'), draws) = runCounter (runStateT (fst <$> moveOwnHit dex user target mv) b)
              noneToSwap = itemOf b user == noItem && itemOf b target == noItem
              refused = holdsBoosterParadox b user || holdsBoosterParadox b target
                || (booster b user && isParadox b target) || (booster b target && isParadox b user)
          draws === 0
          assert ((present b' \\ present b) == [])
          assert ((res == HitFail) == (noneToSwap || refused))
          if res == HitFail
            then (itemOf b' user, itemOf b' target) === (itemOf b user, itemOf b target)
            else do
              assert (itemOf b' user `elem` [itemOf b target, noItem])
              assert (itemOf b' target `elem` [itemOf b user, noItem])
          mapM_ (\a -> if isParadox b' a && booster b' a then assert (booster b a) else pure ()) [user, target]
          -- the order of the events (pokemon.ts takeItem and setItem, each of which takes the next effectOrder for the state or
          -- volatile it creates): Trick takes the target's item, then the user's, then sets the user's old item on the target, then
          -- the target's old item on the user. So two Unburden holders that both had items get their volatiles in that order, and two
          -- items that both stay in hand have states in that order. The orders are the battle's counter, which only grows.
          if itemOf b user /= noItem && itemOf b target /= noItem && hasUnburden b user && hasUnburden b target
              && not (unburdenVolatile b user) && not (unburdenVolatile b target)
            then assert (unburdenOrder b' target < unburdenOrder b' user)
            else pure ()
          if res == HitOk && itemOf b' user /= noItem && itemOf b' target /= noItem
            then assert (esOrder (monItemState (monOf b' target)) < esOrder (monItemState (monOf b' user)))
            else pure ()) ["trick", "switcheroo"]
