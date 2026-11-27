{-# LANGUAGE OverloadedStrings #-}
-- | Property suite (hedgehog). Properties check invariants and agreement with JavaScript arithmetic; the
-- expected result of a battle comes only from the Showdown traces (see the parity test).
--
-- Environment: @POKESIM_TIER@ = @fast@ (about 100 tests per property) or @long@ (about 2000; the default),
-- @POKESIM_DEX@ (default @data/gen9@).
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (Value (..), eitherDecodeFileStrict')
import Data.Bits (shiftR)
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
import qualified Data.Vector.Unboxed as U
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
import Engine.Dex (Condition (..), Dex (..), EvMask (..), MoveData (..), SpeciesData (..), abilityId, evBit, evMaskHas, moveSpec, prefixMask, dexCondLookup, itemId, supportedAbilities, supportedItems, supportedMoves, supportedSpecies)
import Engine.Dispatch (HitResult (..), addSideCondition, basePowerCallback, damageMon, moveAfterHit, moveFieldHit, moveOwnHit, moveTry)
import Engine.Effects (abilityBodies, fieldCondBodies, itemBodies, moveBodies)
import Engine.Damage (hasAbility, ignoringAbility, moveTypeMod, typeIsNamed)
import Engine.Event (CondId (..), EffectKey (..), EffectKind (..), EventId (..), HandlerSpec (..), Prefix (..), condName, parseHandlerName)
import Engine.Format (trunc, truncBits)
import Engine.Rule (BoostStat (..), DamageKind (..), MoveRule (..), RuleTarget (..), executable, moveRule)
import Engine.State
import Engine.Modifier (baseMod, chain, chainModify, finalModify, modify, modifyBy)
import Engine.Stats (actionSpeed, boostedStat, finalSpeed, parSpe)
import Engine.Turn (getDamage, roundDiv, runMove, stagedAccuracy)
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
      -- the properties with `cover` thresholds on rare events (a few percent of playouts): 100 cases leave a threshold short by chance
      -- often enough to fail a run, so the fast tier runs these for more cases (slice 7). The long tier runs them at n.
      nCover = if tier == ("fast" :: String) then 400 else n
  putStrLn ("props: tier " ++ tier ++ ", " ++ show n ++ " tests per property (" ++ show nCover ++ " for the playouts with coverage thresholds)")
  H.defaultMain
    [ checkSequential $ Group "engine"
        [ ("random playouts keep the decision-point and state invariants", withTests (fromIntegral n) (playouts dex))
        , ("random playouts over teams heavy in self-switch moves, Rocky Helmet, Shedinja, Regenerator, Natural Cure, Eject Button, Red Card and Eject Pack keep the invariants, switch flags and mid-turn requests included",
            withTests (fromIntegral nCover) (pivotPlayouts dex))
        -- slice 8a
        , ("random playouts over teams heavy in the stage and switch-in abilities (Intimidate, Dauntless Shield, Defiant, Contrary, Clear Body, Guard Dog, Inner Focus, Big Pecks, Mirror Armor, Keen Eye, Weak Armor, Justified, Gooey, Speed Boost) and the moves that change stages keep the invariants, the stage range and the once-only Dauntless Shield boost included",
            withTests (fromIntegral nCover) (statPlayouts dex))
        -- slice 7b
        , ("Substitute: a successful use costs exactly a quarter of the maximum HP and leaves a substitute of that size; a failed one changes nothing",
            withTests (fromIntegral nCover) (substituteCost dex))
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
        , ("every effect's event mask, and its mask per prefix, is exactly the events its handler list names, and every event fits the 128-bit mask",
            withTests 1 (eventMasksExact dex))
        , ("Court Change and Haze: the sides exchange the listed conditions with their states and nothing else changes; Haze clears the stages in play",
            withTests (fromIntegral n) (fieldMoves dex))
        , ("Knock Off, Thief and Trick: an item removal never creates an item, a refusal keeps it, and a swap hands each item over whole",
            withTests (fromIntegral n) (itemMoves dex))
        -- slice 8f
        , ("Magic Bounce: a reflectable status move aimed at an unsuppressed holder changes nothing on the holder or its side, and does land on the user",
            withTests (fromIntegral nCover) (magicBounceHolds dex))
        -- slice 7d
        , ("Sucker Punch and Thunderclap: other queued actions never change whether onTry passes, and a failed move makes no accuracy, crit or damage draw and changes no HP",
            withTests (fromIntegral n) (punchMoves dex))
        , ("a weight-based base power never decreases as the target gets heavier or the user lighter",
            withTests (fromIntegral n) (weightBands dex))
        , ("a base-power callback never lowers the move's own base power below what its condition adds",
            withTests (fromIntegral n) (callbacksRaise dex))
        -- slice 8b
        , ("damage abilities: each one only raises or only lowers the damage of the hits it names and leaves every other hit alone, Supreme Overlord rises with the fallen count, and Unaware equals zeroed stages",
            withTests (fromIntegral nCover) (damageAbilities dex))
        -- slice 8c
        , ("random playouts over teams heavy in the immunity and guard abilities, their items and their moves keep the invariants, and reach what they guard against",
            withTests (fromIntegral nCover) (guardPlayouts dex))
        , ("Magic Guard, Sturdy and Poison Heal change a damage only where their handler acts: every other damage is the one a holder with no ability takes",
            withTests (fromIntegral nCover) (damageGuards dex))
        , ("Good as Gold, Water Absorb, Sap Sipper, Soundproof, Bulletproof and Levitate block a move before any other effect: the move changes nothing but the holder's HP (Water Absorb) or Attack (Sap Sipper)",
            withTests (fromIntegral nCover) (immunityAbilities dex))
        , ("Shell Armor: a move deals no more to its holder than to a Pokemon with no ability, whatever the crit roll, and draws the same",
            withTests (fromIntegral nCover) (shellArmor dex))
        -- slice 8d
        , ("random playouts over small teams heavy in Flame Body, Static, Rough Skin, Toxic Chain, Synchronize, Magician, Pickpocket, Berserk, Moxie, Shield Dust and Bad Dreams keep the invariants, and no item is ever created",
            withTests (fromIntegral nCover) (abilityPlayouts dex))
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
    Right (b, _) -> case request b of
      Finished _ -> success
      Awaiting {} -> footnote "playout ended without a result" >> failure

-- | 'playouts' over 'genPivotTeams': the switch-flag invariants of 'checkBattle' and the mid-turn ones of 'checkStep' (slice 6a), the
-- force-switch ones (slice 6b) and the switch-out ones (slice 6c: Regenerator, Natural Cure, Eject Button, Red Card) and Eject Pack (6d: a used pack leaves its holder flagged, benched or fainted; `eject` never survives to a decision point), are checked at every state and step, and these teams reach the requests and drags they are about at every
-- few turns (the coverage statement fails the property if they stop doing so).
pivotPlayouts :: Dex -> Property
pivotPlayouts dex = property $ do
  (t1, t2) <- forAll (genPivotTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  case fst (runSample (mkSMGen seed) (playChecked dex t1 t2)) of
    Left e -> footnote e >> failure
    Right (b, seen) -> do
      cover 10 "a forced switch (drag) happened" (seenDrags seen > 0)
      cover 5 "a Regenerator holder below max HP switched out" (seenRegen seen > 0)
      cover 2 "a statused Natural Cure holder switched out" (seenCure seen > 0)
      cover 2 "an Eject Button was used" (seenEject seen > 0)
      cover 3 "a Red Card was used" (seenRedCard seen > 0)
      cover 3 "an Eject Pack was used" (seenPack seen > 0)
      cover 1 "an Eject Pack was used by a Pokemon as it switched in" (seenPackIn seen > 0)
      -- slice 7a: the protect family succeeded, and again with the stall volatile in place (the counter climbed)
      cover 20 "a protect move succeeded (a stall volatile was held)" (seenStall seen > 0)
      cover 5 "a protect move succeeded with a stall counter of 9 or more" (seenStallClimb seen > 0)
      -- slice 7e
      cover 20 "a Pokemon was locked by a Choice item at a move request" (seenLocked seen > 0)
      cover 1 "a locked Pokemon had no PP left in its move (Struggle)" (seenLockStruggle seen > 0)
      -- slice 7c
      cover 10 "a move request came with an active Pokemon encored" (seenEncore seen > 0)
      cover 10 "a move request came with an active Pokemon taunted" (seenTaunt seen > 0)
      -- slice 7b: a Pokemon that had no Substitute has one after the step, and a substitute that was hit stays or breaks
      cover 10 "a Substitute was made" (seenSub seen > 0)
      cover 2 "a Substitute was hit" (seenSubHit seen > 0)
      -- slice 7f: a move request with a Pokemon under two disabling effects at once (the exact-moves check of checkBattle then combines them)
      cover 7 "a move request came with an active Pokemon under two disabling effects" (seenMultiDisable seen > 0)
      -- slice 8e: a Pokemon lost 2 PP to one move (a Pressure holder on the other side), and a Libero or Protean holder had changed its type
      cover 10 "a move cost 2 PP (Pressure)" (seenPressure seen > 0)
      cover 10 "a Libero or Protean holder changed its type" (seenTypeChange seen > 0)
      -- slice 8f: suppression and reflection
      cover 15 "an unfainted Neutralizing Gas holder was in play beside a foe at the end of a step" (seenGasOn seen > 0)
      cover 8 "a Neutralizing Gas holder left play" (seenGasLeft seen > 0)
      cover 5 "a reflectable status move was chosen into an unsuppressed Magic Bounce holder" (seenBounceTried seen > 0)
      cover 3 "a Mold Breaker user chose a move against an unsuppressed breakable ability" (seenBreaker seen > 0)
      case request b of
        Finished _ -> success
        Awaiting {} -> footnote "playout ended without a result" >> failure

-- | slice 8a: 'playouts' over 'genStatTeams'. 'checkBattle' and 'checkStep' hold every stage in -6..6 and a Dauntless Shield boost to a
-- holder of the ability that never goes back; the coverage statements fail the property if these teams stop reaching the abilities.
statPlayouts :: Dex -> Property
statPlayouts dex = property $ do
  (t1, t2) <- forAll (genStatTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  case fst (runSample (mkSMGen seed) (playChecked dex t1 t2)) of
    Left e -> footnote e >> failure
    Right (b, seen) -> do
      cover 5 "a Dauntless Shield holder spent its boost" (seenShieldBoost seen > 0)
      cover 5 "an Intimidate holder switched in" (seenIntimidateIn seen > 0)
      cover 10 "a holder of Clear Body, Guard Dog, Inner Focus, Big Pecks, Mirror Armor, Defiant or Contrary ended a step with a stage change" (seenDropAbility seen > 0)
      case request b of
        Finished _ -> success
        Awaiting {} -> footnote "playout ended without a result" >> failure

-- | slice 8d. 'playouts' over 'genAbilityTeams': the invariants of 'checkBattle' (Berserk's flag among them) and 'checkStep' (no step creates an
-- item: Magician, Pickpocket, Thief and Trick only move them) at every state and step. The covers keep the generator reaching the events of
-- the abilities: a Moxie holder boosted after a knockout, an item taken by Magician or Pickpocket (or Thief, Trick) into an empty hand, and a
-- Berserk holder that lost HP and had its Special Attack rise.
abilityPlayouts :: Dex -> Property
abilityPlayouts dex = property $ do
  (t1, t2) <- forAll (genAbilityTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  case fst (runSample (mkSMGen seed) (playChecked dex t1 t2)) of
    Left e -> footnote e >> failure
    Right (b, seen) -> do
      cover 1 "a Moxie holder's Attack rose in a step where the foe fainted" (seenMoxie seen > 0)
      cover 1 "a Magician or Pickpocket holder with no item had one after the step" (seenThief seen > 0)
      cover 2 "a Berserk holder lost HP and its Special Attack rose" (seenBerserk seen > 0)
      case request b of
        Finished _ -> success
        Awaiting {} -> footnote "playout ended without a result" >> failure

-- | ESCAPE HATCH: substitute (slice 7b). A Pokemon that uses Substitute with the foe switching (so that nothing else hits it) loses
-- exactly @floor(maxhp / 4)@ and has a substitute with that HP, or, when the move fails (it has one already, its HP is at most a quarter
-- of the maximum, or its maximum is 1), nothing changes. The rest of the turn must not touch its HP: no status, no weather or terrain at
-- the end of the step (set by the replacement's ability), no residual-healing or residual-damage item, and it stays in play.
substituteCost :: Dex -> Property
substituteCost dex = property $ do
  (t1, t2) <- forAll (genPivotTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  k <- forAll (Gen.int (Range.linear 0 40))
  let reached = fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex k (const True)))
  b <- evalEither reached
  case request b of
    Awaiting MoveRequest True True -> do
      p <- forAll (Gen.element [P1, P2])
      let foe = if p == P1 then P2 else P1
          m = monOf b (activeActor p b)
          moveId j = [mvId (dxMoves dex V.! msMove ms) | Just ms <- [monMoves m SV.!? (j - 1)]]
          -- (a Move choice is Struggle, not the move in that slot, when every move is disabled or out of PP, as for a Substitute user that
          -- is Taunted (slice 7c): Struggle's recoil is a quarter of the maximum HP too, so only a slot that is usable counts)
          usable j = maybe False (\ms -> not (msDisabled ms) && msPP ms > 0) (monMoves m SV.!? (j - 1))
          subChoices = [c | c@(Move j) <- legalChoices b p, moveId j == ["substitute"], usable j]
          switches = [c | c@(Switch _) <- legalChoices b foe]
          residualItem = itemId dex (monItem m) `elem` ["leftovers", "blacksludge", "flameorb", "toxicorb"]
          hasSub x = any (\(Volatile i _) -> i == fromEnum CSubstitute) (SV.toList (monVolatiles x))
      case (subChoices, switches) of
        (c : _, sw : _) | monStatus m == StatusNone, not residualItem -> do
          let cs = if p == P1 then (Just c, Just sw) else (Just sw, Just c)
          b' <- evalEither (either (Left . show) Right (fst (runSample (mkSMGen seed) (step dex b cs))))
          let m' = monOf b' (activeActor p b')
              -- (a weather or terrain that was up before the step and ended at its Residual still hurt or healed the user there:
              -- Grassy Terrain's heal, a sandstorm's chip: so none may be up before the step either)
              fieldQuiet = all (\x -> fWeather (bField x) == noCond && fTerrain (bField x) == noCond) [b, b']
              q = monMaxHp m `quot` 4
              works = not (hasSub m) && 4 * monHp m > monMaxHp m && monMaxHp m /= 1
              subHp = [esA st | Volatile i st <- SV.toList (monVolatiles m'), i == fromEnum CSubstitute]
          cover 1 "a Substitute was tried in isolation" True
          if fieldQuiet && monActive m' && monHp m' > 0
            then if works then (monHp m' === monHp m - q) >> (subHp === [q]) else (monHp m' === monHp m) >> (hasSub m' === hasSub m)
            else success
        _ -> success
    _ -> success

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
        -- slice 7a: the volatiles of the protect family are their moves' condition blocks (stall is in conditions.json)
        CProtect -> lookupKey "condition" (lookupKey "protect" movesV)
        CSpikyShield -> lookupKey "condition" (lookupKey "spikyshield" movesV)
        CBanefulBunker -> lookupKey "condition" (lookupKey "banefulbunker" movesV)
        CEndure -> lookupKey "condition" (lookupKey "endure" movesV)
        -- slice 7c
        CEncore -> lookupKey "condition" (lookupKey "encore" movesV)
        CTaunt -> lookupKey "condition" (lookupKey "taunt" movesV)
        CSubstitute -> lookupKey "condition" (lookupKey "substitute" movesV) -- slice 7b
        CFlashFire -> abilityCondition "flashfire" -- slice 8b
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
  -- (slice 8e: ModifyPriority and ModifyType left the list: Prankster and Liquid Voice listen, and 'movePriorityEvent' and 'useMove' run both through runEvent)
  let moveOnly = [EvEffectiveness, EvBasePowerCallback, EvDamageCallback, EvTry, EvAfterHit, EvAfterSubDamage, EvHitField, EvTryImmunity]
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
  assert (fromEnum (maxBound :: EventId) < 128)
  let tables =
        [("condition", dxConditions dex), ("ability", dxAbilities dex), ("item", dxItems dex), ("field condition", dxFieldConds dex)]
      -- bit by bit against the handler list, independently of 'eventBits'
      checkMask what c = do
        let named = [e | (_, e, _) <- cnHandlers c]
            wrong = [e | e <- [minBound .. maxBound :: EventId], evMaskHas (cnEvents c) (evBit (fromEnum e)) /= (e `elem` named)]
        footnote (what ++ " " ++ T.unpack (cnId c) ++ ": events whose bit is wrong: " ++ show wrong)
        wrong === []
        -- the per-prefix masks, bit by bit against the same list
        let wrongBy =
              [ (p, e)
              | p <- [minBound .. maxBound :: Prefix]
              , e <- [minBound .. maxBound :: EventId]
              , evMaskHas (prefixMask c p) (evBit (fromEnum e)) /= any (\(p', e', _) -> p' == p && e' == e) (cnHandlers c)
              ]
        footnote (what ++ " " ++ T.unpack (cnId c) ++ ": (prefix, event) pairs whose bit is wrong: " ++ show wrongBy)
        wrongBy === []
        U.length (cnEventsBy c) === 2 * (fromEnum (maxBound :: Prefix) + 1)
        -- no bit beyond the last event
        let EvMask lo hi = cnEvents c
        assert (hi `shiftR` max 0 (fromEnum (maxBound :: EventId) + 1 - 64) == 0)
        assert (fromEnum (maxBound :: EventId) >= 63 || lo `shiftR` (fromEnum (maxBound :: EventId) + 1) == 0)
        assert (fromEnum (maxBound :: EventId) + 1 > 64 || hi == 0)
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
              -- (slice 8f: not while Neutralizing Gas ignores the target's ability: its onTakeItem is skipped)
              if hasUnburden b target && not (ignoringAbility dex b target) then assert (unburdenVolatile b' target) else pure ()
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
               && not (ignoringAbility dex b user) && not (ignoringAbility dex b target) -- (slice 8f: Neutralizing Gas)
              && not (unburdenVolatile b user) && not (unburdenVolatile b target)
            then assert (unburdenOrder b' target < unburdenOrder b' user)
            else pure ()
          if res == HitOk && itemOf b' user /= noItem && itemOf b' target /= noItem
            then assert (esOrder (monItemState (monOf b' target)) < esOrder (monItemState (monOf b' user)))
            else pure ()) ["trick", "switcheroo"]

-- | ESCAPE HATCH: magicbounce (slice 8f). A move from 'bounceMoves' (reflectable, a status move whose effect is only on its target) that the
-- user of a one-Pokemon team uses against a Magic Bounce holder whose ability nothing suppresses (the user has neither Mold Breaker nor
-- Neutralizing Gas) is sent back at its user: the holder, and the conditions on its side, are exactly as they were (a bounced use
-- never reaches it, and the move cannot bounce twice), whichever side is the user. The bounced use changes the user (a status, a stage, a
-- volatile, a hazard on its side) in a good share of the cases; a move that fails on the user (an immune type, no last move for Encore)
-- changes nothing.
magicBounceHolds :: Dex -> Property
magicBounceHolds dex = property $ do
  (tu, th) <- forAll (genBounceTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  who <- forAll Gen.enumBounded
  let (t1, t2) = if who == P1 then (tu, th) else (th, tu)
  b0 <- evalEither (fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex 1 (const True))))
  let user = activeActor who b0
      holder = activeActor (other who) b0
      slot0 = monMoves (monOf b0 user) SV.!? 0
  case (request b0, slot0) of
    (Awaiting MoveRequest _ _, Just ms)
      | monActive (monOf b0 user) && monActive (monOf b0 holder) && monHp (monOf b0 user) > 0 && monHp (monOf b0 holder) > 0
      , hasAbility dex b0 holder "magicbounce", msMove ms /= -1 -> do
          let (((), b'), _) = runSample (mkSMGen seed) (runStateT (runMove (bFormat b0) dex user 0) b0)
              holderSide x = sConds (sideOf (other who) x)
              userSide x = sConds (sideOf who x)
              m0 = monOf b0 user
              m' = monOf b' user
              landed = monStatus m' /= monStatus m0 || monBoosts m' /= monBoosts m0 || monVolatiles m' /= monVolatiles m0 || userSide b' /= userSide b0
                || not (monActive m') || monHp m' /= monHp m0
          monOf b' holder === monOf b0 holder
          holderSide b' === holderSide b0
          cover 20 "the bounced move changed the user or its side" landed
    _ -> success

-- | Sucker Punch and Thunderclap (slice 7d). The move's @onTry@ reads only the target's queued @move@ actions (@queue.willMove@), so the
-- result of 'moveTry' is the same whatever else the queue holds: the user's own actions, the target's switch or Terastallization, a
-- residual. And a move that fails there stops before the accuracy roll: running it makes no accuracy, crit or damage draw (only the speed-tie shuffles of the events before Try), and no HP changes, on either side.
punchMoves :: Dex -> Property
punchMoves dex = property $ do
  (t1, t2) <- forAll (genPivotTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  steps <- forAll (Gen.int (Range.linear 0 40))
  who <- forAll Gen.enumBounded
  name <- forAll (Gen.element ["suckerpunch", "thunderclap" :: T.Text])
  b0 <- evalEither (fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex steps (const True))))
  let user = activeActor who b0
      target = activeActor (other who) b0
      ix = fromMaybe (-1) (M.lookup name (dxMoveIx dex))
      mv = dxMoves dex V.! ix
      act k a arg = Action {acKind = k, acOrder = 0, acPriority = 0, acSpeed = 0, acActor = Just a, acArg = arg, acPranksterBoosted = False}
      nMoves = SV.length (monMoves (monOf b0 target))
  case request b0 of
    Finished _ -> success
    Awaiting {} | not (monActive (monOf b0 user) && monActive (monOf b0 target) && monHp (monOf b0 user) > 0 && monHp (monOf b0 target) > 0) -> success
    Awaiting {} -> do
      -- the actions that count (the target's moves, possibly Struggle) and the ones that must not
      relevant <- forAll (Gen.list (Range.linear 0 2) (act KMove target <$> Gen.int (Range.linear (-1) (nMoves - 1))))
      noise <- forAll (Gen.list (Range.linear 0 4) (Gen.choice
        [ act KMove user <$> Gen.int (Range.linear 0 3)
        , act KSwitch target <$> Gen.int (Range.linear 0 5)
        , pure (act KTerastallize target 0)
        , pure (act KResidual target 0)
        ]))
      interleaved <- forAll (Gen.shuffle (relevant ++ noise))
      let clean m = m {monStatus = StatusNone, monVolatiles = SV.empty}
          withQueue q = (modifyMon user (\m -> (clean m) {monMoves = SV.singleton (MoveSlot ix 5 5 False)}) b0) {bQueue = SV.fromList q}
          bA = withQueue interleaved
          bB = withQueue [a | a <- interleaved, acKind a == KMove, acActor a == Just target]
          tryOf bb = fst (runCounter (evalStateT (moveTry dex user target mv) bb))
      tryOf bA === tryOf bB
      cover 10 "onTry fails" (not (tryOf bA))
      cover 10 "onTry passes" (tryOf bA)
      if tryOf bA
        then success
        else do
          let (((), b'), draws) = runTape [] (runStateT (runMove (bFormat bA) dex user 0) bA)
          -- (speed-tie shuffles of the events before Try (BeforeMove, ModifyMove ...) are draws of a few values; the accuracy roll, the
          -- crit roll and the damage roll, random(100), random(24) and random(16), are never made)
          footnote (show draws)
          assert (all (\(lo, hi) -> hi - lo < 16) draws)
          monHp (monOf b' user) === monHp (monOf bA user)
          monHp (monOf b' target) === monHp (monOf bA target)

-- | The damage abilities of slice 8b (Multiscale, Sharpness, Tinted Lens, Guts, Transistor, Sniper, Heatproof, Purifying Salt, Water Bubble, Flash
-- Fire, Supreme Overlord, Vessel of Ruin, Unaware), through 'getDamage' on a state from a random playout: the two Pokemon in play lose their
-- abilities, items, statuses and volatiles, get random stages, and a random damaging move is used with the same draws in every variant. Each
-- ability then joins one Pokemon, and the damage is compared with the damage without it. The invariants are the sign of each ability's
-- modifier (every modifier is a chain of factors at or above 1, or at or below 1, and the damage formula is monotone in them), and that the hits an
-- ability does not name come out unchanged:
--
-- * Multiscale halves a hit on a full-HP holder and leaves one on a holder below full alone; Heatproof, Purifying Salt and Water Bubble only lower
--   the hits of their type (Fire, Ghost, Fire) and Vessel of Ruin a Special hit; Sharpness, Tinted Lens, Transistor, Sniper, Water Bubble's Water
--   boost, Flash Fire's boost and Supreme Overlord only raise a slicing move, a resisted hit, an Electric move, any hit, a Water move, a Fire move
--   and any hit (Guts: a Physical hit by a statused holder, and nothing else);
-- * Supreme Overlord's damage does not fall as the fallen count rises, and is the plain damage with no count;
-- * two Vessel of Ruin holders, or one attacking, leave the damage as it is;
-- * Unaware on the target gives the damage of an attacker whose Attack, Defense and Special Attack stages are 0, and on the user the damage against
--   a target whose Defense and Special Defense stages are 0.
damageAbilities :: Dex -> Property
damageAbilities dex = property $ do
  (t1, t2) <- forAll (genTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  steps <- forAll (Gen.int (Range.linear 0 40))
  who <- forAll Gen.enumBounded
  mvIx <- forAll (Gen.element damaging)
  draws <- forAll (Gen.list (Range.singleton 8) (Gen.int (Range.linear 0 1000)))
  boostsU <- forAll genBoosts
  boostsT <- forAll genBoosts
  status <- forAll (Gen.element [StatusNone, StatusBrn, StatusPar, StatusPsn, StatusTox, StatusFrz])
  b0 <- evalEither (fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex steps (const True))))
  let user = activeActor who b0
      target = activeActor (other who) b0
      mv = dxMoves dex V.! mvIx
      ix name = fromMaybe (-1) (M.lookup name (dxAbilityIx dex))
      clean m = m {monAbility = ix "noability", monAbilityState = noEffectState, monItem = noItem, monItemState = noEffectState, monStatus = StatusNone, monStatusState = noEffectState, monVolatiles = SV.empty}
      base = modifyMon target (\m -> (clean m) {monBoosts = boostsT}) (modifyMon user (\m -> (clean m) {monBoosts = boostsU}) b0)
      -- the damage of a state, with the move in use set as the move loop does
      dmg st = fst (fst (runTape draws (evalStateT (getDamage (bFormat st) dex user target mv) st {bActiveMove = Just (MoveInUse user False)})))
      with a name = modifyMon a (\m -> m {monAbility = ix name})
      -- the target at full HP, and one below it (when it has an HP to lose)
      full = modifyMon target (\m -> m {monHp = monMaxHp m})
      below = modifyMon target (\m -> m {monHp = max 1 (monMaxHp m - 1)})
      statused = modifyMon user (\m -> m {monStatus = status})
      tm = moveTypeMod dex mv (monOf base target)
      isT t = typeIsNamed dex (mvType mv) t
      physical = mvPhysical mv
      rule = mvRule mv
      -- an ability on one Pokemon: the damage is at or above, or at or below, the damage without it
      atLeast st a name = let (d0, d1) = (dmg st, dmg (with a name st)) in footnote (T.unpack name ++ ": " ++ show d0 ++ " -> " ++ show d1) >> assert (d1 >= d0)
      atMost st a name = let (d0, d1) = (dmg st, dmg (with a name st)) in footnote (T.unpack name ++ ": " ++ show d0 ++ " -> " ++ show d1) >> assert (d1 <= d0)
      same st a name = let (d0, d1) = (dmg st, dmg (with a name st)) in footnote (T.unpack name ++ ": " ++ show d0 ++ " -> " ++ show d1) >> d1 === d0
      -- for each: the hits it names move the damage one way, the others leave it alone
      only cond dir st a name = if cond then dir st a name else same st a name
  case request b0 of
    Finished _ -> success
    Awaiting {} | user == target || not (all (\a -> monActive (monOf b0 a) && monHp (monOf b0 a) > 0) [user, target]) -> success
    Awaiting {} -> do
      -- Multiscale
      only True atMost (full base) target "multiscale"
      if monMaxHp (monOf base target) > 1 then same (below base) target "multiscale" else success
      cover 5 "Multiscale lowered the damage of a hit on a full-HP holder" (dmg (with target "multiscale" (full base)) < dmg (full base))
      -- the plain modifiers by the hit they name
      only (mrSlicing rule) atLeast base user "sharpness"
      only (tm < 0) atLeast base user "tintedlens"
      only (isT "Electric") atLeast base user "transistor"
      atLeast base user "sniper"
      only (isT "Fire") atMost base target "heatproof"
      only (isT "Ghost") atMost base target "purifyingsalt"
      only (isT "Fire") atMost base target "waterbubble"
      only (isT "Water") atLeast base user "waterbubble"
      -- Guts: a Physical hit by a holder with a status; a Special hit, and a holder with none, are as without it
      only (physical && status /= StatusNone) atLeast (statused base) user "guts"
      -- Flash Fire's volatile, on a holder of the ability
      let lit = modifyMon user (\m -> m {monVolatiles = SV.singleton (Volatile (fromEnum CFlashFire) noEffectState)})
      only (isT "Fire") atLeast (lit base) user "flashfire"
      -- Supreme Overlord: no count, or each count from 1 to 5; the damage never falls as the count rises
      let overlord k = modifyMon user (\m -> m {monAbility = ix "supremeoverlord", monAbilityState = noEffectState {esA = k}}) base
          byCount = map (dmg . overlord) [1 .. 5]
      dmg (overlord (-1)) === dmg base
      assert (and (zipWith (<=) byCount (drop 1 byCount)) && all (>= dmg base) byCount)
      cover 5 "Supreme Overlord raised the damage" (last byCount > dmg base)
      -- Vessel of Ruin: a Special hit on a holder is lowered; a Physical hit is not; a holder that attacks, or two holders, change nothing
      only (not physical) atMost base target "vesselofruin"
      same base user "vesselofruin"
      -- (two holders with the same speed tie in the handler sort, which draws: the draws that follow then differ, as in Showdown)
      if monSpeed (monOf base user) /= monSpeed (monOf base target)
        then dmg (with user "vesselofruin" (with target "vesselofruin" base)) === dmg base else success
      -- Unaware equals the stages it ignores: the target's, the Attack, Defense and Special Attack of the user (and the Attack of the target for
      -- Foul Play, whose Attack is the target's); the user's, the Defense and Special Defense of the target
      let zeroU m = m {monBoosts = (monBoosts m) {bsAtk = 0, bsDef = 0, bsSpa = 0}}
          zeroT m = m {monBoosts = (monBoosts m) {bsDef = 0, bsSpd = 0}}
          zeroTAtk m = m {monBoosts = (monBoosts m) {bsAtk = 0}}
          ignoredByTarget = modifyMon user zeroU (if mrOverrideOffensivePokemon rule then modifyMon target zeroTAtk base else base)
          ignoredByUser = modifyMon target zeroT base
      -- (a base-power callback may read the stages themselves, as Stored Power counts the user's positive ones; Unaware leaves those alone)
      if moveSpec mv POwn EvBasePowerCallback == Nothing
        then dmg (with target "unaware" base) === dmg ignoredByTarget >> dmg (with user "unaware" base) === dmg ignoredByUser
        else success
      cover 5 "Unaware changed the damage of a hit against a boosted attacker" (dmg (with target "unaware" base) /= dmg base)
      cover 5 "Unaware changed the damage of a hit into a boosted target" (dmg (with user "unaware" base) /= dmg base)
  where
    -- the moves 'getDamage' computes the damage of: a damaging move with no fixed damage and no damage callback
    damaging =
      [ i
      | (i, m) <- zip [0 ..] (V.toList (dxMoves dex))
      , mvSupported m, mrDamaging (mvRule m), mrDamage (mvRule m) == Nothing, mvPower m > 0
      , moveSpec m POwn EvDamageCallback == Nothing
      ]
    genBoosts = Boosts <$> stage <*> stage <*> stage <*> stage <*> stage <*> stage <*> stage
    stage = Gen.int (Range.linear (-6) 6)
-- ---------------------------------------------------------------------------
-- slice 8c

-- | 'playouts' over 'genGuardTeams': every state and step is checked as in 'pivotPlayouts', and the teams reach the guard abilities at work
-- (the coverage statement fails the property if they stop doing so).
guardPlayouts :: Dex -> Property
guardPlayouts dex = property $ do
  (t1, t2) <- forAll (genGuardTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  case fst (runSample (mkSMGen seed) (playChecked dex t1 t2)) of
    Left e -> footnote e >> failure
    Right (b, seen) -> do
      cover 1 "a Magic Guard holder went through a step with burn, poison or toxic" (seenGuarded seen > 0)
      cover 0.3 "a Water Absorb holder gained HP in a step" (seenAbsorbed seen > 0)
      case request b of
        Finished _ -> success
        Awaiting {} -> footnote "playout ended without a result" >> failure

-- | The state a slice 8c property starts from: a battle a few steps in, with a side's active Pokemon (the target) and its foe (the user) both
-- in play with HP.
reachedBattle :: Dex -> PropertyT IO (Maybe (Battle, Player))
reachedBattle dex = do
  (t1, t2) <- forAll (genPivotTeams dex)
  seed <- forAll (Gen.word64 Range.linearBounded)
  steps <- forAll (Gen.int (Range.linear 0 40))
  who <- forAll Gen.enumBounded
  b0 <- evalEither (fst (runSample (mkSMGen seed) (startBattle dex t1 t2 >>= advanceWhile dex steps (const True))))
  let inPlay p = sHasActive (sideOf p b0) && monActive (monOf b0 (activeActor p b0)) && monHp (monOf b0 (activeActor p b0)) > 0
  pure (case request b0 of
    Awaiting {} | inPlay who && inPlay (other who) -> Just (b0, who)
    _ -> Nothing)

-- | The ability with this id, as 'monAbility' holds it.
abilityIx :: Dex -> T.Text -> Int
abilityIx dex name = fromMaybe (-1) (M.lookup name (dxAbilityIx dex))

-- | A Pokemon given an ability (as 'hasAbility' reads it), with no volatile or item that has a Damage or TryHit handler.
withAbility :: Dex -> T.Text -> Mon -> Mon
withAbility dex name m = m {monAbility = abilityIx dex name, monBaseAbility = abilityIx dex name, monVolatiles = SV.empty, monItem = noItem}

-- | @spreadDamage@ (battle.ts 2107-2154), through 'damageMon': what a damage deals to a Pokemon with Magic Guard, Sturdy or Poison Heal
-- differs from what a Pokemon with no ability takes only where the ability's handler (data/abilities.ts) acts:
--
-- * Magic Guard: a damage whose effect is not a Move deals nothing (the event fails); a Move's damage is unchanged.
-- * Sturdy: a Move's damage of at least the HP, at full HP, leaves 1 HP; every other damage is unchanged.
-- * Poison Heal: a damage whose effect is psn or tox deals nothing and heals the holder (never lowers its HP, raises it when it was
--   below the maximum); every other damage is unchanged.
--
-- The effects are every kind of source the engine deals damage from.
damageGuards :: Dex -> Property
damageGuards dex = property $ do
  r <- reachedBattle dex
  case r of
    Nothing -> success
    Just (b0, who) -> do
      guardName <- forAll (Gen.frequency [(2, pure "magicguard"), (3, pure "sturdy"), (2, pure ("poisonheal" :: T.Text))])
      -- (a move's damage, and psn and tox, weigh more: they are the effects the three abilities act on)
      cause <- forAll (Gen.frequency [(4, pure (EffectKey EffMove 0)), (3, Gen.element (filter (`elem` [EffectKey EffStatus (fromEnum CPsn), EffectKey EffStatus (fromEnum CTox)]) (map fst causes))), (6, Gen.element (map fst causes))])
      full <- forAll Gen.bool
      hpFrac <- forAll (Gen.int (Range.linear 1 100))
      let target = activeActor who b0
          m0 = monOf b0 target
          hp0 = if full then monMaxHp m0 else max 1 (monMaxHp m0 * hpFrac `quot` 100)
      -- (the amount is drawn around the HP, so that it is sometimes less, sometimes exactly, sometimes more)
      amount <- forAll (Gen.frequency [(1, pure 0), (3, Gen.int (Range.linear 1 (max 1 hp0))), (2, pure hp0), (3, Gen.int (Range.linear hp0 (2 * hp0)))])
      let start name = modifyMon target (\m -> (withAbility dex name m) {monHp = hp0}) b0
          run name = let ((res, b'), _) = runCounter (runStateT (damageMon dex target Nothing Nothing (Just cause) amount) (start name))
                      in (res, monHp (monOf b' target))
          (resG, hpG) = run guardName
          (resN, hpN) = run "noability"
          isMove = cause == EffectKey EffMove 0
          isPoison = cause `elem` [EffectKey EffStatus (fromEnum CPsn), EffectKey EffStatus (fromEnum CTox)]
          clamped = if amount == 0 then 0 else max 1 amount
          savesFromFull = isMove && hp0 == monMaxHp m0 && clamped >= hp0
      footnote ("cause " ++ show cause ++ ", amount " ++ show amount ++ ", hp " ++ show hp0 ++ "/" ++ show (monMaxHp m0) ++ ": " ++ show (resG, hpG) ++ " against " ++ show (resN, hpN))
      case guardName of
        "magicguard"
          | isMove -> (resG, hpG) === (resN, hpN)
          | otherwise -> hpG === hp0
        "sturdy"
          | savesFromFull -> hpG === 1
          | otherwise -> (resG, hpG) === (resN, hpN)
        _
          | isPoison -> do
              assert (hpG >= hp0)
              if hp0 < monMaxHp m0 then assert (hpG > hp0) else hpG === hp0
          | otherwise -> (resG, hpG) === (resN, hpN)
      cover 5 "Magic Guard met a damage that is not a move's" (guardName == "magicguard" && not isMove)
      cover 1 "Sturdy saved a full-HP holder" (guardName == "sturdy" && savesFromFull)
      cover 1 "Sturdy met a move's damage it does not change" (guardName == "sturdy" && isMove && not savesFromFull)
      cover 1 "Poison Heal met psn or tox damage below full HP" (guardName == "poisonheal" && isPoison && hp0 < monMaxHp m0)
      cover 5 "Poison Heal met another damage" (guardName == "poisonheal" && not isPoison)
  where
    item n = EffectKey EffItem (fromMaybe 0 (M.lookup n (dxItemIx dex)))
    field n = EffectKey EffSideCond (fromMaybe 0 (M.lookup n (dxFieldIx dex)))
    weather n = EffectKey EffWeather (fromMaybe 0 (M.lookup n (dxFieldIx dex)))
    causes =
      [ (EffectKey EffMove 0, "a move")
      , (EffectKey EffStatus (fromEnum CBrn), "burn")
      , (EffectKey EffStatus (fromEnum CPsn), "poison")
      , (EffectKey EffStatus (fromEnum CTox), "toxic")
      , (EffectKey EffRecoil 0, "recoil")
      , (item "lifeorb", "Life Orb")
      , (item "rockyhelmet", "Rocky Helmet")
      , (item "blacksludge", "Black Sludge")
      , (field "stealthrock", "Stealth Rock")
      , (field "spikes", "Spikes")
      , (weather "sandstorm", "sandstorm")
      , (EffectKey EffVolatile (fromEnum CSpikyShield), "Spiky Shield")
      ] :: [(EffectKey, T.Text)]

-- | The moves an immunity ability blocks (data/abilities.ts), on a single foe: Good as Gold a Status move, Water Absorb a Water move,
-- Sap Sipper a Grass move, Soundproof a sound move, Bulletproof a bullet move, Levitate a damaging Ground move that does not ignore immunity.
blockedBy :: Dex -> T.Text -> MoveData -> Bool
blockedBy dex ability mv = mvSupported mv && mrTarget (mvRule mv) == TargetFoe && case ability of
  "goodasgold" -> mvStatus mv
  "waterabsorb" -> typeIsNamed dex (mvType mv) "Water"
  "sapsipper" -> typeIsNamed dex (mvType mv) "Grass"
  "soundproof" -> mrSound (mvRule mv)
  "bulletproof" -> mrBullet (mvRule mv)
  "levitate" -> typeIsNamed dex (mvType mv) "Ground" && mrDamaging (mvRule mv) && not (mrIgnoreImmunity (mvRule mv))
  _ -> False

-- | An immunity ability stops a move at @TryHit@ (or, Levitate, at the type-immunity step), before the accuracy roll and every effect
-- of the move: the target is unchanged except for the HP Water Absorb heals and the Attack Sap Sipper raises, the user loses no HP
-- (no recoil, no Life Orb), and no draw at all is made, not even a speed-tie shuffle (the two sides each have one Pokemon in play).
immunityAbilities :: Dex -> Property
immunityAbilities dex = property $ do
  r <- reachedBattle dex
  case r of
    Nothing -> success
    Just (b0, who) -> do
      ability <- forAll (Gen.element ["goodasgold", "waterabsorb", "sapsipper", "soundproof", "bulletproof", "levitate" :: T.Text])
      let user = activeActor (other who) b0
          target = activeActor who b0
          eligible = [(i, mv) | (i, mv) <- zip [0 ..] (V.toList (dxMoves dex)), blockedBy dex ability mv]
      (ix, mv) <- forAll (Gen.element eligible)
      let clean m = (withAbility dex "noability" m) {monStatus = StatusNone}
          b1 = modifyMon target (withAbility dex ability) (modifyMon user (\m -> (clean m) {monMoves = SV.singleton (MoveSlot ix 5 5 False)}) b0) {bQueue = SV.empty}
          (((), b'), draws) = runTape [] (runStateT (runMove (bFormat b1) dex user 0) b1)
          t0 = monOf b1 target
          t1 = monOf b' target
          atk m = boostOf BAtk (monBoosts m)
          sameButHp x y = x {monHp = 0} == y {monHp = 0}
          sameButHpAtk x y = x {monHp = 0, monBoosts = setBoostOf BAtk 0 (monBoosts x)} == y {monHp = 0, monBoosts = setBoostOf BAtk 0 (monBoosts y)}
      footnote (T.unpack (mvId mv) ++ " into " ++ T.unpack ability ++ ", draws " ++ show draws)
      draws === []
      monHp (monOf b' user) === monHp (monOf b1 user)
      case ability of
        "waterabsorb" -> assert (sameButHp t0 t1 && monHp t1 >= monHp t0)
        "sapsipper" -> assert (sameButHpAtk t0 t1 && monHp t1 == monHp t0 && atk t1 >= atk t0 && atk t1 <= atk t0 + 1)
        _ -> assert (t0 == t1)
      cover 5 "Good as Gold blocked a status move" (ability == "goodasgold")
      cover 5 "Water Absorb blocked a Water move" (ability == "waterabsorb")
      cover 5 "Sap Sipper blocked a Grass move" (ability == "sapsipper")
      cover 5 "Soundproof blocked a sound move" (ability == "soundproof")
      cover 5 "Bulletproof blocked a bullet move" (ability == "bulletproof")
      cover 5 "Levitate blocked a Ground move" (ability == "levitate")

-- | Shell Armor cancels a critical hit after the roll (battle-actions.ts 1704-1706), so the draws are the same and the damage never greater.
-- With every draw 0 the crit roll succeeds for a move that has one, so the Pokemon with no ability takes the critical hit.
shellArmor :: Dex -> Property
shellArmor dex = property $ do
  r <- reachedBattle dex
  case r of
    Nothing -> success
    Just (b0, who) -> do
      let user = activeActor (other who) b0
          target = activeActor who b0
          eligible = [ (i, mv) | (i, mv) <- zip [0 ..] (V.toList (dxMoves dex)), mvSupported mv, mrDamaging (mvRule mv), mrTarget (mvRule mv) == TargetFoe
                     , mrDamage (mvRule mv) == Nothing, mvCritRatio mv >= 1 ]
      (ix, mv) <- forAll (Gen.element eligible)
      let clean m = (withAbility dex "noability" m) {monStatus = StatusNone}
          prepared name = modifyMon target (\m -> (withAbility dex name m) {monHp = monMaxHp m}) (modifyMon user (\m -> (clean m) {monMoves = SV.singleton (MoveSlot ix 5 5 False)}) b0) {bQueue = SV.empty}
          run name = let b1 = prepared name
                         (((), b'), draws) = runTape [] (runStateT (runMove (bFormat b1) dex user 0) b1)
                      in (monMaxHp (monOf b1 target) - monHp (monOf b' target), draws, monHp (monOf b' target) > 0)
          (lossShell, drawsShell, aliveShell) = run "shellarmor"
          (lossPlain, drawsPlain, alivePlain) = run "noability"
      footnote (T.unpack (mvId mv) ++ ": " ++ show lossShell ++ " against " ++ show lossPlain)
      -- (a hit that the critical hit would have fainted the target with leaves a multi-hit move no later hit to draw for)
      if aliveShell && alivePlain then drawsShell === drawsPlain else success
      assert (lossShell <= lossPlain)
      cover 15 "the critical hit would have changed the damage" (lossShell < lossPlain)
