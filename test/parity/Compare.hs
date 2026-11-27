{-# LANGUAGE OverloadedStrings #-}
-- | The engine's battle against a recorded snapshot ('Snap'): every field the trace records, compared as typed values.
-- A difference is a field path, the expected value and the engine's, rendered as aeson's 'Value' shows them (the
-- rendering the JSON-based comparison used), so a failure reads the same whichever format the trace came from.
module Compare
  ( Diff
  , showDiff
  , cmp
  , diffState
  , unmodelled
  ) where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine
import Engine.Damage (monTypes)
import Engine.Dex
import Engine.Event (condName)
import Engine.State
import Trace

-- | A difference between expected (trace) and actual (engine) values: field path, expected, actual.
type Diff = (String, String, String)

showDiff :: Diff -> String
showDiff (path, e, a) = path ++ ": expected " ++ e ++ ", engine has " ++ a

-- | Compare a field.
cmp :: (Eq a, Show a) => String -> a -> a -> [Diff]
cmp path e a = [(path, show e, show a) | e /= a]

-- | An integer field, shown as the JSON comparison showed it (@Just n@).
cmpI :: String -> Int -> Int -> [Diff]
cmpI path e a = cmp path (Just e) (Just a)

-- | A boolean field, shown as a JSON boolean.
cmpB :: String -> Bool -> Bool -> [Diff]
cmpB path e a = [(path, show (Bool e), show (Bool a)) | e /= a]

-- | An integer or null.
cmpMI :: String -> Maybe Int -> Maybe Int -> [Diff]
cmpMI path e a = [(path, show (maybeIntToV1 e), show (maybeIntToV1 a)) | e /= a]

-- | A condition map, shown as the JSON object.
cmpConds :: String -> M.Map T.Text Cond -> M.Map T.Text Cond -> [Diff]
cmpConds path e a = [(path, show (condsToV1 e), show (condsToV1 a)) | e /= a]

-- | All differences between the engine's battle and a trace snapshot, for every field slice 0 models.
diffState :: Dex -> Battle -> Snap -> [Diff]
diffState dex b st =
  cmpI "turn" (snTurn st) (bTurn b)
    ++ cmp "request" (T.unpack (snRequest st)) requestName
    ++ cmpB "ended" (snEnded st) ended
    ++ [("winner", show (maybe Null String (snWinner st)), show winnerValue) | maybe Null String (snWinner st) /= winnerValue]
    ++ diffField dex (bField b) (snField st)
    ++ concat [diffSide dex p (sideOf p b) sd | (p, sd) <- [(P1, snP1 st), (P2, snP2 st)]]
  where
    (requestName, ended, winnerValue) = case request b of
      Finished w -> ("", True, case w of Winner P1 -> String "p1"; Winner P2 -> String "p2"; Tie -> String "tie")
      Awaiting TeamPreview _ _ -> ("teampreview", False, Null)
      Awaiting MoveRequest _ _ -> ("move", False, Null)
      Awaiting SwitchRequest _ _ -> ("switch", False, Null)

-- | The field as the trace records it (record.js @snapshot@): the weather and terrain ids and their durations (null
-- when the state has none), and the pseudo-weathers with their duration.
diffField :: Dex -> Field -> FieldSnap -> [Diff]
diffField dex f ev =
  cmp "field.weather" (T.unpack (fsWeather ev)) (condIdName dex (fWeather f))
    ++ cmpMI "field.weatherDuration" (fsWeatherDuration ev) (durationOf (fWeatherState f))
    ++ cmp "field.terrain" (T.unpack (fsTerrain ev)) (condIdName dex (fTerrain f))
    ++ cmpMI "field.terrainDuration" (fsTerrainDuration ev) (durationOf (fTerrainState f))
    ++ cmpConds "field.pseudoWeather" (fsPseudo ev) (condMap dex (fPseudo f))
  where
    durationOf st = if esDuration st == 0 then Nothing else Just (esDuration st)

-- | A field condition's id as Showdown spells it (@''@ for none). An index with no table entry still shows up, so
-- it cannot hide from the compare.
condIdName :: Dex -> Int -> String
condIdName dex i
  | i == noCond = ""
  | otherwise = maybe ("<condition " ++ show i ++ ">") (T.unpack . cnId) (dxFieldConds dex V.!? i)

-- | @conditionMap@ of record.js: each condition with its @duration@ and @layers@ when its state has them.
condMap :: Dex -> SV.Vector CondEntry -> M.Map T.Text Cond
condMap dex es = M.fromList
  [ ( T.pack (condIdName dex i)
    , Cond (if esDuration st /= 0 then Just (esDuration st) else Nothing) (if esA st /= -1 then Just (esA st) else Nothing) )
  | CondEntry i st <- SV.toList es
  ]

diffSide :: Dex -> Player -> Side -> SideSnap -> [Diff]
diffSide dex p s sd =
  cmpI (name ++ ".pokemonLeft") (ssLeft sd) (sLeft s)
    ++ cmpConds (name ++ ".sideConditions") (ssConds sd) (condMap dex (sConds s))
    ++ cmp (name ++ ".pokemon count") (length expected) (U.length (sSlots s))
    ++ concat
      [ diffMon dex (name ++ ".pokemon[" ++ show slot ++ "]") orig m ev
      | (slot, ev, orig) <- zip3 [0 :: Int ..] expected (U.toList (sSlots s))
      , let m = sMons s SV.! orig
      ]
  where
    name = "sides[" ++ show (playerIx p) ++ "]"
    expected = ssMons sd

-- | The other @effectState@ keys record.js keeps for a volatile (@counter@, @hp@, @move@; see @snapshotPokemon@), for
-- the volatiles whose state has them, as the engine holds them (docs/handlers.md, "Volatile conditions (slice 7)",
-- the State table). The whole entry is compared, so a key Showdown writes and this table lacks is a failure, never a
-- skip. A move is its Showdown id; -1 is Struggle (@choicelock@ records the active move, which can be Struggle).
volatileKeys :: Dex -> T.Text -> EffectState -> [(T.Text, Scalar)]
volatileKeys dex name vs = case name of
  "stall" -> [("counter", SInt (esA vs))]
  "substitute" -> [("hp", SInt (esA vs))]
  "encore" -> [("move", moveKey)]
  "choicelock" -> [("move", moveKey)]
  _ -> []
  where
    moveKey = SText (if esA vs < 0 then "struggle" else mvId (moveData dex (esA vs)))

diffMon :: Dex -> String -> Int -> Mon -> MonSnap -> [Diff]
diffMon dex path orig m ev =
  cmpI (path ++ ".orig") (mOrig ev) orig
    ++ cmp (path ++ ".species") (T.unpack (mSpecies ev)) (T.unpack (spId (speciesData dex (monSpecies m))))
    ++ cmpI (path ++ ".level") (mLevel ev) (monLevel m)
    ++ cmp (path ++ ".gender") (T.unpack (mGender ev)) (genderName (monGender m))
    ++ cmpI (path ++ ".hp") (mHp ev) (monHp m)
    ++ cmpI (path ++ ".maxhp") (mMaxHp ev) (monMaxHp m)
    ++ cmp (path ++ ".status") (T.unpack (mStatus ev)) (statusName (monStatus m))
    -- statusState.stage (toxic's counter): present with the value, or absent
    ++ cmp (path ++ ".statusState.stage") (stageOf (mStatusState ev)) (if esA (monStatusState m) < 0 then Nothing else Just (esA (monStatusState m)))
    -- volatiles: every volatile the engine holds, by name, with its duration as record.js writes it (only when
    -- Showdown's state has one: the engine's 0 is "no duration", since a duration that counts down to 0 removes the
    -- volatile) and its other recorded keys ('volatileKeys'); every other volatile must be absent
    ++ [ (path ++ ".volatiles", show (volatilesToV1 (mVolatiles ev)), show (volatilesToV1 volatiles))
       | mVolatiles ev /= volatiles ]
    ++ cmp (path ++ ".ability") (T.unpack (mAbility ev)) (T.unpack (abilityId dex (monAbility m)))
    ++ cmp (path ++ ".item") (T.unpack (mItem ev)) (T.unpack (itemId dex (monItem m)))
    ++ cmpB (path ++ ".fainted") (mFainted ev) (monFainted m)
    ++ cmpB (path ++ ".active") (mActive ev) (monActive m)
    ++ concat
      [ cmpI (path ++ ".stats." ++ k) e v
      | (k, e, v) <-
          [ ("atk", ea, stAtk st), ("def", ed, stDef st), ("spa", esa, stSpa st), ("spd", esd, stSpd st), ("spe", ese, stSpe st) ]
      ]
    ++ cmpI (path ++ ".speed") (mSpeed ev) (monSpeed m)
    ++ concat
      [ cmpI (path ++ ".boosts." ++ k) e v
      | (k, e, v) <-
          [ ("atk", ba, bsAtk bs), ("def", bd, bsDef bs), ("spa", bsa, bsSpa bs), ("spd", bsd, bsSpd bs), ("spe", bse, bsSpe bs)
          , ("accuracy", bac, bsAccuracy bs), ("evasion", bev, bsEvasion bs) ]
      ]
    ++ cmp (path ++ ".types") (map T.unpack (mTypes ev)) types
    ++ cmp (path ++ ".teraType") (T.unpack (mTeraType ev)) (typeName (monTeraType m))
    ++ cmp (path ++ ".terastallized") (T.unpack (mTerastallized ev)) (if monTerastallized m then typeName (monTeraType m) else "")
    ++ cmp (path ++ ".moves.count") (length (mMoves ev)) (SV.length (monMoves m))
    ++ concat
      [ cmp (mp ++ ".id") (T.unpack (moId mv)) (T.unpack (mvId (moveData dex (msMove ms))))
          ++ cmpI (mp ++ ".pp") (moPP mv) (msPP ms)
          ++ cmpI (mp ++ ".maxpp") (moMaxPP mv) (msMaxPP ms)
          ++ cmpB (mp ++ ".disabled") (moDisabled mv) (msDisabled ms)
      | (i, mv, ms) <- zip3 [0 :: Int ..] (mMoves ev) (SV.toList (monMoves m))
      , let mp = path ++ ".moves[" ++ show i ++ "]"
      ]
  where
    st = monStats m
    bs = monBoosts m
    StatsSnap ea ed esa esd ese = mStats ev
    BoostsSnap ba bd bsa bsd bse bac bev = mBoosts ev
    stageOf ss = case M.lookup "stage" ss of
      Just (SInt i) -> Just i
      _ -> Nothing
    volatiles = M.fromList
      [ (name, M.fromList ([("duration", SInt (esDuration vs)) | esDuration vs /= 0] ++ volatileKeys dex name vs))
      | Volatile i vs <- SV.toList (monVolatiles m)
        -- an id with no condition name still shows up (as "<volatile i>"), so it cannot hide from the compare
      , let name = case [condName c | c <- [minBound .. maxBound], fromEnum c == i] of
              (n : _) -> n
              [] -> T.pack ("<volatile " ++ show i ++ ">")
      ]
    typeName t = T.unpack (dxTypeNames dex V.! t)
    -- record.js snapshots `p.getTypes()` (the Type event applied), so the engine's getTypes is compared, not the stored types
    (ty1, ty2) = monTypes dex m
    types = typeName ty1 : [typeName ty2 | ty2 /= noType]
    genderName g = case g of GenderM -> "M"; GenderF -> "F"; GenderNone -> ""
    statusName s = case s of
      StatusNone -> ""
      StatusFainted -> "fnt"
      StatusBrn -> "brn"
      StatusPar -> "par"
      StatusPsn -> "psn"
      StatusTox -> "tox"
      StatusFrz -> "frz"

-- | Fields of a snapshot that the engine does not model must hold their defaults, so a trace that uses an
-- out-of-slice mechanic fails loudly instead of going unchecked.
unmodelled :: Snap -> [Diff]
unmodelled st =
  concat
    [ [ (sp ++ ".pokemon[" ++ show j ++ "].statusState (keys other than stage)", "{}", show (entriesToV1 rest))
      | (j, pk) <- zip [0 :: Int ..] (ssMons sd)
      , let rest = M.delete "stage" (mStatusState pk)
      , not (M.null rest)
      ]
    | (i, sd) <- [(0 :: Int, snP1 st), (1, snP2 st)]
    , let sp = "sides[" ++ show i ++ "]"
    ]
