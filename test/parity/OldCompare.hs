{-# LANGUAGE OverloadedStrings #-}
-- | The comparison as it was before trace format v2: the engine's battle against a snapshot held as an aeson
-- 'Value'. Kept only for the cross-check ("CrossCheck"), which shows that "Compare" produces the same differences.
module OldCompare (diffState, unmodelled) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Compare (Diff, cmp)
import Engine
import Engine.Damage (monTypes)
import Engine.Dex
import Engine.Event (condName)
import Engine.State

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


-- | All differences between the engine's battle and a trace snapshot, for every field slice 0 models.
diffState :: Dex -> Battle -> Value -> [Diff]
diffState dex b st =
  cmp "turn" (int (st .: "turn")) (Just (bTurn b))
    ++ cmp "request" (txt (st .: "request")) requestName
    ++ cmp "ended" (st .: "ended") (Bool ended)
    ++ cmp "winner" (st .: "winner") winnerValue
    ++ diffField dex (bField b) (st .: "field")
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

-- | The field as the trace records it (record.js @snapshot@): the weather and terrain ids and their durations (null
-- when the state has none), and the pseudo-weathers with their duration.
diffField :: Dex -> Field -> Value -> [Diff]
diffField dex f ev =
  cmp "field.weather" (txt (ev .: "weather")) (condIdName dex (fWeather f))
    ++ cmp "field.weatherDuration" (ev .: "weatherDuration") (durationValue (fWeatherState f))
    ++ cmp "field.terrain" (txt (ev .: "terrain")) (condIdName dex (fTerrain f))
    ++ cmp "field.terrainDuration" (ev .: "terrainDuration") (durationValue (fTerrainState f))
    ++ cmp "field.pseudoWeather" (ev .: "pseudoWeather") (condMap dex (fPseudo f))
  where
    durationValue st = if esDuration st == 0 then Null else Number (fromIntegral (esDuration st))

-- | A field condition's id as Showdown spells it (@''@ for none). An index with no table entry still shows up, so
-- it cannot hide from the compare.
condIdName :: Dex -> Int -> String
condIdName dex i
  | i == noCond = ""
  | otherwise = maybe ("<condition " ++ show i ++ ">") (T.unpack . cnId) (dxFieldConds dex V.!? i)

-- | @conditionMap@ of record.js: each condition with its @duration@ and @layers@ when its state has them.
condMap :: Dex -> SV.Vector CondEntry -> Value
condMap dex es = Object (KM.fromList
  [ (K.fromText (T.pack (condIdName dex i)), Object (KM.fromList
      ([("duration", Number (fromIntegral (esDuration st))) | esDuration st /= 0]
        ++ [("layers", Number (fromIntegral (esA st))) | esA st /= -1])))
  | CondEntry i st <- SV.toList es
  ])

diffSide :: Dex -> Player -> Side -> Value -> [Diff]
diffSide dex p s sd =
  cmp (name ++ ".pokemonLeft") (int (sd .: "pokemonLeft")) (Just (sLeft s))
    ++ cmp (name ++ ".sideConditions") (sd .: "sideConditions") (condMap dex (sConds s))
    ++ cmp (name ++ ".pokemon count") (length expected) (U.length (sSlots s))
    ++ concat
      [ diffMon dex (name ++ ".pokemon[" ++ show slot ++ "]") orig m ev
      | (slot, ev, orig) <- zip3 [0 :: Int ..] expected (U.toList (sSlots s))
      , let m = sMons s SV.! orig
      ]
  where
    name = "sides[" ++ show (playerIx p) ++ "]"
    expected = arr (sd .: "pokemon")

-- | The other @effectState@ keys record.js keeps for a volatile (@counter@, @hp@, @move@; see @snapshotPokemon@), for
-- the volatiles whose state has them, as the engine holds them (docs/handlers.md, "Volatile conditions (slice 7)",
-- the State table). The whole entry is compared, so a key Showdown writes and this table lacks is a failure, never a
-- skip. A move is its Showdown id; -1 is Struggle (@choicelock@ records the active move, which can be Struggle).
volatileKeys :: Dex -> T.Text -> EffectState -> [(K.Key, Value)]
volatileKeys dex name vs = case name of
  "stall" -> [("counter", Number (fromIntegral (esA vs)))]
  "substitute" -> [("hp", Number (fromIntegral (esA vs)))]
  "encore" -> [("move", moveKey)]
  "choicelock" -> [("move", moveKey)]
  _ -> []
  where
    moveKey = String (if esA vs < 0 then "struggle" else mvId (moveData dex (esA vs)))

diffMon :: Dex -> String -> Int -> Mon -> Value -> [Diff]
diffMon dex path orig m ev =
  cmp (path ++ ".orig") (int (ev .: "orig")) (Just orig)
    ++ cmp (path ++ ".species") (txt (ev .: "species")) (T.unpack (spId (speciesData dex (monSpecies m))))
    ++ cmp (path ++ ".level") (int (ev .: "level")) (Just (monLevel m))
    ++ cmp (path ++ ".gender") (txt (ev .: "gender")) (genderName (monGender m))
    ++ cmp (path ++ ".hp") (int (ev .: "hp")) (Just (monHp m))
    ++ cmp (path ++ ".maxhp") (int (ev .: "maxhp")) (Just (monMaxHp m))
    ++ cmp (path ++ ".status") (txt (ev .: "status")) (statusName (monStatus m))
    -- statusState.stage (toxic's counter): present with the value, or absent
    ++ cmp (path ++ ".statusState.stage") (int (ev .: "statusState" .: "stage")) (if esA (monStatusState m) < 0 then Nothing else Just (esA (monStatusState m)))
    -- volatiles: every volatile the engine holds, by name, with its duration as record.js writes it (only when
    -- Showdown's state has one: the engine's 0 is "no duration", since a duration that counts down to 0 removes the
    -- volatile) and its other recorded keys ('volatileKeys'); every other volatile must be absent
    ++ cmp (path ++ ".volatiles") (ev .: "volatiles")
         (Object (KM.fromList
           [ (K.fromText name, Object (KM.fromList ([("duration", Number (fromIntegral (esDuration vs))) | esDuration vs /= 0] ++ volatileKeys dex name vs)))
           | Volatile i vs <- SV.toList (monVolatiles m)
             -- an id with no condition name still shows up (as "<volatile i>"), so it cannot hide from the compare
           , let name = case [condName c | c <- [minBound .. maxBound], fromEnum c == i] of
                   (n : _) -> n
                   [] -> T.pack ("<volatile " ++ show i ++ ">")
           ]))
    ++ cmp (path ++ ".ability") (txt (ev .: "ability")) (T.unpack (abilityId dex (monAbility m)))
    ++ cmp (path ++ ".item") (txt (ev .: "item")) (T.unpack (itemId dex (monItem m)))
    ++ cmp (path ++ ".fainted") (ev .: "fainted") (Bool (monFainted m))
    ++ cmp (path ++ ".active") (ev .: "active") (Bool (monActive m))
    ++ concat
      [ cmp (path ++ ".stats." ++ T.unpack k) (int (ev .: "stats" .: k)) (Just v)
      | (k, v) <- [("atk", stAtk st), ("def", stDef st), ("spa", stSpa st), ("spd", stSpd st), ("spe", stSpe st)]
      ]
    ++ cmp (path ++ ".speed") (int (ev .: "speed")) (Just (monSpeed m))
    ++ concat
      [ cmp (path ++ ".boosts." ++ T.unpack k) (int (ev .: "boosts" .: k)) (Just v)
      | (k, v) <- [("atk", bsAtk bs), ("def", bsDef bs), ("spa", bsSpa bs), ("spd", bsSpd bs), ("spe", bsSpe bs), ("accuracy", bsAccuracy bs), ("evasion", bsEvasion bs)]
      ]
    ++ cmp (path ++ ".boosts key count") (length (objEntries (ev .: "boosts"))) 7
    ++ cmp (path ++ ".types") (map txt (arr (ev .: "types"))) types
    ++ cmp (path ++ ".teraType") (txt (ev .: "teraType")) (typeName (monTeraType m))
    ++ cmp (path ++ ".terastallized") (txt (ev .: "terastallized")) (if monTerastallized m then typeName (monTeraType m) else "")
    ++ cmp (path ++ ".moves.count") (length (arr (ev .: "moves"))) (SV.length (monMoves m))
    ++ concat
      [ cmp (mp ++ ".id") (txt (mv .: "id")) (T.unpack (mvId (moveData dex (msMove ms))))
          ++ cmp (mp ++ ".pp") (int (mv .: "pp")) (Just (msPP ms))
          ++ cmp (mp ++ ".maxpp") (int (mv .: "maxpp")) (Just (msMaxPP ms))
          ++ cmp (mp ++ ".disabled") (mv .: "disabled") (Bool (msDisabled ms))
      | (i, mv, ms) <- zip3 [0 :: Int ..] (arr (ev .: "moves")) (SV.toList (monMoves m))
      , let mp = path ++ ".moves[" ++ show i ++ "]"
      ]
  where
    st = monStats m
    bs = monBoosts m
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
unmodelled :: Value -> [Diff]
unmodelled st =
  concat
      [ concat
            [ let pp = sp ++ ".pokemon[" ++ show j ++ "]"
               in isEmptyObj (pp ++ ".statusState (keys other than stage)") (dropKey "stage" (pk .: "statusState"))
            | (j, pk) <- zip [0 :: Int ..] (arr (sd .: "pokemon"))
            ]
      | (i, sd) <- zip [0 :: Int ..] (arr (st .: "sides"))
      , let sp = "sides[" ++ show i ++ "]"
      ]
  where
    dropKey k (Object o) = Object (KM.delete (K.fromText k) o)
    dropKey _ v = v
    isEmptyObj path v = case v of
      Object o | KM.null o -> []
      _ -> [(path, "{}", txt v)]

objEntries :: Value -> [(K.Key, Value)]
objEntries (Object o) = KM.toList o
objEntries _ = []
