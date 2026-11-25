{-# LANGUAGE OverloadedStrings #-}
-- | Immutable static data (species, moves, types, natures) loaded once from
-- @data/gen9/*.json@ (exported from Showdown by @tools/oracle/export-dex.js@).
--
-- Battle state refers to entries only by small integer ids that index these
-- tables; names live here, never in the state.
module Engine.Dex
  ( Dex (..)
  , SpeciesData (..)
  , MoveData (..)
  , NatureData (..)
  , FixedGender (..)
  , supportedMoves
  , supportedSpecies
  , loadDex
  , speciesData
  , moveData
  , typeEffect
  , typeCount
  , noType
  , neverMisses
  , toId
  ) where

import Data.Aeson (Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, toLower)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.FilePath ((</>))

-- | A species' fixed gender, if any (Showdown @species.gender@).
data FixedGender = FixedMale | FixedFemale | FixedGenderless | GenderFree
  deriving (Eq, Show)

-- | Per-species static data.
data SpeciesData = SpeciesData
  { spId :: !T.Text
  , spType1 :: !Int
  , spType2 :: !Int
  -- ^ 'noType' when single-typed.
  , spBase :: !(U.Vector Int)
  -- ^ Base stats in the order hp, atk, def, spa, spd, spe.
  , spGender :: !FixedGender
  , spMaxHp :: !Int
  -- ^ Fixed max HP (Shedinja), or 0 when HP follows the stat formula.
  , spSupported :: !Bool
  -- ^ A species the slice is tested on (same criteria as the species pool in tools/oracle/gen-slice0.js).
  }
  deriving (Show)

-- | Per-move static data (only what plain damaging moves use).
data MoveData = MoveData
  { mvId :: !T.Text
  , mvType :: !Int
  , mvPhysical :: !Bool
  , mvPower :: !Int
  , mvAccuracy :: !Int
  -- ^ Percent, or 'neverMisses' when Showdown's accuracy is @true@.
  , mvPriority :: !Int
  , mvCritRatio :: !Int
  , mvPP :: !Int
  , mvNoPPBoosts :: !Bool
  , mvStruggleRecoil :: !Bool
  -- ^ Showdown @struggleRecoil@: recoil of a quarter of max HP, whatever the damage.
  , mvSupported :: !Bool
  -- ^ A plain move the slice supports (same criteria as @isPlainMove@ in tools/oracle/gen-slice0.js).
  }
  deriving (Show)

-- | A nature's boosted and lowered stat (index into atk, def, spa, spd, spe), or -1.
data NatureData = NatureData
  { natPlus :: !Int
  , natMinus :: !Int
  }
  deriving (Show)

-- | The static tables.
data Dex = Dex
  { dxSpecies :: !(V.Vector SpeciesData)
  , dxMoves :: !(V.Vector MoveData)
  , dxStruggle :: !MoveData
  -- ^ Struggle: Showdown's @onModifyMove@ makes it typeless ('noType').
  , dxNatures :: !(V.Vector NatureData)
  , dxTypeNames :: !(V.Vector T.Text)
  , dxTypeChart :: !(U.Vector Int)
  -- ^ @defendingType * typeCount + attackingType@, Showdown codes: 0 neutral, 1 super effective, 2 resisted, 3 immune.
  , dxSpeciesIx :: !(M.Map T.Text Int)
  , dxMoveIx :: !(M.Map T.Text Int)
  , dxNatureIx :: !(M.Map T.Text Int)
  }

-- | Marker for a missing second type.
noType :: Int
noType = -1

-- | 'mvAccuracy' value for moves that never miss (accuracy @true@).
neverMisses :: Int
neverMisses = -1

-- | Showdown's @toID@: lowercase alphanumerics only.
toId :: T.Text -> T.Text
toId = T.map toLower . T.filter isAlphaNum

-- | Number of types in the chart.
typeCount :: Dex -> Int
typeCount = V.length . dxTypeNames

-- | Species by id. Ids are valid by construction (see "Engine.Team").
speciesData :: Dex -> Int -> SpeciesData
speciesData dx i = dxSpecies dx V.! i
{-# INLINE speciesData #-}

-- | Move by id. Ids are valid by construction (see "Engine.Team").
moveData :: Dex -> Int -> MoveData
moveData dx i = dxMoves dx V.! i
{-# INLINE moveData #-}

-- | Showdown's effectiveness code of an attacking type against a defending type.
typeEffect :: Dex -> Int -> Int -> Int
typeEffect dx def atk = dxTypeChart dx U.! (def * typeCount dx + atk)
{-# INLINE typeEffect #-}

-- | Load the dex from a directory holding @species.json@, @moves.json@, @types.json@, @natures.json@.
loadDex :: FilePath -> IO Dex
loadDex dir = do
  speciesV <- load "species.json"
  movesV <- load "moves.json"
  typesV <- load "types.json"
  naturesV <- load "natures.json"
  either (ioError . userError) pure $ do
    tnames <- typeNamesOf typesV
    let tix = M.fromList (zip (V.toList tnames) [0 ..])
    chart <- chartOf tnames typesV
    speciesL <- objectEntries speciesV >>= traverse (speciesOf tix . snd)
    moveEntries <- objectEntries movesV
    tackle <- maybe (Left "moves.json has no tackle") Right (lookup "tackle" moveEntries)
    let plainKeys = case tackle of
          Object o -> Set.fromList (map K.toText (KM.keys o))
          _ -> Set.empty
    movesL <- traverse (moveOf tix plainKeys . snd) moveEntries
    struggleV <- maybe (Left "moves.json has no struggle") Right (lookup "struggle" moveEntries)
    struggle <- (\m -> m {mvType = noType, mvSupported = False}) <$> moveOf tix plainKeys struggleV
    natureEntries <- objectEntries naturesV
    natureL <- traverse (natureOf . snd) natureEntries
    pure Dex
      { dxSpecies = V.fromList speciesL
      , dxMoves = V.fromList movesL
      , dxStruggle = struggle
      , dxNatures = V.fromList natureL
      , dxTypeNames = tnames
      , dxTypeChart = chart
      , dxSpeciesIx = M.fromList (zip (map spId speciesL) [0 ..])
      , dxMoveIx = M.fromList (zip (map mvId movesL) [0 ..])
      , dxNatureIx = M.fromList (zip (map fst natureEntries) [0 ..])
      }
  where
    load f = do
      r <- eitherDecodeFileStrict (dir </> f)
      either (\e -> ioError (userError (dir </> f ++ ": " ++ e))) pure r

objectEntries :: Value -> Either String [(T.Text, Value)]
objectEntries (Object o) = Right [(K.toText k, v) | (k, v) <- KM.toList o]
objectEntries _ = Left "expected a JSON object"

field :: T.Text -> Value -> Either String Value
field k (Object o) = maybe (Left ("missing field " ++ T.unpack k)) Right (KM.lookup (K.fromText k) o)
field k _ = Left ("expected an object with field " ++ T.unpack k)

asInt :: Value -> Either String Int
asInt (Number n) = maybe (Left "expected an integer") Right (toBoundedInteger n)
asInt _ = Left "expected a number"

asArray :: Value -> Either String [Value]
asArray (Array xs) = Right (V.toList xs)
asArray _ = Left "expected an array"

asText :: Value -> Either String T.Text
asText (String t) = Right t
asText _ = Left "expected a string"

typeNamesOf :: Value -> Either String (V.Vector T.Text)
typeNamesOf v = do
  names <- field "names" v
  xs <- asArray names
  V.fromList <$> traverse asText xs

chartOf :: V.Vector T.Text -> Value -> Either String (U.Vector Int)
chartOf names v = do
  dt <- field "damageTaken" v
  rows <- traverse (\n -> field n dt >>= row) (V.toList names)
  pure (U.fromList (concat rows))
  where
    row r = asArray r >>= traverse asInt

speciesOf :: M.Map T.Text Int -> Value -> Either String SpeciesData
speciesOf tix v = do
  ident <- field "id" v >>= asText
  ts <- field "types" v >>= asArray
  -- placeholder types of fakemon (e.g. "Bird") are not in the chart and are dropped
  tys <- (\names -> [i | n <- names, Just i <- [M.lookup n tix]]) <$> traverse asText ts
  bs <- field "baseStats" v
  base <- traverse (\k -> field k bs >>= asInt) ["hp", "atk", "def", "spa", "spd", "spe"]
  g <- field "gender" v >>= asText
  let t1 = case tys of (a : _) -> a; [] -> noType
      t2 = case tys of (_ : b : _) -> b; _ -> noType
      gender = case g of
        "M" -> FixedMale
        "F" -> FixedFemale
        "N" -> FixedGenderless
        _ -> GenderFree
  maxHp <- case v of
    Object o | Just mh <- KM.lookup "maxHP" o, Number _ <- mh -> asInt mh
    _ -> Right 0
  num <- field "num" v >>= asInt
  -- gen-slice0.js: num > 0, not nonstandard, a base forme, not battle-only; plus Shedinja for its fixed HP
  let supported =
        ident == "shedinja"
          || (num > 0 && not (truthyField "isNonstandard" v) && not (truthyField "forme" v) && not (truthyField "battleOnly" v))
  pure (SpeciesData ident t1 t2 (U.fromList base) gender maxHp supported)

typeIx :: M.Map T.Text Int -> T.Text -> Either String Int
typeIx tix t = maybe (Left ("unknown type " ++ T.unpack t)) Right (M.lookup t tix)

moveOf :: M.Map T.Text Int -> Set.Set T.Text -> Value -> Either String MoveData
moveOf tix plainKeys v = do
  ident <- field "id" v >>= asText
  ty <- field "type" v >>= asText >>= typeIx tix
  cat <- field "category" v >>= asText
  bp <- field "basePower" v >>= asInt
  acc <- field "accuracy" v >>= \a -> case a of
    Bool _ -> Right neverMisses
    _ -> asInt a
  prio <- field "priority" v >>= asInt
  crit <- field "critRatio" v >>= asInt
  pp <- field "pp" v >>= asInt
  noBoost <- field "noPPBoosts" v >>= \b -> case b of
    Bool x -> Right x
    _ -> Left "noPPBoosts is not a boolean"
  pure (MoveData ident ty (cat == "Physical") bp acc prio crit pp noBoost (truthyField "struggleRecoil" v) (isPlainMove plainKeys v))

natureOf :: Value -> Either String NatureData
natureOf v = do
  p <- field "plus" v
  m <- field "minus" v
  pure (NatureData (statIx p) (statIx m))
  where
    statIx (String s) = fromMaybe (-1) (lookup s (zip ["atk", "def", "spa", "spd", "spe"] [0 ..]))
    statIx _ = -1

-- | Ids of the moves the slice supports.
supportedMoves :: Dex -> [T.Text]
supportedMoves dx = [mvId m | m <- V.toList (dxMoves dx), mvSupported m]

-- | Ids of the species 'Engine.Team.parseTeam' accepts.
supportedSpecies :: Dex -> [T.Text]
supportedSpecies dx = [spId s | s <- V.toList (dxSpecies dx), spSupported s]

-- | The criteria of @isPlainMove@ (tools/oracle/gen-slice0.js) applied to the exported move data:
-- a damaging single-target move with none of the listed fields set, no handlers, no keys beyond
-- those @tackle@ has (unless falsy) and only inert flags.
isPlainMove :: Set.Set T.Text -> Value -> Bool
isPlainMove plainKeys (Object o) =
  category /= Just "Status"
    && maybe False (> 0) (numberField "basePower")
    && target `elem` [Just "normal", Just "any"]
    && not (any (truthy . lookupKey) mustBeEmpty)
    && not (case lookupKey "secondaries" of Array xs -> not (V.null xs); _ -> False)
    && not (KM.member "handlers" o)
    && all (\(k, x) -> Set.member (K.toText k) plainKeys || not (truthy x)) (KM.toList o)
    && (case lookupKey "flags" of
          Object fl -> all (\k -> K.toText k `elem` inertFlags) (KM.keys fl)
          _ -> True)
  where
    lookupKey k = fromMaybe Null (KM.lookup (K.fromText k) o)
    category = case lookupKey "category" of String t -> Just t; _ -> Nothing
    target = case lookupKey "target" of String t -> Just t; _ -> Nothing
    numberField k = case lookupKey k of Number _ -> either (const Nothing) Just (asInt (lookupKey k)); _ -> Nothing
isPlainMove _ _ = False

-- | JavaScript truthiness of an exported value.
truthy :: Value -> Bool
truthy Null = False
truthy (Bool b) = b
truthy (Number n) = n /= 0
truthy (String t) = not (T.null t)
truthy _ = True

mustBeEmpty :: [T.Text]
mustBeEmpty =
  [ "isNonstandard", "secondary", "damage", "volatileStatus", "ignoreImmunity", "pseudoWeather"
  , "isZ", "isMax", "ohko", "thawsTarget", "heal", "forceSwitch", "selfSwitch", "selfBoost"
  , "selfdestruct", "breaksProtect", "recoil", "drain", "mindBlownRecoil", "stealsBoosts"
  , "struggleRecoil", "overrideOffensivePokemon", "overrideOffensiveStat"
  , "overrideDefensivePokemon", "overrideDefensiveStat", "ignoreAbility", "ignoreAccuracy"
  , "ignoreDefensive", "ignoreEvasion", "ignoreNegativeOffensive", "ignoreOffensive"
  , "ignorePositiveDefensive", "ignorePositiveEvasion", "multiaccuracy", "multihit"
  , "noDamageVariance", "willCrit", "tracksTarget", "sleepUsable", "smartTarget"
  , "stallingMove", "status", "weather", "terrain", "sideCondition", "slotCondition"
  , "boosts", "self", "basePowerCallback", "callsMove", "hasCrashDamage", "forceSTAB"
  , "realMove", "nonGhostTarget", "defensiveCategory"
  ]

inertFlags :: [T.Text]
inertFlags =
  [ "contact", "protect", "mirror", "metronome", "slicing", "punch", "bite", "bullet"
  , "pulse", "wind", "distance", "allyanim", "noassist", "failcopycat", "failmefirst"
  , "nosleeptalk", "failinstruct", "failencore", "failmimic", "nosketch"
  ]

truthyField :: T.Text -> Value -> Bool
truthyField k (Object o) = maybe False truthy (KM.lookup (K.fromText k) o)
truthyField _ _ = False
