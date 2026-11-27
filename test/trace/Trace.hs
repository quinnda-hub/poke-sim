{-# LANGUAGE OverloadedStrings #-}
-- | A typed model of a v1 trace (the JSON that @tools/oracle/record.js@ writes), and the two total conversions
-- between it and aeson's 'Value'.
--
-- 'fromV1' rejects every deviation from record.js's schema, with the path of the offending value. 'toV1' rebuilds
-- the JSON. The law that makes trace format v2 ("Trace.V2") lossless is, for every trace @v@ in the corpus,
--
-- > toV1 <$> fromV1 v == Right v
--
-- (aeson 'Value' equality). This is test infrastructure, not engine state: it uses 'Text' and 'Map' freely.
module Trace
  ( Trace (..)
  , Step (..)
  , TDraw (..)
  , Snap (..)
  , FieldSnap (..)
  , SideSnap (..)
  , MonSnap (..)
  , StatsSnap (..)
  , BoostsSnap (..)
  , MoveSnap (..)
  , Cond (..)
  , Scalar (..)
  , emptySnap
  , fromV1
  , toV1
  , snapToV1
  , condsToV1
  , volatilesToV1
  , entriesToV1
  , maybeIntToV1
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Vector as V

-- | One random draw: the half-open range @[lo, hi)@ and the result.
data TDraw = TDraw !Int !Int !Int
  deriving (Eq, Show)

-- | A whole trace.
data Trace = Trace
  { trVersion :: !Int
  , trShowdownVersion :: !Text
  , trFormat :: !Text
  , trSeed :: !Text
  , trPolicySeed :: !Int
  , trTera :: !(Maybe Bool)
  -- ^ The @tera@ key: absent, or present with its value (record.js writes it only as @true@).
  , trTruncated :: !Bool
  , trTeams :: !Value
  -- ^ Kept as recorded; small, and 'Engine.Team.parseTeam' and the coverage check take JSON.
  , trSteps :: [Step]
  , trFinalDraws :: [TDraw]
  , trFinal :: !Snap
  }
  deriving (Eq, Show)

-- | One decision point: the draws since the previous one, the state, the legal choices and the choices taken (one
-- per side; 'Nothing' for a side that is not asked).
data Step = Step
  { stDraws :: [TDraw]
  , stState :: !Snap
  , stLegal :: !([Text], [Text])
  , stChosen :: !(Maybe Text, Maybe Text)
  }
  deriving (Eq, Show)

-- | An int or text scalar: the values of a volatile's recorded keys.
data Scalar = SInt !Int | SText !Text
  deriving (Eq, Show)

-- | @conditionMap@'s entry: the @duration@ and @layers@ keys, when present.
data Cond = Cond
  { cDuration :: !(Maybe Int)
  , cLayers :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | The state snapshot of record.js @snapshot@, with exactly two sides.
data Snap = Snap
  { snTurn :: !Int
  , snRequest :: !Text
  , snEnded :: !Bool
  , snWinner :: !(Maybe Text)
  , snField :: !FieldSnap
  , snP1 :: !SideSnap
  , snP2 :: !SideSnap
  }
  deriving (Eq, Show)

data FieldSnap = FieldSnap
  { fsWeather :: !Text
  , fsWeatherDuration :: !(Maybe Int)
  , fsTerrain :: !Text
  , fsTerrainDuration :: !(Maybe Int)
  , fsPseudo :: !(Map Text Cond)
  }
  deriving (Eq, Show)

data SideSnap = SideSnap
  { ssId :: !Text
  , ssLeft :: !Int
  , ssConds :: !(Map Text Cond)
  , ssMons :: [MonSnap]
  }
  deriving (Eq, Show)

data StatsSnap = StatsSnap !Int !Int !Int !Int !Int
  deriving (Eq, Show)

-- | atk, def, spa, spd, spe, accuracy, evasion.
data BoostsSnap = BoostsSnap !Int !Int !Int !Int !Int !Int !Int
  deriving (Eq, Show)

data MoveSnap = MoveSnap
  { moId :: !Text
  , moPP :: !Int
  , moMaxPP :: !Int
  , moDisabled :: !Bool
  }
  deriving (Eq, Show)

-- | @snapshotPokemon@.
data MonSnap = MonSnap
  { mOrig :: !Int
  , mSpecies :: !Text
  , mLevel :: !Int
  , mGender :: !Text
  , mHp :: !Int
  , mMaxHp :: !Int
  , mStatus :: !Text
  , mStatusState :: !(Map Text Scalar)
  , mFainted :: !Bool
  , mActive :: !Bool
  , mStats :: !StatsSnap
  , mBoosts :: !BoostsSnap
  , mSpeed :: !Int
  , mTypes :: [Text]
  , mTeraType :: !Text
  , mTerastallized :: !Text
  , mAbility :: !Text
  , mItem :: !Text
  , mMoves :: [MoveSnap]
  , mVolatiles :: !(Map Text (Map Text Scalar))
  }
  deriving (Eq, Show)

-- | The snapshot before the first step, which "Trace.V2" deltas the first real one against: no Pokemon at all.
emptySnap :: Snap
emptySnap = Snap 0 "" False Nothing (FieldSnap "" Nothing "" Nothing M.empty) emptySide emptySide
  where
    emptySide = SideSnap "" 0 M.empty []

-- ---------------------------------------------------------------------------
-- JSON -> Trace

type P a = Either String a

-- | The keys of an object, which must be exactly the required ones plus any of the optional ones.
object :: String -> [K.Key] -> [K.Key] -> Value -> P (KM.KeyMap Value)
object path req opt v = case v of
  Object o -> do
    mapM_ (\k -> if KM.member k o then Right () else Left (path ++ "." ++ K.toString k ++ ": missing key")) req
    mapM_ (\k -> if k `elem` req || k `elem` opt then Right () else Left (path ++ "." ++ K.toString k ++ ": unexpected key")) (KM.keys o)
    Right o
  _ -> Left (path ++ ": expected an object")

-- | A key's value (the key is known to be present, or the caller handles absence).
at :: KM.KeyMap Value -> K.Key -> Value
at o k = case KM.lookup k o of
  Just x -> x
  Nothing -> Null

sub :: String -> K.Key -> String
sub p k = p ++ "." ++ K.toString k

int :: String -> Value -> P Int
int path v = case v of
  Number n | Just i <- toBoundedInteger n -> Right i
  _ -> Left (path ++ ": expected an integer, got " ++ show v)

text :: String -> Value -> P Text
text path v = case v of
  String t -> Right t
  _ -> Left (path ++ ": expected a string, got " ++ show v)

bool :: String -> Value -> P Bool
bool path v = case v of
  Bool b -> Right b
  _ -> Left (path ++ ": expected a boolean, got " ++ show v)

list :: String -> (String -> Value -> P a) -> Value -> P [a]
list path f v = case v of
  Array xs -> traverse (\(i, x) -> f (path ++ "[" ++ show i ++ "]") x) (zip [0 :: Int ..] (V.toList xs))
  _ -> Left (path ++ ": expected an array")

geti :: String -> KM.KeyMap Value -> K.Key -> P Int
geti p o k = int (sub p k) (at o k)

gett :: String -> KM.KeyMap Value -> K.Key -> P Text
gett p o k = text (sub p k) (at o k)

getb :: String -> KM.KeyMap Value -> K.Key -> P Bool
getb p o k = bool (sub p k) (at o k)

-- | An int, or null.
getMaybeInt :: String -> KM.KeyMap Value -> K.Key -> P (Maybe Int)
getMaybeInt p o k = case at o k of
  Null -> Right Nothing
  v -> Just <$> int (sub p k) v

-- | An optional int key: absent, or an int.
optInt :: String -> KM.KeyMap Value -> K.Key -> P (Maybe Int)
optInt p o k = case KM.lookup k o of
  Nothing -> Right Nothing
  Just v -> Just <$> int (sub p k) v

scalar :: String -> Value -> P Scalar
scalar path v = case v of
  Number n | Just i <- toBoundedInteger n -> Right (SInt i)
  String t -> Right (SText t)
  _ -> Left (path ++ ": expected an integer or a string, got " ++ show v)

-- | An object whose values are all scalars (a volatile's entry, statusState).
scalars :: String -> Value -> P (Map Text Scalar)
scalars path v = case v of
  Object o -> M.fromList <$> traverse (\(k, x) -> (,) (K.toText k) <$> scalar (sub path k) x) (KM.toList o)
  _ -> Left (path ++ ": expected an object")

conds :: String -> Value -> P (Map Text Cond)
conds path v = case v of
  Object o -> M.fromList <$> traverse one (KM.toList o)
  _ -> Left (path ++ ": expected an object")
  where
    one (k, x) = do
      let p = sub path k
      e <- object p [] ["duration", "layers"] x
      d <- optInt p e "duration"
      l <- optInt p e "layers"
      Right (K.toText k, Cond d l)

draw :: String -> Value -> P TDraw
draw path v = case v of
  Array xs | [a, b, c] <- V.toList xs -> do
    lo <- int (path ++ "[0]") a
    hi <- int (path ++ "[1]") b
    r <- int (path ++ "[2]") c
    if lo < hi then Right (TDraw lo hi r) else Left (path ++ ": draw has an empty range [" ++ show lo ++ "," ++ show hi ++ ")")
  _ -> Left (path ++ ": draw is not three integers: " ++ show v)

moveSnap :: String -> Value -> P MoveSnap
moveSnap p v = do
  o <- object p ["id", "pp", "maxpp", "disabled"] [] v
  MoveSnap <$> gett p o "id" <*> geti p o "pp" <*> geti p o "maxpp" <*> getb p o "disabled"

monSnap :: String -> Value -> P MonSnap
monSnap p v = do
  o <- object p
    [ "orig", "species", "level", "gender", "hp", "maxhp", "status", "statusState", "fainted", "active", "stats", "boosts"
    , "speed", "types", "teraType", "terastallized", "ability", "item", "moves", "volatiles" ] [] v
  st <- object (sub p "stats") ["atk", "def", "spa", "spd", "spe"] [] (at o "stats")
  bs <- object (sub p "boosts") ["atk", "def", "spa", "spd", "spe", "accuracy", "evasion"] [] (at o "boosts")
  let sp = sub p "stats"
      bp = sub p "boosts"
  stats <- StatsSnap <$> geti sp st "atk" <*> geti sp st "def" <*> geti sp st "spa" <*> geti sp st "spd" <*> geti sp st "spe"
  boosts <- BoostsSnap <$> geti bp bs "atk" <*> geti bp bs "def" <*> geti bp bs "spa" <*> geti bp bs "spd" <*> geti bp bs "spe"
    <*> geti bp bs "accuracy" <*> geti bp bs "evasion"
  vols <- case at o "volatiles" of
    Object vo -> M.fromList <$> traverse (\(k, x) -> (,) (K.toText k) <$> scalars (sub (sub p "volatiles") k) x) (KM.toList vo)
    _ -> Left (sub p "volatiles" ++ ": expected an object")
  MonSnap
    <$> geti p o "orig" <*> gett p o "species" <*> geti p o "level" <*> gett p o "gender" <*> geti p o "hp" <*> geti p o "maxhp"
    <*> gett p o "status" <*> scalars (sub p "statusState") (at o "statusState") <*> getb p o "fainted" <*> getb p o "active"
    <*> pure stats <*> pure boosts <*> geti p o "speed" <*> list (sub p "types") text (at o "types")
    <*> gett p o "teraType" <*> gett p o "terastallized" <*> gett p o "ability" <*> gett p o "item"
    <*> list (sub p "moves") moveSnap (at o "moves") <*> pure vols

sideSnap :: String -> Value -> P SideSnap
sideSnap p v = do
  o <- object p ["id", "pokemonLeft", "sideConditions", "pokemon"] [] v
  SideSnap <$> gett p o "id" <*> geti p o "pokemonLeft" <*> conds (sub p "sideConditions") (at o "sideConditions")
    <*> list (sub p "pokemon") monSnap (at o "pokemon")

snapFromV1 :: String -> Value -> P Snap
snapFromV1 p v = do
  o <- object p ["turn", "request", "ended", "winner", "field", "sides"] [] v
  fo <- object (sub p "field") ["weather", "weatherDuration", "terrain", "terrainDuration", "pseudoWeather"] [] (at o "field")
  let fp = sub p "field"
  field <- FieldSnap <$> gett fp fo "weather" <*> getMaybeInt fp fo "weatherDuration" <*> gett fp fo "terrain"
    <*> getMaybeInt fp fo "terrainDuration" <*> conds (sub fp "pseudoWeather") (at fo "pseudoWeather")
  winner <- case at o "winner" of
    Null -> Right Nothing
    w -> Just <$> text (sub p "winner") w
  sides <- list (sub p "sides") sideSnap (at o "sides")
  case sides of
    [s1, s2] -> Snap <$> geti p o "turn" <*> gett p o "request" <*> getb p o "ended" <*> pure winner <*> pure field <*> pure s1 <*> pure s2
    _ -> Left (sub p "sides" ++ ": expected exactly two sides, got " ++ show (length sides))

stepFromV1 :: String -> Value -> P Step
stepFromV1 p v = do
  o <- object p ["draws", "state", "legal", "chosen"] [] v
  ds <- list (sub p "draws") draw (at o "draws")
  st <- snapFromV1 (sub p "state") (at o "state")
  legal <- list (sub p "legal") (list' text) (at o "legal")
  chosen <- list (sub p "chosen") (\q x -> case x of Null -> Right Nothing; _ -> Just <$> text q x) (at o "chosen")
  case (legal, chosen) of
    ([a, b], [c, d]) -> Right (Step ds st (a, b) (c, d))
    ([_, _], _) -> Left (sub p "chosen" ++ ": expected exactly two entries")
    _ -> Left (sub p "legal" ++ ": expected exactly two entries")
  where
    list' f q x = list q f x

-- | Decode a v1 trace. Total: every deviation from the schema is a @Left@ naming the path.
fromV1 :: Value -> Either String Trace
fromV1 v = do
  o <- object "trace"
    [ "traceVersion", "showdownVersion", "format", "seed", "policySeed", "teams", "steps", "truncated", "finalDraws", "final" ]
    ["tera"] v
  tera <- case KM.lookup "tera" o of
    Nothing -> Right Nothing
    Just x -> Just <$> bool "trace.tera" x
  steps <- list "trace.steps" stepFromV1 (at o "steps")
  fdraws <- list "trace.finalDraws" draw (at o "finalDraws")
  final <- snapFromV1 "trace.final" (at o "final")
  Trace <$> geti "trace" o "traceVersion" <*> gett "trace" o "showdownVersion" <*> gett "trace" o "format" <*> gett "trace" o "seed"
    <*> geti "trace" o "policySeed" <*> pure tera <*> getb "trace" o "truncated" <*> pure (at o "teams")
    <*> pure steps <*> pure fdraws <*> pure final

-- ---------------------------------------------------------------------------
-- Trace -> JSON

num :: Int -> Value
num = Number . fromIntegral

str :: Text -> Value
str = String

obj :: [(K.Key, Value)] -> Value
obj = Object . KM.fromList

-- | A duration or layer count as JSON: the number, or null.
maybeIntToV1 :: Maybe Int -> Value
maybeIntToV1 = maybe Null num

scalarToV1 :: Scalar -> Value
scalarToV1 (SInt i) = num i
scalarToV1 (SText t) = String t

-- | A map of scalars as an object.
entriesToV1 :: Map Text Scalar -> Value
entriesToV1 m = obj [(K.fromText k, scalarToV1 x) | (k, x) <- M.toList m]

-- | @conditionMap@: each condition with the keys it has.
condsToV1 :: Map Text Cond -> Value
condsToV1 m = obj
  [ (K.fromText k, obj ([("duration", num d) | Just d <- [cDuration c]] ++ [("layers", num l) | Just l <- [cLayers c]]))
  | (k, c) <- M.toList m ]

-- | A Pokemon's @volatiles@.
volatilesToV1 :: Map Text (Map Text Scalar) -> Value
volatilesToV1 m = obj [(K.fromText k, entriesToV1 e) | (k, e) <- M.toList m]

monToV1 :: MonSnap -> Value
monToV1 m = obj
  [ ("orig", num (mOrig m)), ("species", str (mSpecies m)), ("level", num (mLevel m)), ("gender", str (mGender m))
  , ("hp", num (mHp m)), ("maxhp", num (mMaxHp m)), ("status", str (mStatus m)), ("statusState", entriesToV1 (mStatusState m))
  , ("fainted", Bool (mFainted m)), ("active", Bool (mActive m))
  , ("stats", obj [("atk", num a), ("def", num d), ("spa", num sa), ("spd", num sd), ("spe", num se)])
  , ("boosts", obj [("atk", num a'), ("def", num d'), ("spa", num sa'), ("spd", num sd'), ("spe", num se'), ("accuracy", num ac), ("evasion", num ev)])
  , ("speed", num (mSpeed m)), ("types", Array (V.fromList (map str (mTypes m))))
  , ("teraType", str (mTeraType m)), ("terastallized", str (mTerastallized m)), ("ability", str (mAbility m)), ("item", str (mItem m))
  , ("moves", Array (V.fromList [obj [("id", str (moId mv)), ("pp", num (moPP mv)), ("maxpp", num (moMaxPP mv)), ("disabled", Bool (moDisabled mv))] | mv <- mMoves m]))
  , ("volatiles", volatilesToV1 (mVolatiles m))
  ]
  where
    StatsSnap a d sa sd se = mStats m
    BoostsSnap a' d' sa' sd' se' ac ev = mBoosts m

sideToV1 :: SideSnap -> Value
sideToV1 s = obj
  [ ("id", str (ssId s)), ("pokemonLeft", num (ssLeft s)), ("sideConditions", condsToV1 (ssConds s))
  , ("pokemon", Array (V.fromList (map monToV1 (ssMons s)))) ]

-- | One snapshot as record.js writes it.
snapToV1 :: Snap -> Value
snapToV1 s = obj
  [ ("turn", num (snTurn s)), ("request", str (snRequest s)), ("ended", Bool (snEnded s)), ("winner", maybe Null str (snWinner s))
  , ( "field"
    , obj
        [ ("weather", str (fsWeather f)), ("weatherDuration", maybeIntToV1 (fsWeatherDuration f))
        , ("terrain", str (fsTerrain f)), ("terrainDuration", maybeIntToV1 (fsTerrainDuration f))
        , ("pseudoWeather", condsToV1 (fsPseudo f)) ] )
  , ("sides", Array (V.fromList [sideToV1 (snP1 s), sideToV1 (snP2 s)]))
  ]
  where
    f = snField s

drawToV1 :: TDraw -> Value
drawToV1 (TDraw lo hi r) = Array (V.fromList [num lo, num hi, num r])

stepToV1 :: Step -> Value
stepToV1 s = obj
  [ ("draws", Array (V.fromList (map drawToV1 (stDraws s)))), ("state", snapToV1 (stState s))
  , ("legal", Array (V.fromList [strs (fst (stLegal s)), strs (snd (stLegal s))]))
  , ("chosen", Array (V.fromList [maybe Null str (fst (stChosen s)), maybe Null str (snd (stChosen s))])) ]
  where
    strs = Array . V.fromList . map str

-- | Rebuild the v1 JSON.
toV1 :: Trace -> Value
toV1 t = obj $
  [ ("traceVersion", num (trVersion t)), ("showdownVersion", str (trShowdownVersion t)), ("format", str (trFormat t))
  , ("seed", str (trSeed t)), ("policySeed", num (trPolicySeed t)) ]
    ++ [("tera", Bool b) | Just b <- [trTera t]]
    ++ [ ("teams", trTeams t), ("steps", Array (V.fromList (map stepToV1 (trSteps t)))), ("truncated", Bool (trTruncated t))
       , ("finalDraws", Array (V.fromList (map drawToV1 (trFinalDraws t)))), ("final", snapToV1 (trFinal t)) ]
