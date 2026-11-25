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
  , Condition (..)
  , conditionOf
  , condSpec
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
import Data.Maybe (fromMaybe, isNothing)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.FilePath ((</>))

import Engine.Event
import Engine.Rule

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

-- | Per-move static data.
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
  , mvRule :: !MoveRule
  -- ^ What the move does ('plainRule' when the move did not translate).
  , mvReject :: !(Maybe Reason)
  -- ^ Why the move does not translate ('Engine.Rule.moveRule'), if it does not.
  , mvSupported :: !Bool
  -- ^ The move translates and its rule is 'executable'.
  }
  deriving (Show)

-- | A nature's boosted and lowered stat (index into atk, def, spa, spd, spe), or -1.
data NatureData = NatureData
  { natPlus :: !Int
  , natMinus :: !Int
  }
  deriving (Show)

-- | Static data of a condition the engine supports (conditions.json, data/conditions.ts).
data Condition = Condition
  { cnDuration :: !Int
  -- ^ @condition.duration@; 0 when it has none.
  , cnHandlers :: ![(Prefix, EventId, HandlerSpec)]
  -- ^ The handlers the engine runs, with their metadata from the export.
  , cnSupported :: !Bool
  -- ^ Every handler the export lists for the condition has a body ('nativeBody').
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
  , dxTypeImmune :: !(V.Vector [T.Text])
  -- ^ By type id: the non-type keys of typechart @damageTaken@ that are 3 (immune): @brn@, @par@, @psn@, @tox@,
  -- @frz@, @powder@, @prankster@, @sandstorm@, @hail@. @runStatusImmunity@ reads them.
  , dxConditions :: !(V.Vector Condition)
  -- ^ Indexed by 'CondId'.
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

-- | A supported condition by id.
conditionOf :: Dex -> CondId -> Condition
conditionOf dx c = dxConditions dx V.! fromEnum c
{-# INLINE conditionOf #-}

-- | The handler of a condition for a prefix and event (@on<Prefix><Event>@), if the engine has one.
condSpec :: Condition -> Prefix -> EventId -> Maybe HandlerSpec
condSpec cn p e = go (cnHandlers cn)
  where
    go [] = Nothing
    go ((p', e', h) : rest)
      | p' == p && e' == e = Just h
      | otherwise = go rest
{-# INLINE condSpec #-}

-- | Showdown's effectiveness code of an attacking type against a defending type.
typeEffect :: Dex -> Int -> Int -> Int
typeEffect dx def atk = dxTypeChart dx U.! (def * typeCount dx + atk)
{-# INLINE typeEffect #-}

-- | Load the dex from a directory holding @species.json@, @moves.json@, @types.json@, @natures.json@ and
-- @conditions.json@.
loadDex :: FilePath -> IO Dex
loadDex dir = do
  speciesV <- load "species.json"
  movesV <- load "moves.json"
  typesV <- load "types.json"
  naturesV <- load "natures.json"
  conditionsV <- load "conditions.json"
  either (ioError . userError) pure $ do
    tnames <- typeNamesOf typesV
    let tix = M.fromList (zip (V.toList tnames) [0 ..])
    chart <- chartOf tnames typesV
    immune <- immunityOf tnames typesV
    conds <- traverse (conditionFor conditionsV) [minBound .. maxBound]
    speciesL <- objectEntries speciesV >>= traverse (speciesOf tix . snd)
    moveEntries <- objectEntries movesV
    movesL <- traverse (moveOf tix . snd) moveEntries
    struggleV <- maybe (Left "moves.json has no struggle") Right (lookup "struggle" moveEntries)
    struggle <- (\m -> m {mvType = noType, mvSupported = False}) <$> moveOf tix struggleV
    natureEntries <- objectEntries naturesV
    natureL <- traverse (natureOf . snd) natureEntries
    pure Dex
      { dxSpecies = V.fromList speciesL
      , dxMoves = V.fromList movesL
      , dxStruggle = struggle
      , dxNatures = V.fromList natureL
      , dxTypeNames = tnames
      , dxTypeChart = chart
      , dxTypeImmune = immune
      , dxConditions = V.fromList conds
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

-- | The immune (3) non-type keys of @otherDamageTaken@, by type id.
immunityOf :: V.Vector T.Text -> Value -> Either String (V.Vector [T.Text])
immunityOf names v = do
  other <- field "otherDamageTaken" v
  traverse (\n -> case other of
    Object o | Just (Object ks) <- KM.lookup (K.fromText n) o ->
      Right [K.toText k | (k, Number x) <- KM.toList ks, x == 3]
    _ -> Right []) names

-- | A condition from @conditions.json@. Its handlers are the exported handler names that the engine knows
-- ('parseHandlerName') and has a body for ('nativeBody'); order, priority and subOrder come from the entry's
-- @on<Prefix><Event>Order|Priority|SubOrder@ fields, and subOrder otherwise from the @effectType@
-- (battle.ts @resolvePriority@ 973-1011: @Condition@ 2; the statuses, @effectType@ @Status@, 0). A condition with
-- a duration also gets a @Residual@ entry without a body when it has no @onResidual@: Showdown collects it
-- (@getKey = 'duration'@) to count the duration down.
conditionFor :: Value -> CondId -> Either String Condition
conditionFor conditionsV c = do
  v <- field (condName c) conditionsV
  effectType <- field "effectType" v >>= asText
  names <- case v of
    Object o | Just hs <- KM.lookup "handlers" o -> asArray hs >>= traverse asText
    _ -> Right []
  duration <- case v of
    Object o | Just d <- KM.lookup "duration" o -> asInt d
    _ -> Right 0
  let typeOrder = case effectType of
        "Condition" -> 2
        "Weather" -> 5
        "Format" -> 5
        "Rule" -> 5
        "Ruleset" -> 5
        "Ability" -> 7
        "Item" -> 8
        _ -> 0 :: Int
      parsed = [(n, parseHandlerName n) | n <- names]
      bodies = [(n, pe, nativeBody c p e) | (n, Just pe@(p, e)) <- parsed]
      supported = all (\(_, mpe) -> maybe False (\(p, e) -> nativeBody c p e /= Nothing) mpe) parsed
      spec (p, e) body = do
        let key suffix = "on" <> prefixName p <> eventName e <> suffix
            num suffix = case v of
              Object o | Just (Number x) <- KM.lookup (K.fromText (key suffix)) o -> Just x
              _ -> Nothing
        order <- maybe (Right 0) (maybe (Left (T.unpack (key "Order") ++ " is not an integer")) Right . toBoundedInteger) (num "Order")
        prio <- maybe (Right 0) (maybe (Left (T.unpack (key "Priority") ++ " times 10 is not an integer")) Right . toBoundedInteger . (* 10)) (num "Priority")
        sub <- maybe (Right 0) (maybe (Left (T.unpack (key "SubOrder") ++ " is not an integer")) Right . toBoundedInteger) (num "SubOrder")
        pure (p, e, HandlerSpec order prio (if sub /= 0 then sub else typeOrder) body)
  specs <- traverse (\(pe, b) -> spec pe (Just b)) [(pe, b) | (_, pe, Just b) <- bodies]
  residual <- if duration /= 0 && null [() | (POwn, EvResidual, _) <- specs]
    then (: []) <$> spec (POwn, EvResidual) Nothing
    else Right []
  pure (Condition duration (specs ++ residual) supported)

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

moveOf :: M.Map T.Text Int -> Value -> Either String MoveData
moveOf tix v = do
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
  let (rule, rejected) = case moveRule v of
        Left why -> (plainRule, Just why)
        Right r -> (r, Nothing)
      supported = isNothing rejected && executable rule
  pure (MoveData ident ty (cat == "Physical") bp acc prio crit pp noBoost (truthyField "struggleRecoil" v) rule rejected supported)

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

-- | JavaScript truthiness of an exported value.
truthy :: Value -> Bool
truthy Null = False
truthy (Bool b) = b
truthy (Number n) = n /= 0
truthy (String t) = not (T.null t)
truthy _ = True

truthyField :: T.Text -> Value -> Bool
truthyField k (Object o) = maybe False truthy (KM.lookup (K.fromText k) o)
truthyField _ _ = False
