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
  , dexCondLookup
  , moveSpec
  , effectOf
  , abilityId
  , itemId
  , supportedAbilities
  , supportedItems
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

import Engine.Effects
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
  , spWeight :: !Int
  -- ^ @weighthg@: the weight in tenths of a kilogram (@pokemon.weighthg@ before @ModifyWeight@).
  , spParadox :: !Bool
  -- ^ @tags@ includes @"Paradox"@ (data/pokedex.ts), which Booster Energy's @onTakeItem@ reads.
  }
  deriving (Show)

-- | Per-move static data.
data MoveData = MoveData
  { mvId :: !T.Text
  , mvType :: !Int
  , mvPhysical :: !Bool
  , mvStatus :: !Bool
  -- ^ @category === 'Status'@.
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
  , mvEffect :: !Condition
  -- ^ The move as an effect (docs/handlers.md, "A move's own handlers"): its own handlers (not its @condition.*@ ones),
  -- with the metadata of the export and the bodies of "Engine.Effects" @moveBodies@. Empty when the move has none or
  -- when a handler has no body ('cnReject').
  --
  -- A 'MoveData' is also the move in use (Showdown's @ActiveMove@, a copy made when the move is used): the handlers
  -- that Showdown lets change the move return a changed copy, and every later step of the use reads the copy. It is never
  -- stored in the battle state.
  }
  deriving (Show)

-- | A nature's boosted and lowered stat (index into atk, def, spa, spd, spe), or -1.
data NatureData = NatureData
  { natPlus :: !Int
  , natMinus :: !Int
  }
  deriving (Show)

-- | Static data of an effect: a condition the engine runs (conditions.json, data/conditions.ts), an ability or an
-- item (abilities.json, items.json).
data Condition = Condition
  { cnDuration :: !Int
  -- ^ @condition.duration@; 0 when it has none.
  , cnHandlers :: ![(Prefix, EventId, HandlerSpec)]
  -- ^ The handlers the engine runs, with their metadata from the export.
  , cnSupported :: !Bool
  -- ^ The engine implements the effect and every handler the export lists for it has a body.
  , cnReject :: !(Maybe T.Text)
  -- ^ Why not, when 'cnSupported' is false.
  , cnBreakable :: !Bool
  -- ^ @flags.breakable@ (Mold Breaker suppresses the effect's handlers).
  , cnId :: !T.Text
  , cnItemBoosts :: ![(BoostStat, Int)]
  -- ^ An item's @boosts@ (the seeds): the stages @useItem@ raises (pokemon.ts 1857).
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
  , dxAbilities :: !(V.Vector Condition)
  , dxItems :: !(V.Vector Condition)
  , dxFieldConds :: !(V.Vector Condition)
  -- ^ Field conditions: the weathers of @conditions.json@, then the @condition@ blocks of the moves that carry
  -- @sideCondition@, @terrain@ or @pseudoWeather@. The state holds indices into this table (side conditions, weather,
  -- terrain and pseudo-weather share one id space; the 'EffectKind' says which holder an id is in).
  , dxFieldIx :: !(M.Map T.Text Int)
  , dxAbilityIx :: !(M.Map T.Text Int)
  , dxItemIx :: !(M.Map T.Text Int)
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

-- | The static data of an effect (a status or volatile, an ability or an item), if its id is valid.
effectOf :: Dex -> EffectKey -> Maybe Condition
effectOf dx (EffectKey kind i) = case kind of
  EffStatus -> dxConditions dx V.!? i
  EffVolatile -> dxConditions dx V.!? i
  EffAbility -> dxAbilities dx V.!? i
  EffItem -> dxItems dx V.!? i
  EffSideCond -> dxFieldConds dx V.!? i
  EffWeather -> dxFieldConds dx V.!? i
  EffTerrain -> dxFieldConds dx V.!? i
  EffPseudoWeather -> dxFieldConds dx V.!? i
  EffMove -> Nothing -- a move's effect is 'mvEffect' of the move in use
{-# INLINE effectOf #-}

-- | A move's own handler for a prefix and event, if the engine has one (docs/handlers.md, "A move's own handlers").
moveSpec :: MoveData -> Prefix -> EventId -> Maybe HandlerSpec
moveSpec mv = condSpec (mvEffect mv)
{-# INLINE moveSpec #-}

-- | The id (@toID@ name) of an ability.
abilityId :: Dex -> Int -> T.Text
abilityId dx i = maybe "" cnId (dxAbilities dx V.!? i)

-- | The id of an item, @""@ for none.
itemId :: Dex -> Int -> T.Text
itemId dx i = maybe "" cnId (dxItems dx V.!? i)

-- | Ids of the abilities 'Engine.Team.parseTeam' accepts.
supportedAbilities :: Dex -> [T.Text]
supportedAbilities dx = [cnId a | a <- V.toList (dxAbilities dx), cnSupported a]

-- | Ids of the items 'Engine.Team.parseTeam' accepts, including @""@ (no item).
supportedItems :: Dex -> [T.Text]
supportedItems dx = "" : [cnId a | a <- V.toList (dxItems dx), cnSupported a]

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
  abilitiesV <- load "abilities.json"
  itemsV <- load "items.json"
  either (ioError . userError) pure $ do
    tnames <- typeNamesOf typesV
    let tix = M.fromList (zip (V.toList tnames) [0 ..])
    chart <- chartOf tnames typesV
    immune <- immunityOf tnames typesV
    conds <- traverse (conditionFor conditionsV abilitiesV movesV) [minBound .. maxBound]
    let tyOf n = M.lookup n tix
    abilityEntries <- objectEntries abilitiesV
    -- the field-condition table first: its ids (the weathers, then the terrains and the rest in the order of moves.json)
    -- are named by the rules of abilities and items (swiftswim's weather, the seeds' terrain)
    moveEntries <- objectEntries movesV
    fieldL <- fieldConditions conditionsV moveEntries
    let fieldV = V.fromList (map snd fieldL)
        fieldIx = M.fromList (zip (map fst fieldL) [0 ..])
        condLookup = fieldLookup fieldIx fieldV
        fieldIdOf n = M.lookup n fieldIx
    abilityL <- traverse (\(k, v) -> effectFor k 7 POwn True v (abilityBodies tyOf fieldIdOf k)) abilityEntries
    itemEntries <- objectEntries itemsV
    itemL <- traverse (\(k, v) -> effectFor k 8 POwn True v (itemBodies tyOf fieldIdOf k)) itemEntries
    speciesL <- objectEntries speciesV >>= traverse (speciesOf tix . snd)
    movesL <- traverse (moveOf tix condLookup (V.fromList conds) . snd) moveEntries
    struggleV <- maybe (Left "moves.json has no struggle") Right (lookup "struggle" moveEntries)
    -- Struggle's onModifyMove makes 'moveRule' reject it, so 'moveOf' gives it 'plainRule'. Its flags still apply:
    -- data/moves.ts struggle `flags: {contact: 1, ...}`, read by checkMoveMakesContact (Rocky Helmet). Its other
    -- flags are inert ('inertFlags').
    let struggleContact = case struggleV of
          Object o | Just (Object fl) <- KM.lookup "flags" o -> KM.member "contact" fl
          _ -> False
    struggle <- (\m -> m {mvType = noType, mvSupported = False, mvRule = (mvRule m) {mrContact = struggleContact}})
      <$> moveOf tix condLookup (V.fromList conds) struggleV
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
      , dxAbilities = V.fromList abilityL
      , dxItems = V.fromList itemL
      , dxFieldConds = fieldV
      , dxFieldIx = fieldIx
      , dxAbilityIx = M.fromList (zip (map fst abilityEntries) [0 ..])
      , dxItemIx = M.fromList (zip (map fst itemEntries) [0 ..])
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

-- | A condition with the bodies of 'nativeBody': from @conditions.json@, or for the volatiles of the abilities
-- protosynthesis, quarkdrive and unburden the @condition@ block of the ability, and for roost's volatile the @condition@
-- block of the move (@dex.conditions.get@ finds it there: dex-conditions.ts 692-699), whose handler names are the owner's
-- @condition.on...@ entries. See 'effectFor'.
conditionFor :: Value -> Value -> Value -> CondId -> Either String Condition
conditionFor conditionsV abilitiesV movesV c = do
  (v, typeOrder) <- case (abilityOf c, moveOf' c) of
    (Just ab, _) -> owned abilitiesV ab
    (_, Just mv) -> owned movesV mv
    _ -> do
      v <- field (condName c) conditionsV
      effectType <- field "effectType" v >>= asText
      pure (v, case effectType of
        "Condition" -> 2
        "Weather" -> 5
        "Format" -> 5
        "Rule" -> 5
        "Ruleset" -> 5
        "Ability" -> 7
        "Item" -> 8
        _ -> 0 :: Int)
  let bodies = [(p, e, b) | p <- [minBound .. maxBound], e <- [minBound .. maxBound], Just b <- [nativeBody c p e]]
  effectFor (condName c) typeOrder POwn False v (Just bodies)
  where
    -- the @condition@ block of an ability or a move, with the owner's @condition.on...@ names as its handlers
    owned table key = do
      a <- field key table
      cond <- field "condition" a
      let hs = case a of
            Object o | Just (Array xs) <- KM.lookup "handlers" o -> [String (T.drop 10 h) | String h <- V.toList xs, "condition." `T.isPrefixOf` h]
            _ -> []
      pure (case cond of Object o -> Object (KM.insert "handlers" (Array (V.fromList hs)) o); _ -> cond, 2) -- effectType Condition
    abilityOf x = case x of
      CProtosynthesis -> Just "protosynthesis"
      CQuarkDrive -> Just "quarkdrive"
      CUnburden -> Just "unburden"
      _ -> Nothing
    moveOf' x = case x of
      CRoost -> Just "roost"
      _ -> Nothing

-- | A move's field condition by key, if the engine runs it ('Engine.Rule.CondLookup').
fieldLookup :: M.Map T.Text Int -> V.Vector Condition -> CondLookup
fieldLookup ix table c = case M.lookup c ix of
  Nothing -> Left ("no condition " <> c)
  Just i -> let cn = table V.! i in
    if cnSupported cn then Right i else Left ("condition " <> c <> " is not supported: " <> fromMaybe "" (cnReject cn))

-- | The lookup 'loadDex' gives the move translation, for a loaded dex.
dexCondLookup :: Dex -> CondLookup
dexCondLookup dx = fieldLookup (dxFieldIx dx) (dxFieldConds dx)

-- | The field conditions, from two sources: the weathers of @conditions.json@ (@effectType: 'Weather'@), and the
-- @condition@ block of each move whose @sideCondition@, @terrain@ or @pseudoWeather@ is the move's own id (the way
-- @dex.conditions.get@ finds a condition that data/conditions.ts does not define). The handler names of the
-- block are the move's @condition.on...@ entries. The default subOrder is per holder kind (battle.ts
-- @resolvePriority@ 973-1011): a side condition 4 (@Condition@ with a side target), a weather 5, a terrain 0
-- (@Terrain@ is not in @effectTypeOrder@) and a pseudo-weather 2 (@Condition@ with no side or field target). A
-- field condition's own callbacks are @onSide<Event>@ ('PSide') or @onField<Event>@ ('PField').
fieldConditions :: Value -> [(T.Text, Value)] -> Either String [(T.Text, Condition)]
fieldConditions conditionsV moveEntries = do
  condEntries <- objectEntries conditionsV
  -- LATENT (pseudo-weather entries below): Showdown's runEvent sets `effectState.target = effectHolder` on each callback
  -- it runs (battle.ts 908-909), so after a pseudo-weather's first runEvent callback its subOrder fallback is 5, not 2
  -- (resolvePriority 995-998), and fieldEvent's in-place check (548-561) starts to apply to that field state.
  -- The in-place half is implemented ('Engine.State.esHasTarget') but UNVERIFIED: no trace can observe it (see the LATENT
  -- in 'Engine.Dispatch.fieldEvent'). The subOrder half is not modelled, and is unreachable while no
  -- pseudo-weather has a runEvent handler without an explicit subOrder (Trick Room has no runEvent handler). A weather's
  -- subOrder is 5 and a terrain's 0 whatever their target.
  weathers <- traverse (\(k, v) -> (,) k <$> effectFor k 5 PField False v (fieldCondBodies k))
    [(k, v) | (k, v) <- condEntries, field "effectType" v == Right (String "Weather")]
  fromMoves <- traverse (\(k, order, own, v) -> (,) k <$> effectFor k order own False v (fieldCondBodies k))
    [ (k, order, own, Object (KM.insert "handlers" (Array (V.fromList [String (T.drop 10 h) | String h <- hs, "condition." `T.isPrefixOf` h])) c))
    | (k, Object m) <- moveEntries
    , Just key <- [conditionField k m] -- the move defines the condition (the same test as the translation's)
    , (key', order, own) <- [("sideCondition", 4 :: Int, PSide), ("terrain", 0, PField), ("pseudoWeather", 2, PField)]
    , key' == key
    , Just (Object c) <- [KM.lookup "condition" m]
    , let hs = case KM.lookup "handlers" m of Just (Array xs) -> V.toList xs; _ -> []
    ]
  pure (weathers ++ fromMoves)

-- | An effect from its export entry and its hand-written bodies ('Nothing': the engine does not implement it).
-- Its handlers are the exported handler names that the engine knows ('parseHandlerName') and has a body for;
-- order, priority and subOrder come from the entry's @on<Prefix><Event>Order|Priority|SubOrder@ fields, and
-- subOrder otherwise from the effect type (battle.ts @resolvePriority@ 973-1011: @Condition@ 2, the statuses
-- (@effectType@ @Status@) 0, abilities 7, items 8; the exceptions Poison Touch, Perish Body and Stall are not
-- supported). An effect with a duration also gets a @Residual@ entry without a body when it has no
-- @onResidual@ (for a side or field condition, @onSideResidual@ / @onFieldResidual@, the prefix @ownPfx@): Showdown
-- collects it (@getKey = 'duration'@) to count the duration down.
-- With @startAsSwitchIn@ (abilities and items), an @onStart@ with no @onSwitchIn@ and no @onAnySwitchIn@ is also the
-- @(POwn, EvSwitchIn)@ handler, with its order, priority and subOrder read from the @onSwitchIn@ keys: Showdown runs
-- the Start of an ability or item as its SwitchIn (battle.ts @getCallback@ 1025-1036; the gen 3 clause and the
-- status-with-@ability:@-id case are not reachable here).
effectFor :: T.Text -> Int -> Prefix -> Bool -> Value -> Maybe [(Prefix, EventId, Body)] -> Either String Condition
effectFor ident typeOrder ownPfx startAsSwitchIn v mbodies = do
  names0 <- case v of
    Object o | Just hs <- KM.lookup "handlers" o -> asArray hs >>= traverse asText
    _ -> Right []
  -- the @condition.on...@ handlers of these abilities belong to their volatile ('conditionFor'), not to the ability
  let names = if ident `elem` ["protosynthesis", "quarkdrive", "unburden"] && startAsSwitchIn
        then filter (not . ("condition." `T.isPrefixOf`)) names0
        else names0
  boosts <- case v of
    Object o | startAsSwitchIn, Just (Object bo) <- KM.lookup "boosts" o ->
      traverse (\(k, x) -> (,) <$> boostStatOf (K.toText k) <*> asInt x) (KM.toList bo)
    _ -> Right []
  duration <- case v of
    Object o | Just d <- KM.lookup "duration" o -> asInt d
    _ -> Right 0
  let parsed = [(n, parseHandlerName n) | n <- names]
      bodyOf p e = mbodies >>= \bs -> case [b | (p', e', b) <- bs, p' == p, e' == e] of
        (b : _) -> Just b
        [] -> Nothing
      missing = [n | (n, mpe) <- parsed, maybe True (\(p, e) -> bodyOf p e == Nothing) mpe]
      reject = case (mbodies, missing) of
        (Nothing, _) -> Just "the engine does not implement it"
        (_, n : _) -> Just ("handler " <> n <> " has no body")
        _ -> Nothing
      spec (p, e) body = do
        let key suffix = "on" <> prefixName p <> eventName e <> suffix
            num suffix = case v of
              Object o | Just (Number x) <- KM.lookup (K.fromText (key suffix)) o -> Just x
              _ -> Nothing
        order <- maybe (Right 0) (maybe (Left (T.unpack (key "Order") ++ " is not an integer")) Right . toBoundedInteger) (num "Order")
        prio <- maybe (Right 0) (maybe (Left (T.unpack (key "Priority") ++ " times 10 is not an integer")) Right . toBoundedInteger . (* 10)) (num "Priority")
        sub <- maybe (Right 0) (maybe (Left (T.unpack (key "SubOrder") ++ " is not an integer")) Right . toBoundedInteger) (num "SubOrder")
        pure (p, e, HandlerSpec order prio (if sub /= 0 then sub else typeOrder) body)
  specs <- if reject == Nothing
    then traverse (\(pe, b) -> spec pe (Just b)) [((p, e), b) | (_, Just (p, e)) <- parsed, Just b <- [bodyOf p e]]
    else Right []
  startSwitchIn <- case bodyOf POwn EvStart of
    Just b | startAsSwitchIn, reject == Nothing, "onStart" `elem` names
           , "onSwitchIn" `notElem` names, "onAnySwitchIn" `notElem` names -> (: []) <$> spec (POwn, EvSwitchIn) (Just b)
    _ -> Right []
  residual <- if reject == Nothing && duration /= 0 && null [() | (p, EvResidual, _) <- specs, p == ownPfx]
    then (: []) <$> spec (ownPfx, EvResidual) Nothing
    else Right []
  let breakable = case v of
        Object o | Just (Object fl) <- KM.lookup "flags" o -> maybe False truthy (KM.lookup "breakable" fl)
        _ -> False
  pure (Condition duration (specs ++ startSwitchIn ++ residual) (reject == Nothing) reject breakable ident boosts)

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
  weight <- field "weighthg" v >>= asInt
  paradox <- case v of
    Object o | Just (Array tagVals) <- KM.lookup "tags" o -> elem "Paradox" <$> traverse asText (V.toList tagVals)
    _ -> Right False
  -- gen-slice0.js: num > 0, not nonstandard, a base forme, not battle-only; plus Shedinja for its fixed HP
  let supported =
        ident == "shedinja"
          || (num > 0 && not (truthyField "isNonstandard" v) && not (truthyField "forme" v) && not (truthyField "battleOnly" v))
  pure (SpeciesData ident t1 t2 (U.fromList base) gender maxHp supported weight paradox)

typeIx :: M.Map T.Text Int -> T.Text -> Either String Int
typeIx tix t = maybe (Left ("unknown type " ++ T.unpack t)) Right (M.lookup t tix)

moveOf :: M.Map T.Text Int -> CondLookup -> V.Vector Condition -> Value -> Either String MoveData
moveOf tix condLookup conds v = do
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
  -- the move's own handlers: the export's, except the @condition.*@ ones, which belong to the field condition the move
  -- defines ('fieldConditions'). A move with none needs no bodies.
  let ownNames = case v of
        Object o | Just (Array hs) <- KM.lookup "handlers" o -> [h | String h <- V.toList hs, not ("condition." `T.isPrefixOf` h)]
        _ -> []
      ownV = case v of
        Object o -> Object (KM.insert "handlers" (Array (V.fromList (map String ownNames))) o)
        _ -> v
  effect <- effectFor ident 0 POwn False ownV (if null ownNames then Just [] else moveBodies ident)
  let (rule, rejected0) = case moveRule condLookup (cnReject effect) v of
        Left why -> (plainRule, Just why)
        Right r -> (r, Nothing)
      -- a move that adds the roost volatile is supported when the volatile is (every handler the export lists for it has a body)
      roost = conds V.! fromEnum CRoost
      rejected = case rejected0 of
        Nothing | ERoost `elem` mrSelf rule, not (cnSupported roost) -> Just ("condition roost is not supported: " <> fromMaybe "" (cnReject roost))
        _ -> rejected0
      supported = isNothing rejected && executable rule
  pure (MoveData ident ty (cat == "Physical") (cat == "Status") bp acc prio crit pp noBoost (truthyField "struggleRecoil" v) rule rejected supported effect)

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

-- | A stat key of @boosts@.
boostStatOf :: T.Text -> Either String BoostStat
boostStatOf k = case k of
  "atk" -> Right BAtk
  "def" -> Right BDef
  "spa" -> Right BSpa
  "spd" -> Right BSpd
  "spe" -> Right BSpe
  "accuracy" -> Right BAccuracy
  "evasion" -> Right BEvasion
  _ -> Left ("boost stat " ++ T.unpack k)
