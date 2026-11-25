{-# LANGUAGE OverloadedStrings #-}
-- | Rules as data (architecture decision 6): a closed, function-free description of what a move does,
-- organised by the slot of Showdown's move pipeline in which each part runs, and the total, conservative
-- translation from the exported move JSON (@data/gen9/moves.json@) into it.
--
-- The vocabulary covers the whole of slice 1 (HP effects, stat stages, status conditions); the part the
-- engine can run so far is selected by 'executable'. A move is supported when it translates and is
-- executable ('Engine.Dex.mvSupported'), so a rule kind that no phase interprets can never reach a battle.
--
-- Translation never guesses. Every truthy key of a move must be translated into the rule, listed as inert
-- (with the reason it cannot matter in this engine), or the move is rejected with a 'Reason'. Handler code
-- (@handlers@) is always a rejection.
module Engine.Rule
  ( MoveRule (..)
  , Hits (..)
  , RuleTarget (..)
  , Effect (..)
  , Secondary (..)
  , BoostStat (..)
  , StatusId (..)
  , Reason
  , plainRule
  , moveRule
  , executable
  ) where

import Control.Monad (unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Why a move is not translated (or not executable).
type Reason = T.Text

-- | How many times the hit loop runs (@move.multihit@, battle-actions.ts @hitStepMoveHitLoop@).
data Hits
  = OneHit
  | FixedHits !Int
  -- ^ @multihit: n@: no draw.
  | TwoToFive
  -- ^ @multihit: [2, 5]@: @battle.sample@ over the 35-35-15-15 table (one @random(20)@ draw).
  deriving (Eq, Show)

-- | Who the move acts on in singles. Spread targets (@allAdjacentFoes@, @allAdjacent@) and @randomNormal@
-- reach the one foe, so they reduce to 'TargetFoe' (pokemon.ts @getMoveTargets@, battle.ts @getRandomTarget@).
data RuleTarget = TargetFoe | TargetSelf
  deriving (Eq, Show)

-- | A stat that stages apply to (@BoostID@).
data BoostStat = BAtk | BDef | BSpa | BSpd | BSpe | BAccuracy | BEvasion
  deriving (Eq, Show)

-- | The major statuses of slice 1 (@Pokemon.status@). Sleep is not in the slice.
data StatusId = Brn | Par | Psn | Tox | Frz
  deriving (Eq, Show)

-- | One effect of a move on a Pokemon.
data Effect
  = EHeal !Int !Int
  -- ^ @move.heal@: @round(baseMaxhp * n / d)@ (battle-actions.ts @runMoveEffects@). Phase 1.
  | EBoost ![(BoostStat, Int)]
  -- ^ @boosts@ (battle.ts @boost@). Phase 2.
  | EStatus !StatusId
  -- ^ @status@ (pokemon.ts @trySetStatus@). Phase 3.
  | EFlinch
  -- ^ @volatileStatus: 'flinch'@ (only ever a secondary effect). Phase 3.
  deriving (Eq, Show)

-- | One entry of @move.secondaries@ (battle-actions.ts @secondaries@): Showdown draws @random(100)@ for
-- every secondary of every target and applies it when the roll is below the chance.
data Secondary = Secondary
  { secChance :: !Int
  , secTarget :: ![Effect]
  -- ^ Applied to the move's target (@moveHit(target, ..., secondary)@).
  , secSelf :: ![Effect]
  -- ^ @secondary.self@: applied to the user.
  }
  deriving (Eq, Show)

-- | What a move does, by pipeline slot.
data MoveRule = MoveRule
  { mrDamaging :: !Bool
  -- ^ @basePower > 0@: @getDamage@ runs. A status move (@basePower 0@) deals no damage and has no hit
  -- effects beyond 'mrOnHit'.
  , mrHits :: !Hits
  , mrTarget :: !RuleTarget
  , mrOnHit :: ![Effect]
  -- ^ @runMoveEffects@ on the move's target: @move.heal@, @move.boosts@, @move.status@.
  , mrSelf :: ![Effect]
  -- ^ @selfDrops@: @move.self@. Showdown draws @random(100)@ for it whenever @self.boosts@ is set.
  , mrSecondaries :: ![Secondary]
  , mrDrain :: !(Maybe (Int, Int))
  -- ^ @move.drain@: battle.ts @spreadDamage@ heals the user @round(damage * n / d)@ on each damaging hit.
  , mrRecoil :: !(Maybe (Int, Int))
  -- ^ @move.recoil@: battle-actions.ts @applyRecoilDamage@ after the hit loop, @round(totalDamage * n / d)@, at least 1.
  , mrThawsTarget :: !Bool
  -- ^ @move.thawsTarget@. Phase 3.
  , mrDefrost :: !Bool
  -- ^ @flags.defrost@: the user thaws when it uses the move (conditions.ts @frz@ @onBeforeMove@). Phase 3.
  , mrIgnoreImmunity :: !Bool
  -- ^ @move.ignoreImmunity === true@: the type-immunity step is skipped (pokemon.ts @runImmunity@).
  }
  deriving (Eq, Show)

-- | A plain single-target damaging move: the rule of Tackle, and of Struggle.
plainRule :: MoveRule
plainRule = MoveRule
  { mrDamaging = True
  , mrHits = OneHit
  , mrTarget = TargetFoe
  , mrOnHit = []
  , mrSelf = []
  , mrSecondaries = []
  , mrDrain = Nothing
  , mrRecoil = Nothing
  , mrThawsTarget = False
  , mrDefrost = False
  , mrIgnoreImmunity = False
  }

-- | The part of the vocabulary the engine can run in the current phase (phases 1 and 2: HP effects and stat
-- stages).
--
-- A damaging move: single or multi-hit, with drain or recoil, @self@ boosts and boost secondaries. A status
-- move: exactly one hit effect, a self-targeted heal or a stat change on its target. Status conditions,
-- flinch, thawing and defrost are not executable yet.
executable :: MoveRule -> Bool
executable r =
  not (mrThawsTarget r) && not (mrDefrost r)
    && all boostOnly (mrSelf r) && all (\s -> all boostOnly (secTarget s ++ secSelf s)) (mrSecondaries r)
    && if mrDamaging r
      then null (mrOnHit r) && not (mrIgnoreImmunity r) && mrTarget r == TargetFoe
      else mrHits r == OneHit && mrDrain r == Nothing && mrRecoil r == Nothing
        && null (mrSelf r) && null (mrSecondaries r)
        && (case (mrOnHit r, mrTarget r) of
              ([EHeal _ _], TargetSelf) -> True
              ([EBoost _], _) -> True
              _ -> False)
  where
    boostOnly (EBoost _) = True
    boostOnly _ = False

-- ---------------------------------------------------------------------------
-- Translation

-- | Keys read by "Engine.Dex" into the static move data (type, power, accuracy, PP, ...).
dataKeys :: [T.Text]
dataKeys =
  [ "id", "name", "num", "gen", "type", "category", "basePower", "accuracy", "critRatio", "pp"
  , "priority", "noPPBoosts"
  ]

-- | Keys translated into the rule below.
ruleKeys :: [T.Text]
ruleKeys =
  [ "target", "flags", "multihit", "drain", "recoil", "heal", "boosts", "self", "secondary"
  , "secondaries", "status", "thawsTarget", "ignoreImmunity"
  ]

-- | Keys that are truthy on moves the engine accepts but cannot change the outcome, and why.
inertKeys :: [(T.Text, T.Text)]
inertKeys =
  [ ("baseMoveType", "copy of type; only read by ModifyType handlers (Hidden Power, Weather Ball), which are rejected")
  ]

-- | Flags that cannot matter while the engine has none of the thing that reads them.
-- REVISIT each entry when the mechanic in its reason is added.
inertFlags :: [(T.Text, T.Text)]
inertFlags =
  [ ("contact", "read by contact abilities and items only")
  , ("protect", "blocked by Protect-like moves, which have handlers and are rejected")
  , ("mirror", "read by Mirror Move, which is rejected")
  , ("metronome", "read by Metronome, which is rejected")
  , ("slicing", "read by Sharpness only")
  , ("punch", "read by Iron Fist and Punching Glove only")
  , ("bite", "read by Strong Jaw only")
  , ("bullet", "read by Bulletproof only")
  , ("pulse", "read by Mega Launcher only")
  , ("wind", "read by Wind Rider and Wind Power only")
  , ("sound", "read by Soundproof, and sound moves ignoring Substitute; neither exists")
  , ("distance", "only matters for the target choice in triples")
  , ("allyanim", "animation hint")
  , ("noassist", "read by Assist, which is rejected")
  , ("failcopycat", "read by Copycat, which is rejected")
  , ("failmefirst", "read by Me First, which is rejected")
  , ("nosleeptalk", "read by Sleep Talk, which is rejected")
  , ("failinstruct", "read by Instruct, which is rejected")
  , ("failencore", "read by Encore, which is rejected")
  , ("failmimic", "read by Mimic, which is rejected")
  , ("nosketch", "read by Sketch, which is rejected")
  , ("noparentalbond", "read by Parental Bond only")
  , ("snatch", "read by Snatch, which is rejected")
  , ("dance", "read by Dancer only")
  , ("nonsky", "only blocks hitting a Pokemon held by Sky Drop, which is rejected")
  , ("heal", "only blocked by Heal Block, which is rejected")
  , ("reflectable", "read by Magic Bounce and Magic Coat only")
  , ("bypasssub", "only matters with a Substitute, which is rejected")
  , ("minimize", "only matters against a Pokemon that used Minimize, which is rejected")
  , ("gravity", "only matters under Gravity, which is rejected")
  ]

-- | Why a move with handlers, or an unknown key, is rejected is reported by the translation; this
-- translates one move. The result is a rule only when every truthy key is accounted for.
moveRule :: Value -> Either Reason MoveRule
moveRule (Object o) = do
  case KM.lookup "handlers" o of
    Just hs | truthy hs -> Left ("has handler code " <> showHandlers hs)
    _ -> Right ()
  mapM_ checkKey [(K.toText k, v) | (k, v) <- KM.toList o, truthy v]
  flags <- case KM.lookup "flags" o of
    Just (Object fl) -> Right [K.toText k | (k, v) <- KM.toList fl, truthy v]
    _ -> Right []
  mapM_ checkFlag flags
  -- LATENT: @multiaccuracy@ (Triple Axel, Population Bomb) is rejected as "not modelled" by 'checkKey'. If it
  -- is ever accepted: per-hit accuracy in @hitStepMoveHitLoop@ (battle-actions.ts ~977-994) uses a
  -- floating-point table [1, 4/3, 5/3, 2, 7/3, 8/3, 3], which disagrees with exact integer division
  -- (accuracy 35 at stage -4 is 14 in JavaScript, 15 exact; 70 at -4 is 29, 30 exact). Unlike
  -- @hitStepAccuracy@ (~790), the result must reproduce those float semantics.
  hits <- case look "multihit" of
    Nothing -> Right OneHit
    Just (Number n) | Just k <- toBoundedInteger n, k >= 2 -> Right (FixedHits k)
    Just (Array xs) | [Number a, Number b] <- V.toList xs, (toBoundedInteger a, toBoundedInteger b) == (Just (2 :: Int), Just (5 :: Int)) -> Right TwoToFive
    Just v -> Left ("multihit " <> showV v)
  target <- case look "target" of
    Just (String t)
      | t `elem` ["normal", "any", "randomNormal", "allAdjacentFoes", "allAdjacent"] -> Right TargetFoe
      | t == "self" -> Right TargetSelf
      | otherwise -> Left ("target " <> t)
    _ -> Left "no target"
  damaging <- case KM.lookup "basePower" o of
    Just (Number n) | Just p <- toBoundedInteger n -> Right ((p :: Int) > 0)
    _ -> Left "no basePower"
  drain <- ratio "drain"
  recoil <- ratio "recoil"
  heal <- ratio "heal"
  boosts <- case look "boosts" of
    Nothing -> Right []
    Just v -> (: []) . EBoost <$> boostTable v
  status <- case look "status" of
    Nothing -> Right []
    Just v -> (: []) . EStatus <$> statusOf v
  self <- case look "self" of
    Nothing -> Right []
    Just (Object s) -> selfEffects "self" s
    Just v -> Left ("self " <> showV v)
  secondaries <- secondariesOf (look "secondary") (look "secondaries")
  ignoreImm <- case look "ignoreImmunity" of
    Nothing -> Right False
    Just (Bool b) -> Right b
    Just v -> Left ("ignoreImmunity " <> showV v)
  pure MoveRule
    { mrDamaging = damaging
    , mrHits = hits
    , mrTarget = target
    , mrOnHit = [EHeal a b | Just (a, b) <- [heal]] ++ boosts ++ status
    , mrSelf = self
    , mrSecondaries = secondaries
    , mrDrain = drain
    , mrRecoil = recoil
    , mrThawsTarget = maybe False truthy (look "thawsTarget")
    , mrDefrost = "defrost" `elem` flags
    , mrIgnoreImmunity = ignoreImm
    }
  where
    look k = case KM.lookup (K.fromText k) o of
      Just v | truthy v -> Just v
      _ -> Nothing
    ratio k = case look k of
      Nothing -> Right Nothing
      Just (Array xs) | [Number a, Number b] <- V.toList xs, Just x <- toBoundedInteger a, Just y <- toBoundedInteger b, y > 0, x > 0 ->
        Right (Just (x, y))
      Just v -> Left (k <> " " <> showV v)
moveRule _ = Left "move is not an object"

checkKey :: (T.Text, Value) -> Either Reason ()
checkKey (k, _)
  | k `elem` dataKeys || k `elem` ruleKeys = Right ()
  | Just _ <- lookup k inertKeys = Right ()
  | k == "handlers" = Right ()
  | otherwise = Left ("not modelled: " <> k)

checkFlag :: T.Text -> Either Reason ()
checkFlag f
  | f == "defrost" = Right () -- translated: 'mrDefrost'
  | Just _ <- lookup f inertFlags = Right ()
  | otherwise = Left ("not modelled: flag " <> f)

-- | JavaScript truthiness of an exported value.
truthy :: Value -> Bool
truthy Null = False
truthy (Bool b) = b
truthy (Number n) = n /= 0
truthy (String t) = not (T.null t)
truthy _ = True

showV :: Value -> T.Text
showV (String t) = t
showV (Number n) = T.pack (show n)
showV (Bool b) = if b then "true" else "false"
showV Null = "null"
showV (Array xs) = "[" <> T.intercalate "," (map showV (V.toList xs)) <> "]"
showV (Object _) = "{..}"

showHandlers :: Value -> T.Text
showHandlers (Array xs) = "(" <> T.intercalate "," [t | String t <- V.toList xs] <> ")"
showHandlers _ = ""

boostTable :: Value -> Either Reason [(BoostStat, Int)]
boostTable (Object b) = traverse one [(K.toText k, v) | (k, v) <- KM.toList b]
  where
    one (k, Number n) | Just x <- toBoundedInteger n = (\s -> (s, x)) <$> stat k
    one (k, _) = Left ("boost " <> k <> " is not an integer")
    stat k = case k of
      "atk" -> Right BAtk
      "def" -> Right BDef
      "spa" -> Right BSpa
      "spd" -> Right BSpd
      "spe" -> Right BSpe
      "accuracy" -> Right BAccuracy
      "evasion" -> Right BEvasion
      _ -> Left ("boost stat " <> k)
boostTable v = Left ("boosts " <> showV v)

statusOf :: Value -> Either Reason StatusId
statusOf (String s) = case s of
  "brn" -> Right Brn
  "par" -> Right Par
  "psn" -> Right Psn
  "tox" -> Right Tox
  "frz" -> Right Frz
  _ -> Left ("status " <> s)
statusOf v = Left ("status " <> showV v)

-- | @move.self@ / @secondary.self@: only stat boosts are translated.
selfEffects :: T.Text -> KM.KeyMap Value -> Either Reason [Effect]
selfEffects what s = do
  mapM_ (\(k, _) -> if k == "boosts" then Right () else Left ("not modelled: " <> what <> "." <> k))
    [(K.toText k, v) | (k, v) <- KM.toList s, truthy v]
  case KM.lookup "boosts" s of
    Just v | truthy v -> (: []) . EBoost <$> boostTable v
    _ -> Right []

-- | @secondaries@, checked against its @secondary@ alias (Showdown's dex copies a lone @secondary@ into
-- @secondaries@).
secondariesOf :: Maybe Value -> Maybe Value -> Either Reason [Secondary]
secondariesOf single many = do
  list <- case many of
    Nothing -> Right []
    Just (Array xs) -> Right (V.toList xs)
    Just v -> Left ("secondaries " <> showV v)
  case single of
    Nothing -> Right ()
    Just s -> unless (list == [s]) (Left "secondary differs from secondaries")
  traverse one list
  where
    one (Object s) = do
      mapM_ (\(k, _) -> if k `elem` ["chance", "boosts", "status", "volatileStatus", "self"] then Right () else Left ("not modelled: secondary." <> k))
        [(K.toText k, v) | (k, v) <- KM.toList s, truthy v]
      chance <- case KM.lookup "chance" s of
        Just (Number n) | Just c <- toBoundedInteger n, c > 0, c <= 100 -> Right c
        _ -> Left "secondary without a chance"
      boosts <- traverse (fmap (: []) . fmap EBoost . boostTable) (truthyLookup "boosts" s)
      status <- traverse (fmap (: []) . fmap EStatus . statusOf) (truthyLookup "status" s)
      flinched <- traverse flinch (truthyLookup "volatileStatus" s)
      let onTarget = concat (concat [maybe [] (: []) boosts, maybe [] (: []) status, maybe [] (: []) flinched])
      onSelf <- case truthyLookup "self" s of
        Just (Object x) -> selfEffects "secondary.self" x
        Just v -> Left ("secondary.self " <> showV v)
        Nothing -> Right []
      when (null onTarget && null onSelf) (Left "secondary without effect")
      pure (Secondary chance onTarget onSelf)
    one v = Left ("secondary " <> showV v)
    flinch (String "flinch") = Right [EFlinch]
    flinch v = Left ("secondary volatileStatus " <> showV v)
    truthyLookup k s = case KM.lookup (K.fromText k) s of
      Just v | truthy v -> Just v
      _ -> Nothing
