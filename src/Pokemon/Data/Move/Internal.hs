{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Data.Move.Internal
  ( Move(..),
    Accuracy(..),
    Boosts(..),
    Status(..),
    loadMoves,
    showLoadMovesError,
    Target(..),
    MoveCategory(..)
  ) where

import           Control.Applicative       ((<|>))
import           Control.Exception         (IOException, try)

import           Data.Aeson                (FromJSON, Object,
                                            Value (Bool, Number, Object, String),
                                            eitherDecode, withBool, withObject,
                                            withScientific, withText, (.!=),
                                            (.:), (.:?))
import qualified Data.Aeson.Key            as Key
import qualified Data.Aeson.KeyMap         as KM
import           Data.Aeson.Types          (FromJSON (parseJSON), Parser)
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (toLower)
import           Data.Map.Strict           (Map)
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Pokemon.Mechanics.PType   (PType)
import           Pokemon.Mechanics.Weather (PseudoWeather, Weather)

-- | Represents a Pokémon move with all its properties.
data Move = Move
  { -- Basic Move Information
    name                  :: Text                    -- ^ Name of the move.
  , description           :: Maybe Text              -- ^ Detailed description.
  , shortDesc             :: Maybe Text              -- ^ Short description.
  , num                   :: Int                     -- ^ Move number.
  , pp                    :: Int                     -- ^ Power points.
  , priority              :: Int                     -- ^ Move priority.
  , moveType              :: PType                   -- ^ Type of the move.
  , category              :: MoveCategory            -- ^ Physical, Special, or Status.

    -- Accuracy and Power
  , accuracy              :: Maybe Accuracy          -- ^ Accuracy or always hits.
  , basePower             :: Maybe Int               -- ^ Base power; 'Nothing' if non-damaging.
  , basePowerCallback     :: Bool                    -- ^ True if base power depends on state.
  , critRatio             :: Maybe Int               -- ^ Critical hit ratio.
  , damage                :: Maybe FixedDamage       -- ^ Move that does fixed damage.

    -- Effects and Mechanics
  , boosts                :: Maybe Boosts            -- ^ Boosts stats.
  , selfBoost             :: Maybe Boosts            -- ^ Boosts own stats.
  , secondary             :: Maybe SecondaryEffect   -- ^ Secondary effect.
  , secondaries           :: Maybe [SecondaryEffect] -- ^ Multiple secondary effects.
  , status                :: Maybe Status            -- ^ Inflicts status condition.
  , volatileStatus        :: Maybe VolatileStatus    -- ^ Inflicts volatile status.
  , recoil                :: Maybe Recoil            -- ^ Recoil damage.
  , drain                 :: Maybe Drain             -- ^ Drains HP.
  , heal                  :: Maybe Heal              -- ^ Heals user.
  , pseudoWeather         :: Maybe PseudoWeather     -- ^ Sets pseudo-weather.
  , weatherEffect         :: Maybe Weather           -- ^ Sets weather.
  , terrain               :: Maybe Terrain           -- ^ Sets terrain.
  , sideCondition         :: Maybe SideCondition     -- ^ Sets side condition.
  , slotCondition         :: Maybe SlotCondition     -- ^ Sets slot condition.
  , self                  :: Maybe SelfEffect        -- ^ Effect on self.
  , selfDestruct          :: Maybe SelfDestruct      -- ^ Self-destruct effect.
  , condition             :: Maybe Condition         -- ^ Additional condition.
  , maxMove               :: Maybe MaxMove           -- ^ Max move information.
  , zMove                 :: Maybe ZMove             -- ^ Z-move information.
  , ohko                  :: Maybe OHKO              -- ^ One-hit KO move.
  , multihit              :: Maybe Multihit          -- ^ Multi-hit information.
  , flags                 :: Maybe Flags             -- ^ Additional move information.

    -- Move Behaviors
  , forceSwitch           :: Bool                    -- ^ Forces target to switch.
  , sleepUsable           :: Bool                    -- ^ Usable while asleep.
  , ignoreAbility         :: Bool                    -- ^ Ignores abilities.
  , breaksProtect         :: Bool                    -- ^ Breaks through protect.
  , stealsBoosts          :: Bool                    -- ^ Steals stat boosts.
  , tracksTarget          :: Bool                    -- ^ Tracks the target.
  , mindBlownRecoil       :: Bool                    -- ^ 'Mind Blown' recoil.
  , struggleRecoil        :: Bool                    -- ^ 'Struggle' recoil.
  , hasCrashDamage        :: Bool                    -- ^ Crash damage on miss.
  , hasSheerForce         :: Bool                    -- ^ Has 'Sheer Force' effect.
  , willCrit              :: Bool                    -- ^ Always results in a critical hit.
  , callsMove             :: Bool                    -- ^ Calls another move.
  , smartTarget           :: Bool                    -- ^ Targets smartly.
  , stallingMove          :: Bool                    -- ^ Is a stalling move.
  , nonGhostTarget        :: Maybe Text              -- ^ Non-Ghost target.
  , noPPBoosts            :: Bool                    -- ^ Cannot be PP boosted.
  , multiaccuracy         :: Bool                    -- ^ Accuracy for multiple hits.
  , isMax                 :: Maybe IsMax             -- ^ Max move user.
  , isZ                   :: Maybe Text              -- ^ Z-move name.
  , isNonstandard         :: Maybe Text              -- ^ Non-standard move.
  , realMove              :: Maybe Text              -- ^ Real move used.
  , overrideDefensiveStat :: Maybe Text              -- ^ Overrides defensive stat.
  , onDamagePriority      :: Maybe Int               -- ^ On damage priority.
  , thawsTarget           :: Bool                    -- ^ Thaws the target.
  , target                :: Target                  -- ^ Targeting information.
  } deriving (Show, Eq)

-- | Represents the accuracy of a move.
data Accuracy
  = AlwaysHits             -- ^ The move always hits.
  | Percent Int            -- ^ The move has a percentage chance to hit.
  deriving (Show, Eq)

-- | Represents stat boosts.
data Boosts = Boosts
  { acc :: Maybe Int       -- ^ Accuracy boost.
  , atk :: Maybe Int       -- ^ Attack boost.
  , def :: Maybe Int       -- ^ Defense boost.
  , eva :: Maybe Int       -- ^ Evasion boost.
  , spa :: Maybe Int       -- ^ Special Attack boost.
  , spd :: Maybe Int       -- ^ Special Defense boost.
  , spe :: Maybe Int       -- ^ Speed boost.
  } deriving (Show, Eq)

-- | Additional conditions for moves.
data Condition = Condition
  { counterMax                      :: Maybe Int
  , duration                        :: Maybe Int
  , noCopy                          :: Bool
  , onAccuracyPriority              :: Maybe Int
  , onAnyPrepareHitPriority         :: Maybe Int
  , onBasePowerPriority             :: Maybe Int
  , onBeforeMovePriority            :: Maybe Int
  , onCriticalHit                   :: Bool
  , onDamagePriority                :: Maybe Int
  , onEffectivenessPriority         :: Maybe Int
  , onFieldResidualOrder            :: Maybe Int
  , onFieldResidualSubOrder         :: Maybe Int
  , onFoeBeforeMovePriority         :: Maybe Int
  , onFoeRedirectTargetPriority     :: Maybe Int
  , onFoeTrapPokemonPriority        :: Maybe Int
  , onInvulnerability               :: Bool
  , onLockMove                      :: Maybe Text
  , onModifyTypePriority            :: Maybe Int
  , onRedirectTargetPriority        :: Maybe Int
  , onResidualOrder                 :: Maybe Int
  , onResidualSubOrder              :: Maybe Int
  , onSideResidualOrder             :: Maybe Int
  , onSideResidualSubOrder          :: Maybe Int
  , onSourceInvulnerabilityPriority :: Maybe Int
  , onTryHitPriority                :: Maybe Int
  , onTryMovePriority               :: Maybe Int
  , onTryPrimaryHitPriority         :: Maybe Int
  , onTypePriority                  :: Maybe Int
  } deriving (Show, Eq)

-- | Contest types for moves.
data ContestType
  = Beautiful
  | Clever
  | Cool
  | Cute
  | Tough
  deriving (Show, Eq)

-- | Represents fixed damage moves.
data FixedDamage
  = FixedDamage Int        -- ^ Deals a fixed amount of damage.
  | Level                  -- ^ Deals damage equal to the user's level.
  deriving (Show, Eq)

-- | Categories of moves.
data MoveCategory
  = Physical
  | Special
  | Status
  deriving (Show, Eq)

-- | Flags that provide additional information about move properties.
data Flags = Flags
  { allyanim       :: Bool
  , bite           :: Bool
  , bullet         :: Bool
  , bypasssub      :: Bool
  , cantusetwice   :: Bool
  , charge         :: Bool
  , contact        :: Bool
  , dance          :: Bool
  , defrost        :: Bool
  , distance       :: Bool
  , failcopycat    :: Bool
  , failencore     :: Bool
  , failinstruct   :: Bool
  , failmefirst    :: Bool
  , failmimic      :: Bool
  , futureremove   :: Bool
  , gravity        :: Bool
  , heal           :: Bool
  , metronome      :: Bool
  , mirror         :: Bool
  , mustpressure   :: Bool
  , noassist       :: Bool
  , nonsky         :: Bool
  , noparentalbond :: Bool
  , nosketch       :: Bool
  , nosleeptalk    :: Bool
  , pledgecombo    :: Bool
  , powder         :: Bool
  , protect        :: Bool
  , pulse          :: Bool
  , punch          :: Bool
  , recharge       :: Bool
  , reflectable    :: Bool
  , slicing        :: Bool
  , snatch         :: Bool
  , sound          :: Bool
  , wind           :: Bool
  } | SpecialFlag
  deriving (Show, Eq)

-- | Includes information for max moves.
data IsMax
  = IsMaxText Text
  | IsMaxBool Bool
  deriving (Show, Eq)

-- | Represents secondary effects of moves.
data SecondaryEffect = SecondaryEffect
  { chance         :: Maybe Int
  , boosts         :: Maybe Boosts
  , status         :: Maybe Status
  , volatileStatus :: Maybe VolatileStatus
  , self           :: Maybe SelfEffect
  , dustproof      :: Bool
  } deriving (Show, Eq)

-- | Effects that apply to the user of the move.
data SelfEffect = SelfEffect
  { boosts         :: Maybe Boosts
  , volatileStatus :: Maybe Text
  , pseudoWeather  :: Maybe PseudoWeather
  , sideCondition  :: Maybe SideCondition
  , placeHolder    :: Bool
  } deriving (Show, Eq)

-- | Represents self-destruct conditions.
data SelfDestruct
  = Always
  | IfHit
  deriving (Show, Enum, Bounded, Eq)

-- | Side conditions that can be set by moves.
data SideCondition
  = AuroraVeil
  | CraftyShield
  | LightScreen
  | LuckyChant
  | MatBlock
  | Mist
  | QuickGuard
  | Reflect
  | Safeguard
  | Spikes
  | StealthRock
  | StickyWeb
  | Tailwind
  | ToxicSpikes
  | Wideguard
  deriving (Show, Enum, Bounded, Eq)

-- | Slot conditions that can be set by moves.
data SlotCondition
  = HealingWish
  | LunarDance
  | RevivalBlessing
  | Wish
  deriving (Show, Enum, Bounded, Eq)

-- | Status conditions that moves can inflict.
data Status
  = Burn
  | Freeze
  | Paralyze
  | Poison
  | Sleep
  | Toxic
  deriving (Show, Eq)

-- | Targeting information for moves.
data Target
  = AdjacentAlly
  | AdjacentAllyOrSelf
  | AdjacentFoe
  | All
  | AllAdjacent
  | AllAdjacentFoes
  | Allies
  | AllySide
  | AllyTeam
  | Any
  | FoeSide
  | Normal
  | RandomNormal
  | Scripted
  | Self
  deriving (Show, Enum, Bounded, Eq)

-- | Terrains that can be set by moves.
data Terrain
  = Electric
  | Grassy
  | Misty
  | Psychic
  deriving (Show, Eq)

-- | Volatile statuses that moves can inflict.
data VolatileStatus
  = AquaRing
  | Attract
  | BanefulBunker
  | Bide
  | BurningBulwark
  | Charge
  | Confusion
  | Curse
  | DefenseCurl
  | DestinyBond
  | Disable
  | DragonCheer
  | Electrify
  | Embargo
  | Encore
  | Endure
  | Flinch
  | FocusEnergy
  | FollowMe
  | ForeSight
  | GastroAcid
  | Grudge
  | HealBlock
  | HelpingHand
  | Imprison
  | Ingrain
  | KingsShield
  | LaserFocus
  | LeechSeed
  | MagicCoat
  | MagnetRise
  | MaxGuard
  | Minimize
  | MiracleEye
  | Nightmare
  | NoRetreat
  | Obstruct
  | OctoLock
  | PartiallyTrapped
  | Powder
  | PowerShift
  | PowerTrick
  | Protect
  | RagePowder
  | SaltCure
  | SilkTrap
  | SmackDown
  | Snatch
  | SparklingAria
  | SpikyShield
  | Spotlight
  | Stockpile
  | Substitute
  | SyrupBomb
  | TarShot
  | Taunt
  | Telekinesis
  | Torment
  | Yawn
  deriving (Show, Enum, Bounded, Eq)

-- | Information about multi-hit moves.
data Multihit
  = Single                   -- ^ Hits once.
  | Range Int Int            -- ^ Hits between a range.
  | Fixed Int                -- ^ Hits a fixed number of times.
  deriving (Show, Eq)

-- | Represents one-hit KO moves.
data OHKO
  = Standard Bool            -- ^ Standard OHKO move.
  | Ice Text                 -- ^ Ice-type OHKO move.
  deriving (Show, Eq)

-- | Represents recoil damage.
data Recoil = Recoil Int Int -- ^ Recoil numerator and denominator.
  deriving (Show, Eq)

-- | Represents moves thats drain HP.
data Drain = Drain Int Int
  deriving (Show, Eq)

-- | Moves that heal.
data Heal = Heal Int Int
  deriving (Show, Eq)

-- | Represents Z-Move information.
data ZMove = ZMove
  { basePower :: Maybe Int
  , effect    :: Maybe Text
  , boost     :: Maybe Boosts
  } deriving (Show, Eq)

-- | Represents Max Move information.
newtype MaxMove = MaxMove
  { basePower :: Maybe Int
  } deriving (Show, Eq)

-- Parsing JSON instances

instance FromJSON Move where
  parseJSON = withObject "Move" $ \v -> Move
    -- Basic Move Information
    <$> v .:  "name"
    <*> v .:? "desc"
    <*> v .:? "shortDesc"
    <*> v .:  "num"
    <*> v .:  "pp"
    <*> v .:  "priority"
    <*> v .:  "type"
    <*> v .:  "category"

    -- Accuracy and Power
    <*> v .:? "accuracy"
    <*> v .:? "basePower"
    <*> v .:? "basePowerCallback"         .!= False
    <*> v .:? "critRatio"
    <*> v .:? "damage"

    -- Effects and Mechanics
    <*> v .:? "boosts"
    <*> v .:? "selfBoost"
    <*> v .:? "secondary"
    <*> v .:? "secondaries"
    <*> v .:? "status"
    <*> v .:? "volatileStatus"
    <*> v .:? "recoil"
    <*> v .:? "drain"
    <*> v .:? "heal"
    <*> v .:? "pseudoWeather"
    <*> v .:? "weather"
    <*> v .:? "terrain"
    <*> v .:? "sideCondition"
    <*> v .:? "slotCondition"
    <*> v .:? "self"
    <*> v .:? "selfDestruct"
    <*> v .:? "condition"
    <*> v .:? "maxMove"
    <*> v .:? "zMove"
    <*> v .:? "ohko"
    <*> v .:? "multihit"
    <*> v .:? "flags"

    -- Move Behaviors
    <*> v .:? "forceSwitch"               .!= False
    <*> v .:? "sleepUsable"               .!= False
    <*> v .:? "ignoreAbility"             .!= False
    <*> v .:? "breaksProtect"             .!= False
    <*> v .:? "stealsBoosts"              .!= False
    <*> v .:? "tracksTarget"              .!= False
    <*> v .:? "mindBlownRecoil"           .!= False
    <*> v .:? "struggleRecoil"            .!= False
    <*> v .:? "hasCrashDamage"            .!= False
    <*> v .:? "hasSheerForce"             .!= False
    <*> v .:? "willCrit"                  .!= False
    <*> v .:? "callsMove"                 .!= False
    <*> v .:? "smartTarget"               .!= False
    <*> v .:? "stallingMove"              .!= False
    <*> v .:? "nonGhostTarget"
    <*> v .:? "noPPBoosts"                .!= False
    <*> v .:? "multiaccuracy"             .!= False
    <*> v .:? "isMax"
    <*> v .:? "isZ"
    <*> v .:? "isNonstandard"
    <*> v .:? "realMove"
    <*> v .:? "overrideDefensiveStat"
    <*> v .:? "onDamagePriority"
    <*> v .:? "thawsTarget"               .!= False
    <*> v .:  "target"

-- Generic function that can parse any sum type fitting the constraints.
parseLowercaseEnum :: (Bounded a, Enum a, Show a) => String -> Value -> Parser a
parseLowercaseEnum typeName = withText typeName $ \t ->
  let tLower = T.unpack $ T.toLower t
      constructors = [minBound .. maxBound] :: (Enum a, Bounded a) => [a]
      lookupTable = [(map toLower $ show c, c) | c <- constructors]
  in case lookup tLower lookupTable of
       Just c  -> pure c
       Nothing -> fail $ "Invalid " ++ typeName ++ " value: " ++ T.unpack t

instance FromJSON Accuracy where
  parseJSON v = parseAlwaysHits v <|> parsePercent v

parseAlwaysHits :: Value -> Parser Accuracy
parseAlwaysHits = withBool "Accuracy" $ \b ->
  if b
    then pure AlwaysHits
    else fail "Boolean accuracy must be true for AlwaysHits."

parsePercent :: Value -> Parser Accuracy
parsePercent = withScientific "Accuracy" $ \n ->
  if n >= 0 && n <= 100
    then pure $ Percent (round n)
    else fail "Accuracy percentage must be between 0 and 100."

instance FromJSON Boosts where
  parseJSON = withObject "Boosts" $ \v -> Boosts
    <$> v .:? "accuracy"
    <*> v .:? "atk"
    <*> v .:? "def"
    <*> v .:? "evasion"
    <*> v .:? "spa"
    <*> v .:? "spd"
    <*> v .:? "spe"

instance FromJSON Condition where
  parseJSON = withObject "Condition" $ \v -> Condition
    <$> v .:? "counterMax"
    <*> v .:? "duration"
    <*> v .:? "noCopy"                          .!= False
    <*> v .:? "onAccuracyPriority"
    <*> v .:? "onAnyPrepareHitPriority"
    <*> v .:? "onBasePowerPriority"
    <*> v .:? "onBeforeMovePriority"
    <*> v .:? "onCriticalHit"                   .!= False
    <*> v .:? "onDamagePriority"
    <*> v .:? "onEffectivenessPriority"
    <*> v .:? "onFieldResidualOrder"
    <*> v .:? "onFieldResidualSubOrder"
    <*> v .:? "onFoeBeforeMovePriority"
    <*> v .:? "onFoeRedirectTargetPriority"
    <*> v .:? "onFoeTrapPokemonPriority"
    <*> v .:? "onInvulnerability"               .!= False
    <*> v .:? "onLockMove"
    <*> v .:? "onModifyTypePriority"
    <*> v .:? "onRedirectTargetPriority"
    <*> v .:? "onResidualOrder"
    <*> v .:? "onResidualSubOrder"
    <*> v .:? "onSideResidualOrder"
    <*> v .:? "onSideResidualSubOrder"
    <*> v .:? "onSourceInvulnerabilityPriority"
    <*> v .:? "onTryHitPriority"
    <*> v .:? "onTryMovePriority"
    <*> v .:? "onTryPrimaryHitPriority"
    <*> v .:? "onTypePriority"

instance FromJSON ContestType where
  parseJSON = withText "ContestType" $ \t -> case t of
    "Beautiful" -> pure Beautiful
    "Clever"    -> pure Clever
    "Cool"      -> pure Cool
    "Tough"     -> pure Tough
    _           -> fail $ "Invalid ContestType: " ++ T.unpack t

instance FromJSON FixedDamage where
  parseJSON v =
    (withScientific "FixedDamage" $ \n -> pure $ FixedDamage (round n)) v <|>
    (withText "FixedDamage" $ \t ->
      if t == "level"
        then pure Level
        else fail "Expected 'level' for Level based Fixed-Damage") v

instance FromJSON MoveCategory where
  parseJSON = withText "MoveCategory" $ \t -> case t of
    "Physical" -> pure Physical
    "Special"  -> pure Special
    "Status"   -> pure Status
    _          -> fail $ "Invalid MoveCategory: " ++ T.unpack t

parseFlag :: Object -> Text -> Parser Bool
parseFlag v field = do
  let key = Key.fromText field
  mVal <- v .:? key
  case mVal of
    Just (Number n) | n == 1 -> pure True
    _                        -> pure False

instance FromJSON Flags where
  parseJSON (Object v)
    | KM.null v = pure SpecialFlag  -- Empty object corresponds to SpecialFlag
    | otherwise = Flags
        <$> parseFlag v "allyanim"
        <*> parseFlag v "bite"
        <*> parseFlag v "bullet"
        <*> parseFlag v "bypasssub"
        <*> parseFlag v "cantusetwice"
        <*> parseFlag v "charge"
        <*> parseFlag v "contact"
        <*> parseFlag v "dance"
        <*> parseFlag v "defrost"
        <*> parseFlag v "distance"
        <*> parseFlag v "failcopycat"
        <*> parseFlag v "failencore"
        <*> parseFlag v "failinstruct"
        <*> parseFlag v "failmefirst"
        <*> parseFlag v "failmimic"
        <*> parseFlag v "futureremove"
        <*> parseFlag v "gravity"
        <*> parseFlag v "heal"
        <*> parseFlag v "metronome"
        <*> parseFlag v "mirror"
        <*> parseFlag v "mustpressure"
        <*> parseFlag v "noassist"
        <*> parseFlag v "nonsky"
        <*> parseFlag v "noparentalbond"
        <*> parseFlag v "nosketch"
        <*> parseFlag v "nosleeptalk"
        <*> parseFlag v "pledgecombo"
        <*> parseFlag v "powder"
        <*> parseFlag v "protect"
        <*> parseFlag v "pulse"
        <*> parseFlag v "punch"
        <*> parseFlag v "recharge"
        <*> parseFlag v "reflectable"
        <*> parseFlag v "slicing"
        <*> parseFlag v "snatch"
        <*> parseFlag v "sound"
        <*> parseFlag v "wind"
  parseJSON _ = fail "Expected an object for Flags"

instance FromJSON IsMax where
  parseJSON (String t) = pure $ IsMaxText t
  parseJSON (Bool b)   = pure $ IsMaxBool b
  parseJSON _          = fail "Expected a String or Bool for IsMax."

instance FromJSON SecondaryEffect where
  parseJSON = withObject "SecondaryEffect" $ \v -> SecondaryEffect
    <$> v .:? "chance"
    <*> v .:? "boosts"
    <*> v .:? "status"
    <*> v .:? "volatileStatus"
    <*> v .:? "self"
    <*> v .:? "dustproof" .!= False

instance FromJSON SelfEffect where
  parseJSON = withObject "SelfEffect" $ \v -> SelfEffect
    <$> v .:? "boosts"
    <*> v .:? "volatileStatus"
    <*> v .:? "pseudoWeather"
    <*> v .:? "sideCondition"
    <*> v .:? "placeHolder" .!= False

instance FromJSON SelfDestruct where
  parseJSON = parseLowercaseEnum "SelfDestruct"

instance FromJSON SideCondition where
  parseJSON = parseLowercaseEnum "SideCondition"

instance FromJSON SlotCondition where
  parseJSON = parseLowercaseEnum "SlotCondition"

instance FromJSON Status where
  parseJSON = withText "Status" $ \t -> case t of
    "brn" -> pure Burn
    "frz" -> pure Freeze
    "par" -> pure Paralyze
    "psn" -> pure Poison
    "slp" -> pure Sleep
    "tox" -> pure Toxic
    _     -> fail $ "Invalid Status: " ++ T.unpack t

instance FromJSON Target where
  parseJSON = parseLowercaseEnum "Target"

instance FromJSON Terrain where
  parseJSON = withText "Terrain" $ \t -> case t of
    "electricterrain" -> pure Electric
    "grassyterrain"   -> pure Grassy
    "mistyterrain"    -> pure Misty
    "psychicterrain"  -> pure Psychic
    _                 -> fail $ "Invalid Terrain: " ++ T.unpack t

instance FromJSON VolatileStatus where
  parseJSON = parseLowercaseEnum "VolatileStatus"

instance FromJSON Multihit where
  parseJSON v =
    parseSingle v <|> parseFixed v <|> parseRange v

parseSingle :: Value -> Parser Multihit
parseSingle = withScientific "Multihit" $ \n ->
  if n == 1
    then pure Single
    else fail "Expected 1 for Single Multihit"

parseFixed :: Value -> Parser Multihit
parseFixed = withScientific "Multihit" $ \n ->
  if n >= 2
    then pure $ Fixed (round n)
    else fail "Expected number >= 2 for Fixed Multihit"

parseRange :: Value -> Parser Multihit
parseRange v = do
  nums <- parseJSON v :: Parser [Int]
  case nums of
    [low, high] -> pure $ Range low high
    _           -> fail "Expected array of two numbers for Range Multihit."

instance FromJSON OHKO where
  parseJSON v = parseStandard v <|> parseIce v

parseStandard :: Value -> Parser OHKO
parseStandard = withBool "OHKO" $ \b -> pure $ Standard b

parseIce :: Value -> Parser OHKO
parseIce = withText "OHKO" $ \t ->
  if t == "Ice"
    then pure $ Ice t
    else fail "Expected 'Ice' for Ice OHKO."

instance FromJSON Recoil where
  parseJSON v = do
    nums <- parseJSON v :: Parser [Int]
    case nums of
      [nume, denom] -> pure $ Recoil nume denom
      _             -> fail "Expected array of two numbers for Recoil."

instance FromJSON Drain where
  parseJSON v = do
    nums <- parseJSON v :: Parser [Int]
    case nums of
      [nume, denom] -> pure $ Drain nume denom
      _             -> fail "Expected array of two numbers for Recoil."


instance FromJSON Heal where
  parseJSON v = do
    nums <- parseJSON v :: Parser [Int]
    case nums of
      [nume, denom] -> pure $ Heal nume denom
      _             -> fail "Expected array of two numbers for Heal."

instance FromJSON ZMove where
  parseJSON = withObject "ZMove" $ \v -> ZMove
    <$> v .:? "basePower"
    <*> v .:? "effect"
    <*> v .:? "boost"

instance FromJSON MaxMove where
  parseJSON = withObject "MaxMove" $ \v -> MaxMove
    <$> v .:? "basePower"

-- Move IO.

data LoadMovesError
  = FileReadError IOException
  | JSONParseError String
  deriving (Show, Eq)

loadMoves :: FilePath -> IO (Either LoadMovesError (Map Text Move))
loadMoves filePath = do
  result <- try $ BL.readFile filePath
  case result of
    Left e        -> pure $ Left $ FileReadError e
    Right content -> pure $ first JSONParseError (eitherDecode content)
  where
    first f (Left x)  = Left (f x)
    first _ (Right y) = Right y

showLoadMovesError :: LoadMovesError -> String
showLoadMovesError (FileReadError e)    = "Failed to read file: " ++ show e
showLoadMovesError (JSONParseError msg) = "Failed to parse JSON: " ++ msg

