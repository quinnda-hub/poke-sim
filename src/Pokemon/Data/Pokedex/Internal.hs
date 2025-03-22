{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Data.Pokedex.Internal
  ( PokedexMon(..),
    Abilities(..),
    BaseStats(..),
   loadPokedex,
   showLoadPokedexError
   ) where

import           Control.Exception       (IOException, try)
import           Data.Aeson              (FromJSON, eitherDecode, withObject,
                                          (.:), (.:?))
import           Data.Aeson.Types        (FromJSON (parseJSON))
import qualified Data.ByteString.Lazy    as BL
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Map.Strict         (Map)
import           Data.Text               (Text)
import           Pokemon.Mechanics.PType (PType)

-- | Represents a Pokemon in the Pokedex with its relevant properties.
data PokedexMon = PokedexMon
  { abilities :: Abilities      -- ^ Abilities of the Pokemon.
  , baseStats :: BaseStats      -- ^ Base stats of the Pokemon.
  , name      :: Text           -- ^ Name of the Pokemon.
  , tier      :: Maybe Text     -- ^ Competitive tier the Pokemon is in.
  , types     :: NonEmpty PType -- ^ Types of the Pokemon.
  , weightKG  :: Double         -- ^ Weight of the Pokemon.
  } deriving (Show)

-- | Represents the abilities of a Pokemon.
data Abilities = Abilities
  { ability0 :: Text        -- ^ Always present.
  , ability1 :: Maybe Text  -- ^ Second ability.
  , abilityh :: Maybe Text  -- ^ Hidden ability.
  } deriving (Show)

-- | Base stats of the Pokemon.
data BaseStats = BaseStats
  { baseHP  :: Int              -- ^ Hit Points.
  , baseAtk :: Int              -- ^ Attack.
  , baseDef :: Int              -- ^ Defense.
  , baseSpa :: Int              -- ^ Special Attack.
  , baseSpd :: Int              -- ^ Special Defense.
  , baseSpe :: Int              -- ^ Speed.
   } deriving (Show)

-- Parsing JSON instances.

data LoadPokedexError
  = PokedexFileReadError IOException
  | PokedexJSONParseError String
  deriving (Show)

instance FromJSON PokedexMon where
  parseJSON = withObject "PokedexMon" $ \v -> PokedexMon
    <$> v .:   "abilities"
    <*> v .:   "baseStats"
    <*> v .:   "name"
    <*> v .:?  "tier"
    <*> v .:   "types"
    <*> v .:   "weightkg"

loadPokedex :: FilePath -> IO (Either LoadPokedexError (Map Text PokedexMon))
loadPokedex filePath = do
  result <- try $ BL.readFile filePath
  case result of
    Left e        -> return $ Left $ PokedexFileReadError e
    Right content -> return $ first PokedexJSONParseError (eitherDecode content)
  where
    -- Helper function to convert Either a b to Either c b using a function (a -> c)
    first :: (a -> c) -> Either a b -> Either c b
    first f (Left x)  = Left (f x)
    first _ (Right y) = Right y

showLoadPokedexError :: LoadPokedexError -> String
showLoadPokedexError (PokedexFileReadError e)    = "Failed to read Pokédex file: " ++ show e
showLoadPokedexError (PokedexJSONParseError msg) = "Failed to parse Pokédex JSON: " ++ msg

-- | Expects the JSON object to have keys "0", "1", and "H" corresponding to
-- | the primary, secondary, and hidden abilities respectively.
instance FromJSON Abilities where
  parseJSON = withObject "Abilities" $ \v -> Abilities
    <$> v .:  "0"          -- Primary ability (mandatory)
    <*> v .:? "1"          -- Secondary ability (optional)
    <*> v .:? "H"          -- Hidden ability (optional)

-- | FromJSON instance for `BaseStats`.
instance FromJSON BaseStats where
  parseJSON = withObject "BaseStats" $ \v -> BaseStats
    <$> v .: "hp"
    <*> v .: "atk"
    <*> v .: "def"
    <*> v .: "spa"
    <*> v .: "spd"
    <*> v .: "spe"
