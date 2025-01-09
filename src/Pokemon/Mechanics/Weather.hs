{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Pokemon.Mechanics.Weather (Weather,
                                  PseudoWeather) where

import           Data.Aeson (FromJSON (parseJSON), withText)
import qualified Data.Text  as T

data Weather = Clear | Hail | Sun | Rain | Sandstorm | Snow
  deriving (Show)

data PseudoWeather = FairyLock | Gravity   | IonDeluge  | MagicRoom
                   | MudSport  | TrickRoom | WaterSport | WonderRoom
                   deriving (Show)

instance FromJSON Weather where
  parseJSON = withText "Weather" $ \t -> case t of
    "clear"          -> pure Clear
    "hail"           -> pure Hail
    "sunnyday"       -> pure Sun
    "RainDance"      -> pure Rain
    "Sandstorm"      -> pure Sandstorm
    "snow"           -> pure Snow
    _                -> fail $ "Invalid Weather value: " ++ T.unpack t                   

instance FromJSON PseudoWeather where
  parseJSON = withText "PseudoWeather" $ \t -> case t of
      "fairylock"  -> pure FairyLock
      "gravity"    -> pure Gravity
      "iondeluge"  -> pure IonDeluge
      "magicroom"  -> pure MagicRoom
      "mudsport"   -> pure MudSport
      "trickroom"  -> pure TrickRoom
      "watersport" -> pure WaterSport
      "wonderroom" -> pure WonderRoom
      _            -> fail $ "Invalid PseudoWeather value: " ++ T.unpack t
