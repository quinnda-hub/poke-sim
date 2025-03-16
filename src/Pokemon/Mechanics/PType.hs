{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Pokemon.Mechanics.PType (PType(..)) where

import           Data.Aeson   (FromJSON)
import           GHC.Generics (Generic)

data PType = Normal | Fire   | Water  | Electric | Grass | Ice  | Fighting
           | Poison | Ground | Flying | Psychic  | Bug   | Rock | Ghost
           | Dragon | Dark   | Steel  | Fairy    | Typeless
    deriving (Show, Eq, Generic, FromJSON)
