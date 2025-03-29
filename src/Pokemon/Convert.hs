{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Convert where

import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, isNothing,
                                                  maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Pokemon.Battle.Pokemon.Internal (EVs, IVs, MoveSet (..),
                                                  Nature (..), Pokemon (..),
                                                  defaultBoosts, mkEVs, mkIVs,
                                                  mkPokemon)
import           Pokemon.Data.Move.Internal      (Move)
import           Pokemon.Data.Pokedex.Internal   (Abilities (..),
                                                  PokedexMon (..))

import           Pokemon.Parse.PokePaste         ()

------------------------------------------------------------
-- Ability Resolution
------------------------------------------------------------

-- | Custom error type for errors that can occur when parsing an ability.
data AbilityError
  = MultipleAbilitiesError { speciesName :: Text, availableAbilities :: [Text] }
  deriving (Show, Eq)

-- | Resolve the ability for a Pokemon.
--   If the ability was provided in the input text file, then use that.
--   Otherwise, if the species only has one ability (ability1 and abilityh are Nothing),
--   then that one ability (ability0) is used.
--   If there are multiple abilities available but none specified in the input,
--   then an error is returned.
resolveAbility :: PokedexMon -> Maybe Text -> Either AbilityError Text
resolveAbility _ (Just abilityText) = Right abilityText
resolveAbility pmon Nothing =
  let Abilities { ability0 = ability0Val, ability1 = ability1Val, abilityh = abilityhVal } = pmon.abilities
      availableAbilitiesList = [ability0Val] ++ maybeToList ability1Val ++ maybeToList abilityhVal
  in if isNothing ability1Val && isNothing abilityhVal
    then Right ability0Val
    else Left $ MultipleAbilitiesError pmon.name availableAbilitiesList



