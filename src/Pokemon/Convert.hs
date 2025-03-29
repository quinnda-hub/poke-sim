{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Convert where

import           Data.Map.Strict                 (Map)
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

-- | Custom error type for errors that can occur when parsing an ability.
data AbilityError
  = MultipleAbilitiesError { speciesName :: Text, availableAbilities :: [Text] }
  deriving (Show, Eq)

-- | Errors that can occur when parsing a nature.
newtype NatureError
  = UnknownNature { unknownNature :: Text }
  deriving (Show, Eq)

-- | Errors that can occur when building a move set.
data MoveSetError
  = NoMovesProvidedError
  | UnknownMove { unknownMove :: Text }

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

-- | Parse a nature from a text representation.
parseNature :: Text -> Either NatureError Nature
parseNature natText =
  case reads (T.unpack natText) of
    [(n, "")] -> Right n
    _         -> Left $ UnknownNature natText

-- | Build a MoveSet from a list of move names, looking them up in the move map.
--   The first move is always required.
buildMoveSet :: Map Text Move -> [Text] -> Either MoveSetError MoveSet
buildMoveSet moveMap moveNames =
  case moveNames of
    [] -> Left NoMovesProvidedError
    (m1:rest) -> do
      let lookupMove moveName = maybe (Left $ UnknownMove moveName )
                                      Right $ Map.lookup moveName moveMap
      move1Val <- lookupMove m1
      let move2Val = case rest of
                       (m2:_) -> Just <$> lookupMove m2
                       []     -> Right Nothing
      move2Final <- move2Val
      let move3Val = case drop 1 rest of
                       (m3:_) -> Just <$> lookupMove m3
                       []     -> Right Nothing
      move3Final <- move3Val
      let move4Val = case drop 2 rest of
                       (m4:_) -> Just <$> lookupMove m4
                       []     -> Right Nothing
      move4Final <- move4Val
      return $ MoveSet
                 { move1 = move1Val
                 , move2 = move2Final
                 , move3 = move3Final
                 , move4 = move4Final
                 }





