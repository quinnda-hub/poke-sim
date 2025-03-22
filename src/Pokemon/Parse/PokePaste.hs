{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Parse.PokePaste where

import           Control.Applicative             (optional, (<|>))
import           Data.Attoparsec.Text            (Parser, char, decimal,
                                                  endOfInput, endOfLine,
                                                  isEndOfLine, option, satisfy,
                                                  sepBy1, skipSpace, string,
                                                  takeTill)
import           Data.List                       (foldl')
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, isNothing,
                                                  maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Pokemon.Battle.Pokemon.Internal (EVs, mkEVs)
import           Pokemon.Data.Pokedex.Internal   (Abilities (..),
                                                  PokedexMon (..))

------------------------------------------------------------
-- Ability Resolution
------------------------------------------------------------

-- | Custom error type for erros that can occur when parsing an ability.
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
resolveAbility _ (Just ability) = Right ability
resolveAbility pmon Nothing =
  let Abilities { ability0 = a0, ability1 = a1, abilityh = ah } = pmon.abilities
      availableAbilities' = [a0] ++ maybeToList a1 ++ maybeToList ah
  in if isNothing a1 && isNothing ah
    then Right a0
    else Left $ MultipleAbilitiesError pmon.name availableAbilities'

-- | Parse the header line of a PokePaste, which includes an optional nickname,
--   the species name, the optional gender, and the optinal item of the Pokemon.
parseHeaderLine ::  Parser (Maybe Text, Text, Maybe Char, Maybe Text)
parseHeaderLine = do
  -- Try to parse a nickname (anything before a '('))
  nick <- option Nothing $ do
    n <- takeTill (== '(')
    _ <- char '('
    return (Just $ T.strip n)
  -- Parse the Pokemon species name up to a closing ')'
  species <- option (T.strip $ fromMaybe "" nick) (takeTill (== ')') <* char ')')
  -- Optionally parse gender in parenthesis
  gender <- option Nothing $ do
    skipSpace
    _ <- char '('
    g <- satisfy (\c -> c == 'M' || c == 'F')
    _ <- char ')'
    return (Just g)
  -- Optionally parse an item if present
  item <- option Nothing $ do
    skipSpace
    _ <- char '@'
    skipSpace
    i <- takeTill isEndOfLine
    return (Just (T.strip i))
  return (nick, T.strip species, gender, item)

-- | Parse an ability line, e.g., "Ability: Gale Wings".
parseAbilityLine :: Parser Text
parseAbilityLine = do
  skipSpace
  _ <- string "Ability:"
  skipSpace
  ability <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return (T.strip ability)

-- | Parse a level line, e.g., "Level: 100".
parseLevel :: Parser Int
parseLevel = do
  skipSpace
  _ <- string "Level:"
  skipSpace
  lvl <- decimal
  endOfLine <|> endOfInput
  return lvl

-- Parse an EVs line, e.g., "EVs: 252 Atk / 4 Def / 252 spe".
parseEVsLine :: Parser EVs
parseEVsLine = do
  skipSpace
  _ <- string "EVs:"
  skipSpace
  segments <- parseSegment `sepBy1` (skipSpace >> char '/' >> skipSpace)
  let evMap = foldl' (\m (stat, value) -> Map.insert stat value m) Map.empty segments
      evHP  = Map.findWithDefault 0 "HP" evMap
      evAtk = Map.findWithDefault 0 "Atk" evMap
      evDef = Map.findWithDefault 0 "Def" evMap
      evSpa = Map.findWithDefault 0 "Spa" evMap
      evSpd = Map.findWithDefault 0 "Spd" evMap
      evSpe = Map.findWithDefault 0 "Spe" evMap
  endOfLine <|> endOfInput
  maybe (fail $ "Invalid EVs: " ++ show (evHP, evAtk, evDef, evSpa, evSpd, evSpe))
        return
        (mkEVs evHP evAtk evDef evSpa evSpd evSpe)

-- | Parse a segment of stat to value, e.g., "252 Atk" -> ("Atk", 252).
parseSegment :: Parser (Text, Int)
parseSegment = do
  skipSpace
  value <- decimal
  skipSpace
  stat <- takeTill (\c -> c == '/' || isEndOfLine c)
  return (T.strip stat, value)

-- | Parse a move line.
moveParser :: Parser Text
moveParser = do
  skipSpace
  _ <- char '-'
  _ <- optional (char ' ')
  move <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return (T.strip move)


