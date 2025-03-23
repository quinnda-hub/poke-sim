{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Pokemon.Parse.PokePaste where

import           Control.Applicative             (many, optional, (<|>))
import           Data.Attoparsec.Text            (Parser, char, decimal,
                                                  endOfInput, endOfLine,
                                                  isEndOfLine, many1, option,
                                                  parseOnly, peekChar, satisfy,
                                                  sepBy, sepBy1, skipSpace,
                                                  string, takeTill)
import           Data.List                       (foldl')
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, isNothing,
                                                  maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Pokemon.Battle.Pokemon.Internal (EVs, IVs, mkEVs, mkIVs)
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

------------------------------------------------------------
-- Parsers for the Different Sections of a Pokemon Block
------------------------------------------------------------

-- | Parse the header line of a PokePaste, which includes an optional nickname,
--   the species name, the optional gender, and the optinal item of the Pokemon.
parseHeaderLine :: Parser (Maybe Text, Text, Maybe Char, Maybe Text)
parseHeaderLine = parseNickAndSpeciesHeader <|> parseSimpleHeader

-- | Parse a header line with a nickname.
--   Pattern: <nickname> ( <species> ) [ ( <gender> ) ] [ @ <item> ]
--   This parser fails if the content inside the first parentheses is "M" or "F",
--   so that the simple header parser can handle that case.
parseNickAndSpeciesHeader :: Parser (Maybe Text, Text, Maybe Char, Maybe Text)
parseNickAndSpeciesHeader = do
  -- Parse everything before the first '(' as a potential nickname.
  nick <- takeTill (== '(')
  let nick' = T.strip nick
  if T.null nick'
    then fail "No nickname found"
    else do
      _ <- char '('
      species <- takeTill (== ')') <* char ')'
      let species' = T.strip species
      -- If the parenthesized group is just a gender indicator, then this branch isn’t valid.
      if species' == "M" || species' == "F"
        then fail "Parenthesized group is gender, not species"
        else do
          skipSpace
          gender <- option Nothing $ do
            _ <- char '('
            g <- satisfy (\c -> c == 'M' || c == 'F')
            _ <- char ')'
            return (Just g)
          item <- option Nothing $ do
            skipSpace
            _ <- char '@'
            skipSpace
            i <- takeTill isEndOfLine
            return (Just (T.strip i))
          return (Just nick', species', gender, item)

-- | Parse a simple header without a nickname.
--   Pattern: <species> [ ( <gender> ) ] [ @ <item> ]
parseSimpleHeader :: Parser (Maybe Text, Text, Maybe Char, Maybe Text)
parseSimpleHeader = do
  species <- takeTill (\c -> c == '@' || c == '(' || isEndOfLine c)
  let species' = T.strip species
  gender <- option Nothing $ do
    skipSpace
    _ <- char '('
    g <- satisfy (\c -> c == 'M' || c == 'F')
    _ <- char ')'
    return (Just g)
  skipSpace
  item <- option Nothing $ do
    _ <- char '@'
    skipSpace
    i <- takeTill isEndOfLine
    return (Just (T.strip i))
  endOfLine <|> endOfInput
  return (Nothing, species', gender, item)

-- | Parse an ability line, e.g., "Ability: Gale Wings".
parseAbilityLine :: Parser Text
parseAbilityLine = do
  skipSpace
  _ <- string "Ability:"
  skipSpace
  ability <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return (T.strip ability)

-- | Parse a tera type line and ignore it (for now).
parseTeraTypeLine :: Parser ()
parseTeraTypeLine = do
  skipSpace
  _ <- string "Tera Type:"
  skipSpace
  _ <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return ()

-- | Parse a level line, e.g., "Level: 100".
parseLevelLine :: Parser Int
parseLevelLine = do
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

-- Parse an IVs line, e.g., "IVs: 0 Atk"
parseIVsLine :: Parser IVs
parseIVsLine = do
  skipSpace
  _ <- string "IVs:"
  skipSpace
  segments <- parseSegment `sepBy1` (skipSpace >> char '/' >> skipSpace)
  let ivMap = foldl' (\m (stat, value) -> Map.insert stat value m) Map.empty segments
      ivHP  = Map.findWithDefault 31 "HP" ivMap
      ivAtk = Map.findWithDefault 31 "Atk" ivMap
      ivDef = Map.findWithDefault 31 "Def" ivMap
      ivSpa = Map.findWithDefault 31 "Spa" ivMap
      ivSpd = Map.findWithDefault 31 "Spd" ivMap
      ivSpe = Map.findWithDefault 31 "Spe" ivMap
  endOfLine <|> endOfInput
  maybe (fail $ "Invalid IVs: " ++ show (ivHP, ivAtk, ivDef, ivSpa, ivSpd, ivSpe))
        return
        (mkIVs ivHP ivAtk ivDef ivSpa ivSpd ivSpe)

-- | Parse a segment of stat to value, e.g., "252 Atk" -> ("Atk", 252).
parseSegment :: Parser (Text, Int)
parseSegment = do
  skipSpace
  value <- decimal
  skipSpace
  stat <- takeTill (\c -> c == '/' || isEndOfLine c)
  return (T.strip stat, value)

-- | Parse a nature line, e.g., "Jolly Nature".
parseNatureLine :: Parser Text
parseNatureLine = do
  skipSpace
  nat <- takeTill (\c -> c == ' ' || isEndOfLine c)
  skipSpace
  _ <- string "Nature"
  endOfLine <|> endOfInput
  return (T.strip nat)

-- | Parse a move line.
parseMoveLine :: Parser Text
parseMoveLine = do
  skipSpace
  _ <- char '-'
  _ <- optional (char ' ')
  move <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return (T.strip move)

------------------------------------------------------------
-- Intermediate Parsed Data Type
------------------------------------------------------------

-- | Data type to represent a parsed Pokemon from a PokePaste text.
data ParsedPokemon = ParsedPokemon
  { nickname :: Maybe Text
  , species  :: Text
  , gender   :: Maybe Char
  , item     :: Maybe Text
  , ability  :: Maybe Text
  , level    :: Maybe Int
  , evs      :: EVs
  , ivs      :: IVs
  , nature   :: Maybe Text
  , moves    :: [Text]
  } deriving (Show, Eq)

-- | Parse a single Pokemon block.
pokemonBlockParser :: Parser ParsedPokemon
pokemonBlockParser = do
  (nick, species, gender, item) <- parseHeaderLine
  ability <- optional parseAbilityLine
  _       <- optional parseTeraTypeLine
  level   <- optional parseLevelLine
  evs     <- parseEVsLine
  ivs     <- parseIVsLine
  nature  <- optional parseNatureLine
  moves   <- many parseMoveLine
  return ParsedPokemon
           { nickname = nick
           , species  = species
           , gender   = gender
           , item     = item
           , ability  = ability
           , level    = level
           , evs      = evs
           , ivs      = ivs
           , nature   = nature
           , moves    = moves
           }

-- | Parse a team of Pokemon.
--   Pokemon blocks are separated by one or more spaces.
teamParser :: Parser [ParsedPokemon]
teamParser = pokemonBlockParser `sepBy` many1 endOfLine

-- | Function to parse an entire PokePaste text.
parsePokePaste :: Text -> Either String [ParsedPokemon]
parsePokePaste = parseOnly (skipSpace *> teamParser <* skipSpace <* endOfInput)

-- | Parse a PokePaste text from disk.
parsePokePasteFile :: FilePath -> IO (Either String [ParsedPokemon])
parsePokePasteFile filePath = do
  content <- TIO.readFile filePath
  return (parsePokePaste content)

