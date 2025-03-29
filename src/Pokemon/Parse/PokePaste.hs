{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Parse.PokePaste
  ( parsePokePasteFile
  , ParsedPokemon(..)
  ) where

import           Control.Applicative             (many, optional, (<|>))
import           Data.Attoparsec.Text            (Parser, char, decimal,
                                                  endOfInput, endOfLine,
                                                  isEndOfLine, many1, option,
                                                  parseOnly, satisfy, sepBy,
                                                  sepBy1, skipSpace, string,
                                                  takeTill, try)
import           Data.Either                     (lefts, rights)
import           Data.List                       (foldl')
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Pokemon.Battle.Pokemon.Internal (EVs, IVs, mkEVs, mkIVs)

------------------------------------------------------------
-- Parsers for the Different Sections of a Pokemon Block
------------------------------------------------------------

-- | Parse the header line of a PokePaste, which includes an optional nickname,
--   the species name, the optional gender, and the optional item of the Pokemon.
parseHeaderLine :: Parser (Maybe Text, Text, Maybe Char, Maybe Text)
parseHeaderLine = parseNickAndSpeciesHeader <|> parseSimpleHeader

-- | Parse a header line with a nickname.
--   Pattern: <nickname> ( <species> ) [ ( <gender> ) ] [ @ <item> ]
--   This parser fails if the content inside the first parentheses is "M" or "F",
--   so that the simple header parser can handle that case.
parseNickAndSpeciesHeader :: Parser (Maybe Text, Text, Maybe Char, Maybe Text)
parseNickAndSpeciesHeader = do
  rawNick <- takeTill (== '(')
  let nickStr = T.strip rawNick
  if T.null nickStr
    then fail "No nickname found"
    else do
      _ <- char '('
      rawSpecies <- takeTill (== ')') <* char ')'
      let speciesStr = T.strip rawSpecies
      if speciesStr == "M" || speciesStr == "F"
        then fail "Parenthesized group is gender, not species"
        else do
          skipSpace
          genderVal <- option Nothing $ do
            _ <- char '('
            genderChar <- satisfy (\c -> c == 'M' || c == 'F')
            _ <- char ')'
            return (Just genderChar)
          itemVal <- option Nothing $ do
            skipSpace
            _ <- char '@'
            skipSpace
            rawItem <- takeTill isEndOfLine
            return (Just (T.strip rawItem))
          return (Just nickStr, speciesStr, genderVal, itemVal)

-- | Parse a simple header without a nickname.
--   Pattern: <species> [ ( <gender> ) ] [ @ <item> ]
parseSimpleHeader :: Parser (Maybe Text, Text, Maybe Char, Maybe Text)
parseSimpleHeader = do
  rawSpecies <- takeTill (\c -> c == '@' || c == '(' || isEndOfLine c)
  let speciesStr = T.strip rawSpecies
  genderVal <- option Nothing $ do
    skipSpace
    _ <- char '('
    genderChar <- satisfy (\c -> c == 'M' || c == 'F')
    _ <- char ')'
    return (Just genderChar)
  skipSpace
  itemVal <- option Nothing $ do
    _ <- char '@'
    skipSpace
    rawItem <- takeTill isEndOfLine
    return (Just (T.strip rawItem))
  endOfLine <|> endOfInput
  return (Nothing, speciesStr, genderVal, itemVal)

-- | Parse an ability line, e.g., "Ability: Gale Wings".
parseAbilityLine :: Parser Text
parseAbilityLine = do
  skipSpace
  _ <- string "Ability:"
  skipSpace
  rawAbility <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return (T.strip rawAbility)

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
  levelNumber <- decimal
  endOfLine <|> endOfInput
  return levelNumber

-- | Parse an EVs line, e.g., "EVs: 252 Atk / 4 Def / 252 Spe".
parseEVsLine :: Parser EVs
parseEVsLine = do
  skipSpace
  _ <- string "EVs:"
  skipSpace
  segmentsList <- parseSegment `sepBy1` (skipSpace >> char '/' >> skipSpace)
  let evMap = foldl' (\acc (statName, statValue) -> Map.insert statName statValue acc) Map.empty segmentsList
      evHP  = Map.findWithDefault 0 "HP" evMap
      evAtk = Map.findWithDefault 0 "Atk" evMap
      evDef = Map.findWithDefault 0 "Def" evMap
      evSpa = Map.findWithDefault 0 "SpA" evMap
      evSpd = Map.findWithDefault 0 "Spd" evMap
      evSpe = Map.findWithDefault 0 "Spe" evMap
  endOfLine <|> endOfInput
  maybe (fail $ "Invalid EVs: " ++ show (evHP, evAtk, evDef, evSpa, evSpd, evSpe))
        return
        (mkEVs evHP evAtk evDef evSpa evSpd evSpe)

-- | Parse an IVs line, e.g., "IVs: 0 Atk".
parseIVsLine :: Parser IVs
parseIVsLine = do
  skipSpace
  _ <- string "IVs:"
  skipSpace
  segmentsList <- parseSegment `sepBy1` (skipSpace >> char '/' >> skipSpace)
  let ivMap = foldl' (\acc (statName, statValue) -> Map.insert statName statValue acc) Map.empty segmentsList
      ivHP  = Map.findWithDefault 31 "HP" ivMap
      ivAtk = Map.findWithDefault 31 "Atk" ivMap
      ivDef = Map.findWithDefault 31 "Def" ivMap
      ivSpa = Map.findWithDefault 31 "SpA" ivMap
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
  statValue <- decimal
  skipSpace
  rawStat <- takeTill (\c -> c == '/' || isEndOfLine c)
  return (T.strip rawStat, statValue)

-- | Parse a nature line, e.g., "Jolly Nature".
parseNatureLine :: Parser Text
parseNatureLine = do
  skipSpace
  rawNat <- takeTill (\c -> c == ' ' || isEndOfLine c)
  skipSpace
  _ <- string "Nature"
  endOfLine <|> endOfInput
  return (T.strip rawNat)

-- | Parse a move line.
parseMoveLine :: Parser Text
parseMoveLine = do
  skipSpace
  _ <- char '-'
  _ <- optional (char ' ')
  rawMove <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return (T.strip rawMove)

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
  , ivs      :: Maybe IVs
  , nature   :: Maybe Text
  , moves    :: [Text]
  } deriving (Show, Eq)

-- | Parse a single Pokemon block.
pokemonBlockParser :: Parser ParsedPokemon
pokemonBlockParser = do
  (maybeNick, speciesVal, genderChar, itemText) <- parseHeaderLine
  abilityText <- optional parseAbilityLine
  _ <- optional parseTeraTypeLine
  levelNumber <- optional parseLevelLine
  evsValue <- parseEVsLine
  extras <- many $ try $ Left <$> parseIVsLine <|> Right <$> parseNatureLine
  let ivsValue   = case lefts extras of
                     (firstIV:_) -> Just firstIV
                     []          -> Nothing
      natureValue = case rights extras of
                     (firstNat:_) -> Just firstNat
                     []           -> Nothing
  movesList <- many parseMoveLine
  return ParsedPokemon
           { nickname = maybeNick
           , species  = speciesVal
           , gender   = genderChar
           , item     = itemText
           , ability  = abilityText
           , level    = levelNumber
           , evs      = evsValue
           , ivs      = ivsValue
           , nature   = natureValue
           , moves    = movesList
           }

-- | Parse a team of Pokemon.
--   Pokemon blocks are separated by one or more end-of-line characters.
teamParser :: Parser [ParsedPokemon]
teamParser = pokemonBlockParser `sepBy` many1 endOfLine

-- | Function to parse an entire PokePaste text.
parsePokePaste :: Text -> Either String [ParsedPokemon]
parsePokePaste = parseOnly (skipSpace *> teamParser <* skipSpace <* endOfInput)

-- | Parse a PokePaste text from disk.
parsePokePasteFile :: FilePath -> IO (Either String [ParsedPokemon])
parsePokePasteFile filePath = do
  fileContent <- TIO.readFile filePath
  return (parsePokePaste $ cleanContent fileContent)

-- | Remove any Windows-style carriage returns and trim each line.
cleanContent :: Text -> Text
cleanContent = T.unlines . map T.strip . T.lines . T.replace "\r" ""

