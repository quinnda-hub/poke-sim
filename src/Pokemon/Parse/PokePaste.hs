{-# LANGUAGE OverloadedStrings #-}

module Pokemon.Parse.PokePaste where

import           Control.Applicative  (optional, (<|>))
import           Data.Attoparsec.Text (Parser, char, endOfInput, endOfLine,
                                       isEndOfLine, option, satisfy, skipSpace,
                                       takeTill)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T

data AbilityError
  = MultipleAbilitiesError { speciesName :: Text, availableAbilities :: [Text] }
  | NoAbilityInfoError { speciesName :: Text }
  deriving (Show, Eq)

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

-- | Parse a move line.
moveParser :: Parser Text
moveParser = do
  skipSpace
  _ <- char '-'
  _ <- optional (char ' ')
  move <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return (T.strip move)


