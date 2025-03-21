{-# LANGUAGE OverloadedStrings #-}

module Pokemon.Parse.PokePaste where

import           Control.Applicative  (many, (<|>))
import           Data.Attoparsec.Text
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T


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


