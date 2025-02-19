{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Pretty
  ( printPokemon
  ) where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Pokemon.Battle.Pokemon.Internal (MoveSet(..), Pokemon (..))
import Pokemon.Data.Move.Internal (Move(..))
import           Pokemon.Mechanics.PType         (PType)
import qualified Data.Text.IO as TIO

-- | Pretty print a Pokemon.
prettyPokemon :: Pokemon -> Text
prettyPokemon p =
  T.unlines
    [ T.concat ["Name: ", p.name]
    , T.concat ["Level: ", T.pack (show p.level)]
    , T.concat ["Type: ", prettyPType p.type1, prettyMaybePType p.type2]
    , "Stats:"
    , T.concat ["  HP:  ", T.pack (show p.hp)]
    , T.concat ["  Atk: ", T.pack (show p.atk)]
    , T.concat ["  Def: ", T.pack (show p.def)]
    , T.concat ["  SpA: ", T.pack (show p.spa)]
    , T.concat ["  SpD: ", T.pack (show p.spd)]
    , T.concat ["  Spe: ", T.pack (show p.spe)]
    , "Moves:"
    , prettyMoveSet p.moves
    ]

-- | Helper to pretty print a primary type.
prettyPType :: PType -> Text
prettyPType = T.pack . show

-- | Helper to pretty print an optional secondary type.
prettyMaybePType :: Maybe PType -> Text
prettyMaybePType Nothing  = ""
prettyMaybePType (Just t) = T.concat ["/", prettyPType t]

-- | Pretty print the MoveSet.
prettyMoveSet :: MoveSet -> Text
prettyMoveSet ms =
  T.unlines $
    [ T.concat ["  Move 1: ", ms.move1.name] ]
    ++ prettyMaybeMove "Move 2" ms.move2
    ++ prettyMaybeMove "Move 3" ms.move3
    ++ prettyMaybeMove "Move 4" ms.move4
  where
    prettyMaybeMove :: Text -> Maybe Move -> [Text]
    prettyMaybeMove label Nothing   = [T.concat ["  ", label, ": None"]]
    prettyMaybeMove label (Just m)  = [T.concat ["  ", label, ": ", m.name]]

-- Print a Pokemon to the console.
printPokemon :: Pokemon -> IO ()
printPokemon p = TIO.putStrLn (prettyPokemon p)
