{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Pretty
  ( printPokemon
  , printLogMessages
  ) where

import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Pokemon.Battle.Logging          (LogMessage (..))
import           Pokemon.Battle.Pokemon.Internal (MoveSet (..), Pokemon (..),
                                                  PokemonBoosts)
import qualified Pokemon.Battle.Pokemon.Internal as P
import           Pokemon.Data.Move.Internal      (Boosts, Move (..))
import qualified Pokemon.Data.Move.Internal      as M
import           Pokemon.Mechanics.PType         (PType)

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
    prettyMaybeMove label Nothing  = [T.concat ["  ", label, ": None"]]
    prettyMaybeMove label (Just m) = [T.concat ["  ", label, ": ", m.name]]

-- Print a Pokemon to the console.
printPokemon :: Pokemon -> IO ()
printPokemon p = TIO.putStrLn (prettyPokemon p)

--------------------------------------------------------------------------------
-- Pretty Logging
--------------------------------------------------------------------------------

-- | Pretty print a single log message.
prettyLogMessage :: LogMessage -> Text
prettyLogMessage (DamageLog dmg tgt) =
  T.concat ["[Damage] ", T.pack (show dmg), " damage dealt to ", tgt]
prettyLogMessage (ActionLog mvName act) =
  T.concat ["[Action] ", act, " used ", mvName]
prettyLogMessage (TurnLog n) =
  T.concat ["==== Turn ", T.pack (show n), " ===="]
prettyLogMessage (WeatherLog w) =
  T.concat ["[Weather] ", T.pack (show w)]
prettyLogMessage (VictoryLog v) =
  T.concat ["[Victory] ", v, " wins!"]
prettyLogMessage (MissLog w) =
  T.concat ["[Miss] ", w, " missed their attack!"]
prettyLogMessage (CritLog _) =
  T.concat ["[Critical Hit] ", "Critical hit!"]
prettyLogMessage (StatusLog a s) =
  T.concat ["[Status] ", s, " is ", a <> "ed"]
prettyLogMessage (BoostsLog a mb pb) =
  prettyBoostsLogMessage a mb pb

-- | Given a stat name, the boost change from the move (delta),
--   and  the new total value, produce a pretty message for the stat. :w
prettyBoostChange :: Text -> Int -> Int -> Text
prettyBoostChange statName delta newVal
  | delta > 0 =
    if newVal == 6 then statName <> " won't go any higher!"
    else if delta == 1 then statName <> " rose!"
    else if delta >= 2 then statName <> " rose sharply!"
    else ""
  | delta < 0 =
    if newVal == (-6) then statName <> " won't go any lower!"
    else if delta == (-1) then statName <> " fell!"
    else if delta <= (-2) then statName <> " fell sharply!"
    else ""
  | otherwise = ""

-- | Pretty print a boost log message.
prettyBoostsLogMessage :: Text -> Boosts -> PokemonBoosts -> Text
prettyBoostsLogMessage targetName moveBoost newBoost =
  let accMsg = prettyBoostChange "Accuracy" (fromMaybe 0 moveBoost.acc) newBoost.acc
      atkMsg = prettyBoostChange "Attack"   (fromMaybe 0 moveBoost.atk) newBoost.atk
      defMsg = prettyBoostChange "Defense"  (fromMaybe 0 moveBoost.def) newBoost.def
      evaMsg = prettyBoostChange "Evasion"  (fromMaybe 0 moveBoost.eva) newBoost.eva
      spaMsg = prettyBoostChange "Special Attack" (fromMaybe  0 moveBoost.spa) newBoost.spa
      spdMsg = prettyBoostChange "Special Defense" (fromMaybe 0 moveBoost.spd) newBoost.spd
      speMsg = prettyBoostChange "Speed"    (fromMaybe 0 moveBoost.spe) newBoost.spe
      messages = filter (not . T.null) [accMsg, atkMsg, defMsg, evaMsg, spaMsg, spdMsg, speMsg]
      combined = T.intercalate " " messages
  in T.concat [targetName, "'s ", combined]

-- | Print a list of log messages to the console.
printLogMessages :: [LogMessage] -> IO ()
printLogMessages logs = TIO.putStrLn (T.unlines (map prettyLogMessage logs))
