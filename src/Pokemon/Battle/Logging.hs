{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Pokemon.Battle.Logging
  ( LoggerMode(..)
  , Logger(..)
  , createLogger
  , logDamage
  , logAction
  , logTurn
  , logWeather
  , logVictory
  , LogMessage(..)
  ) where

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Pokemon.Mechanics.Weather (Weather)

-- | The different logging modes we might use.
data LoggerMode = Console | File FilePath
  deriving (Show, Eq)

-- | Our structured log message.
data LogMessage
  = DamageLog  { damage :: Int, target :: Text }
  | ActionLog  { moveName :: Text, actor :: Text }
  | TurnLog    { turnNumber :: Int }
  | WeatherLog { weather :: Weather }
  | VictoryLog {victor :: Text}

instance Show LogMessage where
  show (DamageLog dmg tgt) =
    "[Damage] " ++ show dmg ++ " damage dealt to " ++ T.unpack tgt
  show (ActionLog mv act) =
    "[Action] " ++ T.unpack mv ++ " used by " ++ T.unpack act
  show (TurnLog n) =
    "=== Turn " ++ show n ++ " ==="
  show (WeatherLog w) =
    "[Weather] Weather changed to " ++ show w
  show (VictoryLog v) =
    "[Victory] Battle over! " ++ T.unpack v ++ " wins!"

-- | Logger holds the logging mode and a function to write logs.
data Logger = Logger
  { loggerMode :: LoggerMode
  , logFunc    :: LogMessage -> IO ()
  }

instance Show Logger where
  show :: Logger -> String
  show (Logger mode _) = "Logger { loggerMode = " ++ show mode ++ ", logFunc = <function> }"

-- | Create a logger for a given mode.
createLogger :: LoggerMode -> IO Logger
createLogger mode =
  case mode of
    Console   -> return $ Logger mode print
    File path -> return $ Logger mode (\msg -> appendFile path (show msg ++ "\n"))

-- | Log a damage event.
logDamage :: Logger -> Int -> Text -> IO ()
logDamage logger dmg tgt = do
  logFunc logger (DamageLog dmg tgt)

-- | Log an action event.
logAction :: Logger -> Text -> Text -> IO ()
logAction logger mvName act = do
  logFunc logger (ActionLog mvName act)

-- | Log a turn event.
logTurn :: Logger -> Int -> IO ()
logTurn logger n = do
  logFunc logger (TurnLog n)

-- | Log a weather change event.
logWeather :: Logger -> Weather -> IO ()
logWeather logger w = do
  logFunc logger (WeatherLog w)

-- | Log winner of a battle. 
logVictory :: Logger -> Text -> IO ()
logVictory logger v = do 
  logFunc logger (VictoryLog v)
