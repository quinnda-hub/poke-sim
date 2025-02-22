{-# LANGUAGE FlexibleContexts #-}

module Pokemon.Battle.Logging
  ( logDamage
  , logAction
  , logTurn
  , logWeather
  , logVictory
  , LogMessage(..)
  ) where

import           Control.Monad.Writer      (MonadWriter, tell)
import           Data.Text                 (Text)
import           Pokemon.Mechanics.Weather (Weather)

data LogMessage
  = DamageLog  { damage :: Int, target :: Text }
  | ActionLog  { moveName :: Text, actor :: Text }
  | TurnLog    { turnNumber :: Int }
  | WeatherLog { weather :: Weather }
  | VictoryLog {victor :: Text} deriving (Show, Eq)

-- | Log a damage event.
logDamage :: MonadWriter [LogMessage] m => Int -> Text -> m ()
logDamage dmg tgt = tell [DamageLog dmg tgt]

-- | Log an action event.
logAction :: MonadWriter [LogMessage] m => Text -> Text -> m ()
logAction mvName act = tell [ActionLog mvName act]

-- | Log a turn event.
logTurn :: MonadWriter [LogMessage] m => Int -> m ()
logTurn n = tell [TurnLog n]

-- | Log a weather change event.
logWeather :: MonadWriter [LogMessage] m => Weather -> m ()
logWeather w = tell [WeatherLog w]

-- | Log the winner of a battle.
logVictory :: MonadWriter [LogMessage] m => Text -> m ()
logVictory v = tell [VictoryLog v]
