{-# LANGUAGE FlexibleContexts #-}

module Pokemon.Battle.Logging (
  logDamage,
  logAction,
  logTurn,
  logWeather,
  logVictory,
  logMiss,
  logCrit,
  logStatus,
  logBoosts,
  LogMessage (..),
) where

import           Control.Monad.Writer            (MonadWriter, tell)
import           Data.Text                       (Text)
import           Pokemon.Battle.Pokemon.Internal (PokemonBoosts)
import           Pokemon.Data.Move.Internal      (Boosts)
import           Pokemon.Mechanics.Weather       (Weather)

data LogMessage
  = DamageLog {damage :: Int, target :: Text}
  | ActionLog {moveName :: Text, actor :: Text}
  | TurnLog {turnNumber :: Int}
  | WeatherLog {weather :: Weather}
  | VictoryLog {victor :: Text}
  | MissLog {whiffer :: Text}
  | CritLog {critter :: Text}
  | StatusLog {actor :: Text, status :: Text}
  | BoostsLog {actor :: Text, boosts :: Boosts, pokeBoosts :: PokemonBoosts}
  deriving (Show, Eq)

-- | Log a damage event.
logDamage :: (MonadWriter [LogMessage] m) => Int -> Text -> m ()
logDamage dmg tgt = tell [DamageLog dmg tgt]

-- | Log an action event.
logAction :: (MonadWriter [LogMessage] m) => Text -> Text -> m ()
logAction mvName act = tell [ActionLog mvName act]

-- | Log a turn event.
logTurn :: (MonadWriter [LogMessage] m) => Int -> m ()
logTurn n = tell [TurnLog n]

-- | Log a weather change event.
logWeather :: (MonadWriter [LogMessage] m) => Weather -> m ()
logWeather w = tell [WeatherLog w]

-- | Log the winner of a battle.
logVictory :: (MonadWriter [LogMessage] m) => Text -> m ()
logVictory v = tell [VictoryLog v]

-- | Log a Pokemon missing an attack.
logMiss :: (MonadWriter [LogMessage] m) => Text -> m ()
logMiss w = tell [MissLog w]

-- | Log a critical hit.
logCrit :: (MonadWriter [LogMessage] m) => Text -> m ()
logCrit w = tell [CritLog w]

-- | Log status being applied.
logStatus :: (MonadWriter [LogMessage] m) => Text -> Text -> m ()
logStatus act sts = tell [StatusLog act sts]

-- | Log boosts being applied.
logBoosts :: (MonadWriter [LogMessage] m) => Text -> Boosts -> PokemonBoosts -> m ()
logBoosts act mBoosts pBoosts = tell [BoostsLog act mBoosts pBoosts]

