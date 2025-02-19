{-# LANGUAGE OverloadedStrings #-}

module Pokemon.Battle.Logging where

import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Writer          (tell)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Pokemon.Battle.BattleState.Internal (BattleM)
import           Pokemon.Mechanics.Weather           (Weather)

-- | Log a damage event.
logDamage :: Int -> Text -> BattleM ()
logDamage dmg tgt = lift $ tell [ T.concat ["[Damage] ", T.pack (show dmg),   " damage dealt to ", tgt] ]

-- | Log an action event.
-- TODO when I factor Actions out of Simulation.hs
logAction :: Text -> Text -> BattleM ()
logAction mvName actor = undefined

-- | Log a turn event.
logTurn :: Int -> BattleM ()
logTurn n = lift $ tell [ T.concat [ "=== Turn ", T.pack (show n), " ===" ] ]

-- | Log a weather change event.
logWeather :: Weather -> BattleM ()
logWeather w = undefined
