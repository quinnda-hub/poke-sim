{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Simulation.Internal (simulateBattle) where

import           Control.Monad.Trans.State           (get, put)

import           Pokemon.Battle.Action.Internal      (applyActions,
                                                      resolveActions)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..))

import           Pokemon.Battle.Logging              (logTurn, logVictory)
import           Pokemon.Battle.Pokemon.Internal     (isFainted)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Increments the turn counter by 1.
incrementTurn :: BattleState -> BattleState
incrementTurn bs =
  bs{ turnNumber = bs.turnNumber + 1}

--------------------------------------------------------------------------------
-- Battle Loop
--------------------------------------------------------------------------------

-- | Simulates a simple 1v1 matchup.
simulateBattle :: BattleState -> BattleM ()
simulateBattle bs0 = do
  put bs0
  loop
  where
    loop :: BattleM ()
    loop = do
      bs <- get
      logTurn bs.turnNumber

      if isFainted bs.player1.active
         then logVictory "Player 2"
         else if isFainted bs.player2.active
           then  logVictory "Player 1"
           else do
             let actions = resolveActions bs
             applyActions actions  -- Process actions one by one.

             bsAfter <- get
             if isFainted bsAfter.player1.active || isFainted bsAfter.player2.active
                then logVictory "Battle Over!"
                else do
                  put $ incrementTurn bsAfter
                  loop
