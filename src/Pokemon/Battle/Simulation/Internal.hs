{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Simulation.Internal (simulateBattle) where

import           Control.Monad.Trans.State           (get, put)

import           Pokemon.Battle.Action               (applyActions,
                                                      resolveActions)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..), Team (..))

import           Pokemon.Battle.Logging              (logTurn, logVictory)
import           Pokemon.Battle.Pokemon.Internal     (isFainted)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

teamFainted :: Team -> Bool
teamFainted t =
  maybe True isFainted t.pokemon1 &&
  maybe True isFainted t.pokemon2 &&
  maybe True isFainted t.pokemon3 &&
  maybe True isFainted t.pokemon4 &&
  maybe True isFainted t.pokemon5

-- | A player is defeated if their active Pokémon is fainted
--   and their entire team (bench) is fainted.
isPlayerDefeated :: Player -> Bool
isPlayerDefeated player = isFainted player.active && teamFainted player.team

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

      if isPlayerDefeated bs.player1
         then logVictory "Player 2"
         else if isPlayerDefeated bs.player2
           then  logVictory "Player 1"
           else do
             actions <- resolveActions
             applyActions actions  -- Process actions one by one.

             bsAfter <- get
             if isPlayerDefeated bsAfter.player1 || isPlayerDefeated bsAfter.player2
                then if isPlayerDefeated bsAfter.player1
                        then logVictory "Player 2"
                        else logVictory "Player 1"
                else do
                  put $ incrementTurn bsAfter
                  loop
