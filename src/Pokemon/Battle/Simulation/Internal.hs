{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Simulation.Internal (simulateBattle) where

import           Control.Monad.State.Class           (get, put)

import           Pokemon.Battle.Action               (applyAction, applyActions,
                                                      resolveActions)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..))

import           Pokemon.Battle.BattleState.Types    (Player (active, team),
                                                      PlayerSlot (..),
                                                      Team (pokemon1, pokemon2, pokemon3, pokemon4, pokemon5))

import           Control.Monad                       (unless, when)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.RWS                   (MonadWriter (..))
import           Pokemon.AI.Internal                 (uniformStrategy)
import           Pokemon.Battle.Action.SwitchAction  (validSwitchActions)
import           Pokemon.Battle.Logging              (logTurn, logVictory)
import           Pokemon.Battle.Pokemon.Internal     (isFainted)
import           Pokemon.Pretty                      (printLogMessages)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

switchAfterFaint :: PlayerSlot -> BattleM ()
switchAfterFaint  slot = do
  bs <- get
  let currentActive = case slot of
                        Player1 -> bs.player1.active
                        Player2 -> bs.player2.active
  when (isFainted currentActive) $ do
    switchActions <- validSwitchActions slot
    if null switchActions
      then return () -- No valid switch actions; player remains with fainted Pokemon.
      else do
        chosenSwitch <- uniformStrategy switchActions
        applyAction chosenSwitch

teamFainted :: Team -> Bool
teamFainted t =
  maybe True isFainted t.pokemon1 &&
  maybe True isFainted t.pokemon2 &&
  maybe True isFainted t.pokemon3 &&
  maybe True isFainted t.pokemon4 &&
  maybe True isFainted t.pokemon5

-- | A player is defeated if their active Pokémon is fainted
--   and their entire team (bench) is fainted.
isPlayerDefeated :: Player BattleM -> Bool
isPlayerDefeated player = isFainted player.active && teamFainted player.team

-- | Increments the turn counter by 1.
incrementTurn :: BattleState -> BattleState
incrementTurn bs =
  bs{ turnNumber = bs.turnNumber + 1 }

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
      -- Run one turn and capture the log messages produced during that turn.
      ((), turnLogs) <- listen simulateTurn
      liftIO $ printLogMessages turnLogs
      bs <- get
      unless (isPlayerDefeated bs.player1 || isPlayerDefeated bs.player2) loop

-- | Simulates one turn of a battle.
simulateTurn :: BattleM ()
simulateTurn = do
  bs <- get
  logTurn bs.turnNumber

  if isPlayerDefeated bs.player1
    then logVictory "Player 2"
    else do
      actions <- resolveActions
      applyActions actions

      switchAfterFaint Player1
      switchAfterFaint Player2

      bsAfter <- get
      if isPlayerDefeated bsAfter.player1 || isPlayerDefeated bsAfter.player2
        then if isPlayerDefeated bsAfter.player1
          then logVictory "Player 2"
          else logVictory "Player 1"
        else do
          put $ incrementTurn bsAfter
          return ()

