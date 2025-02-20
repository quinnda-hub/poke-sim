{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Simulation.Internal (simulateBattle) where

import           Control.Monad.Trans.State           (get, put)
import           Control.Monad.Trans.Writer          (tell)
import qualified Data.Text                           as T

import           Pokemon.Battle.Action.Internal      (applyActions,
                                                      resolveActions)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..))

import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Class           (MonadTrans (lift))
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
      liftIO $ logTurn bs.logger bs.turnNumber

      if isFainted bs.player1.active
         then liftIO $ logVictory bs.logger "Player 2"
         else if isFainted bs.player2.active
           then  liftIO $ logVictory bs.logger "Player 1"
           else do
             let actions = resolveActions bs
             applyActions actions  -- Process actions one by one.

             bsAfter <- get
             if isFainted bsAfter.player1.active || isFainted bsAfter.player2.active
                then lift $ tell [ T.concat [ "Battle over!" ] ]
                else do
                  put $ incrementTurn bsAfter
                  loop
