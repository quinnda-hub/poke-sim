{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Simulation.Internal where

import           Control.Monad.Trans.State           (get, put)
import           Control.Monad.Trans.Writer          (tell)
import qualified Data.Text                           as T

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (..),
                                                      TurnContext (..))

import           Control.Monad.Trans.Class           (MonadTrans (lift))
import           Pokemon.Battle.MoveEffect.Internal  (applySingleStep,
                                                      gatherEffects)
import           Pokemon.Battle.Pokemon.Internal     (MoveSet (..),
                                                      Pokemon (..), isFainted)
import           Pokemon.Data.Move.Internal          (Move (name))

-- | Simulates a simple 1v1 matchup.
simulateBattle :: BattleState -> BattleM ()
simulateBattle bs0 = do
  put bs0
  loop

  where
    loop :: BattleM ()
    loop = do
      bs <- get

      lift $ tell [ T.concat [ "=== Turn ", T.pack (show bs.turnNumber), " ==="] ]

      if isFainted bs.player1.active
        then lift $ tell ["Player 1's Pokemon is fainted! Player 2 wins!"]
        else if isFainted bs.player2.active
          then lift $ tell ["Player 2's Pokemon is fainted! Player 1 wins!"]
          else do
            let attSlot = bs.turnContext.attacker
            let attackerMove = chooseMove attSlot bs

            lift $ tell [ T.concat ["Attacker uses ", attackerMove.name, "!"] ]
            applyMove attackerMove

            bsAfter <- get
            let defSlot = bsAfter.turnContext.defender
                fainted = case defSlot of
                  Player1 -> isFainted bsAfter.player1.active
                  Player2 -> isFainted bsAfter.player2.active
            if fainted
              then do
                lift $ tell ["The defending Pokemon fainted! Battle over!"]
                pure ()
              else do
                let bsNext = incrementTurn (swapAttackerDefender bsAfter)
                put bsNext
                loop

-- | Apply the entire move (all effect steps) from the attacker to the defender.
applyMove :: Move -> BattleM ()
applyMove mv = do
    let steps = gatherEffects mv
    mapM_ (applySingleStep mv) steps

-- | Swaps the turnContext attacker/defender in a 1v1 scenario.
swapAttackerDefender :: BattleState -> BattleState
swapAttackerDefender bs =
    let oldAtt = bs.turnContext.attacker
        oldDef = bs.turnContext.defender
        newTC  = bs.turnContext{ attacker = oldDef, defender = oldAtt }
    in bs{ turnContext = newTC }

-- | Increments the turn counter by 1.
incrementTurn :: BattleState -> BattleState
incrementTurn bs =
  bs{ turnNumber = bs.turnNumber + 1}

-- | Simply choose the first move a Pokemon has.
chooseMove :: PlayerSlot -> BattleState -> Move
chooseMove Player1 bs =
  bs.player1.active.moves.move1
chooseMove Player2 bs =
  bs.player2.active.moves.move1
