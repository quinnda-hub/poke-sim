{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Simulation.Internal (simulateBattle) where

import           Control.Monad.Trans.State           (get, put)
import           Control.Monad.Trans.Writer          (tell)
import qualified Data.Text                           as T

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (..))

import           Control.Monad.Trans.Class           (MonadTrans (lift))
import           Data.List                           (sortBy)
import           Pokemon.Battle.MoveEffect.Internal  (applyMoveWithContext)
import           Pokemon.Battle.Pokemon.Internal     (MoveSet (..),
                                                      Pokemon (..), isFainted)
import           Pokemon.Data.Move.Internal          (Move (name), priority)
import Pokemon.Battle.Logging (logTurn)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Retrieves the active Pokémon's name for a given PlayerSlot.
actorName :: BattleState -> PlayerSlot -> T.Text
actorName bs Player1 = bs.player1.active.name
actorName bs Player2 = bs.player2.active.name

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
         then lift $ tell [ T.concat [ actorName bs Player1, " is fainted! ", actorName bs Player2, " wins!"] ]
         else if isFainted bs.player2.active
           then lift $ tell [ T.concat [ actorName bs Player2, " is fainted! ", actorName bs Player1, " wins!"] ]
           else do
             let actions = resolveActions bs
             applyActions actions  -- Process actions one by one.

             bsAfter <- get
             if isFainted bsAfter.player1.active || isFainted bsAfter.player2.active
                then lift $ tell [ T.concat [ "Battle over!" ] ]
                else do
                  put $ incrementTurn bsAfter
                  loop

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

-- | Represents a battle-level action. In this design each action
--   is associated with the PlayerSlot that is "owning" the action.
data Action = Action
  { actor      :: PlayerSlot  -- ^ The player from whose perspective the move is used.
  , actionType :: ActionType
  } deriving (Show)

-- Different types of actions. For now, only moves.
newtype ActionType = MoveAction Move deriving (Show)

-- | Resolves actions for the turn, associating each move with its originating PlayerSlot.
resolveActions :: BattleState -> [Action]
resolveActions bs =
  let act1 = Action { actor = Player1, actionType = MoveAction bs.player1.active.moves.move1 }
      act2 = Action { actor = Player2, actionType = MoveAction bs.player2.active.moves.move1 }
      actions = [act1, act2]
  in sortBy compareActions actions
  where
    compareActions :: Action -> Action -> Ordering
    compareActions (Action { actionType = MoveAction m1, actor = a1 })
                   (Action { actionType = MoveAction m2, actor = a2 }) =
      let p1 = m1.priority
          p2 = m2.priority
      in if p1 /= p2 then compare (negate p1) (negate p2)
         else
           let spd1 = case a1 of
                        Player1 -> bs.player1.active.spe
                        Player2 -> bs.player2.active.spe
               spd2 = case a2 of
                        Player1 -> bs.player1.active.spe
                        Player2 -> bs.player2.active.spe
           in compare (negate spd1) (negate spd2)

--------------------------------------------------------------------------------
-- Applying Actions
--------------------------------------------------------------------------------

applyActions :: [Action] -> BattleM ()
applyActions [] = return ()
applyActions (act:acts) = do
  applyAction act
  bs <- get
  if isFainted bs.player1.active || isFainted bs.player2.active
     then return ()  -- Stop processing further actions.
     else applyActions acts

-- | Applies a simulation-level action.
applyAction :: Action -> BattleM ()
applyAction (Action { actor = a, actionType = MoveAction mv }) =
  applyMoveActionWithActor a mv

-- | Applies a move action given the actor’s slot. This function sets up a local context:
--   the attacker is the given slot and the defender is the opponent.
applyMoveActionWithActor :: PlayerSlot -> Move -> BattleM ()
applyMoveActionWithActor actorSlot mv = do
  bs <- get
  let (attackerSlot, defenderSlot) = case actorSlot of
        Player1 -> (Player1, Player2)
        Player2 -> (Player2, Player1)
  lift $ tell [ T.concat ["[Action] ", mv.name, " used by ", actorName bs actorSlot] ]
  applyMoveWithContext mv attackerSlot defenderSlot
