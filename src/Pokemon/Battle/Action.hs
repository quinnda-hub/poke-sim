{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Action where

import           Control.Monad.Trans.State           (get)

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (..))

import           Data.List                           (sortBy)
import           Pokemon.Battle.Action.Internal      (Action (..),
                                                      ActionType (..),
                                                      uniformStrategy)
import           Pokemon.Battle.Action.MoveAction    (applyMoveActionWithActor,
                                                      validMoveActions)
import           Pokemon.Battle.Action.SwitchAction  (applySwitchActionWithActor,
                                                      validSwitchActions)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..), isFainted)
import           Pokemon.Data.Move.Internal          (priority)

--------------------------------------------------------------------------------
-- Applying Actions
--------------------------------------------------------------------------------

-- Apply all the actions to the BattleState.
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
applyAction (Action {actor = a, actionType = SwitchAction pk }) =
  applySwitchActionWithActor a pk

-- | Resolves actions for the turn by having each player choose an action using the uniform strategy.
resolveActions :: BattleM [Action]
resolveActions = do
  act1 <- chooseAction Player1
  act2 <- chooseAction Player2
  bs   <- get
  let actions = [act1, act2]
  return (sortBy (compareActions bs) actions)
  where
    compareActions :: BattleState -> Action -> Action -> Ordering
    compareActions bs (Action { actionType = MoveAction m1, actor = a1 })
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
    compareActions _ _ _ = EQ

validActions :: PlayerSlot -> BattleM [Action]
validActions p = (++) <$> validMoveActions p <*> validSwitchActions p

chooseAction :: PlayerSlot -> BattleM Action
chooseAction p = do
  actions <- validActions p
  if null actions
    then error "No valid options available!" -- Just a place holder until we hardcode Struggle in.
    else uniformStrategy actions

