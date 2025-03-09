{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Action.Internal (actorName, applyActions, resolveActions) where

import           Control.Monad.Trans.State           (get)
import qualified Data.Text                           as T

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (..),
                                                      Team (..),
                                                      getPokemonForSlot, getRNG,
                                                      updateRNG)

import           Data.List                           (sortBy)
import           Data.Maybe                          (catMaybes)
import           Pokemon.Battle.Ability              (triggerAbilityOnMoveUse)
import           Pokemon.Battle.Logging              (logAction)
import           Pokemon.Battle.MoveEffect           (applyMoveWithContext)
import           Pokemon.Battle.Pokemon.Internal     (MoveSet (..),
                                                      Pokemon (..), isFainted)
import           Pokemon.Data.Move.Internal          (Move (name), priority)
import           System.Random                       (randomR)

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

-- | Represents a battle-level action. In this design each action
--   is associated with the PlayerSlot that is "owning" the action.
data Action = Action
  { actor      :: PlayerSlot  -- ^ The player from whose perspective the move is used.
  , actionType :: ActionType
  } deriving (Show, Eq)

-- Different types of actions. For now, only moves.
data ActionType = MoveAction Move
                | SwitchAction Pokemon deriving (Show, Eq)

validMoveActions :: PlayerSlot -> BattleM [Action]
validMoveActions p = do
  bs <- get
  let activeMoves   = moves (getPokemonForSlot p bs)
      availableMoves = catMaybes [ Just activeMoves.move1
                                 , activeMoves.move2
                                 , activeMoves.move3
                                 , activeMoves.move4
                                 ]
  return [ Action { actor = p, actionType = MoveAction m } | m <- availableMoves ]

validSwitchActions :: PlayerSlot -> BattleM [Action]
validSwitchActions p = do
  bs <- get
  let t = case p of
            Player1 -> bs.player1.team
            Player2 -> bs.player2.team
  return [ Action { actor = p, actionType = SwitchAction pk }
         | pk <- filter (not . isFainted) (catMaybes [ t.pokemon1
                                                     , t.pokemon2
                                                     , t.pokemon3
                                                     , t.pokemon4
                                                     , t.pokemon5]) ]

validActions :: PlayerSlot -> BattleM [Action]
validActions p = (++) <$> validMoveActions p <*> validSwitchActions p

uniformStrategy :: [Action] -> BattleM Action
uniformStrategy actions = do
  rng0 <- getRNG
  let (idx, newRng) = randomR (0, length actions - 1) rng0
  updateRNG newRng
  return (actions !! idx)

chooseAction :: PlayerSlot -> BattleM Action
chooseAction p = do
  actions <- validActions p
  if null actions
    then error "No valid options available!" -- Just a place holder until we hardcode Struggle in.
    else uniformStrategy actions

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
  logAction mv.name (actorName bs attackerSlot)
  triggerAbilityOnMoveUse attackerSlot mv
  applyMoveWithContext mv attackerSlot defenderSlot

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Retrieves the active Pokémon's name for a given PlayerSlot.
actorName :: BattleState -> PlayerSlot -> T.Text
actorName bs Player1 = bs.player1.active.name
actorName bs Player2 = bs.player2.active.name
