{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Simulation.Internal (simulateBattle) where

import           Control.Monad.Trans.State           (get, put)
import           Control.Monad.Trans.Writer          (tell)
import qualified Data.Text                           as T

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (..),
                                                      TurnContext (..))

import           Control.Monad.Trans.Class           (MonadTrans (lift))
import           Data.List                           (sortBy)
import           Pokemon.Battle.MoveEffect.Internal  (applySingleStep,
                                                      gatherEffects)
import           Pokemon.Battle.Pokemon.Internal     (MoveSet (..),
                                                      Pokemon (..), isFainted)
import           Pokemon.Data.Move.Internal          (Move (name), priority)

-- | Simulates a simple 1v1 matchup.
simulateBattle :: BattleState -> BattleM ()
simulateBattle bs0 = do
  put bs0
  loop
  where
    loop :: BattleM ()
    loop = do
      bs <- get
      lift $ tell [ T.concat [ "=== Turn ", T.pack (show bs.turnNumber), " ===" ] ]

      if isFainted bs.player1.active
         then lift $ tell ["Player 1's Pokémon is fainted! Player 2 wins!"]
         else if isFainted bs.player2.active
           then lift $ tell ["Player 2's Pokémon is fainted! Player 1 wins!"]
           else do
             -- Resolve both actions for this turn.
             let actions = resolveActions bs
             mapM_ applyAction actions

             bsAfter <- get
             if isFainted (getDefenderPokemon bsAfter)
               then lift $ tell ["A defending Pokémon fainted! Battle over!"]
               else do
                 put $ incrementTurn bsAfter
                 loop

getDefenderPokemon :: BattleState -> Pokemon
getDefenderPokemon bs =
  case bs.turnContext.defender of
    Player1 -> bs.player1.active
    Player2-> bs.player2.active

data Action = Action
  { actor :: PlayerSlot -- ^ The player attached to the attacking Pokemon.
  , move  :: Move }     -- ^ The move chosen by the acting Pokemon.

-- | Chooses an action for a given Pokemon
--   (Always choose the first move for now.)
chooseAction :: PlayerSlot -> BattleState -> Action
chooseAction slot bs =
  let mv = case slot of
             Player1 -> bs.player1.active.moves.move1
             Player2 -> bs.player2.active.moves.move1
    in Action { actor = slot, move = mv }

-- | Orders the actions of both Pokemon based on move priority and speed.
resolveActions :: BattleState -> [Action]
resolveActions bs =
  let action1 = chooseAction bs.turnContext.attacker bs
      action2 = chooseAction bs.turnContext.defender bs
      actions = [action1, action2]
      in
        sortBy compareActions actions
        where
          compareActions :: Action -> Action -> Ordering
          compareActions (Action { move = m1, actor = a1 })
                         (Action { move = m2, actor = a2 }) =
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

-- | Applies a single action.
applyAction :: Action -> BattleM ()
applyAction act = do
  bs <- get
  -- Set the defender based on who is attacking.
  let newTC = case act.actor of
                Player1 -> bs.turnContext { defender = Player2 }
                Player2 -> bs.turnContext { defender = Player1 }
  put bs { turnContext = newTC }
  lift $ tell [ T.concat ["[Action] ", act.move.name, " used by ", showSlot act.actor ] ]
  applyMove act.move

-- | Helper: converts a PlayerSlot to displayable text.
showSlot :: PlayerSlot -> T.Text
showSlot Player1 = "Player 1"
showSlot Player2 = "Player 2"

-- | Apply the entire move (all effect steps) from the attacker to the defender.
applyMove :: Move -> BattleM ()
applyMove mv = do
    let steps = gatherEffects mv
    mapM_ (applySingleStep mv) steps

-- | Increments the turn counter by 1.
incrementTurn :: BattleState -> BattleState
incrementTurn bs =
  bs{ turnNumber = bs.turnNumber + 1}

