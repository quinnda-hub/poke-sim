{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Action.SwitchAction
  ( validSwitchActions
  , applySwitchActionWithActor
  ) where

import           Control.Monad.Trans.State           (get)

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..))

import           Control.Monad.State                 (modify)
import           Data.Maybe                          (catMaybes)
import qualified Data.Text                           as T
import           Pokemon.Battle.Action.Internal      (Action (..),
                                                      ActionType (..))
import           Pokemon.Battle.BattleState.Types    (Player (..),
                                                      PlayerSlot (..),
                                                      Team (..))
import           Pokemon.Battle.Logging              (logSwitch)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..),
                                                      defaultBoosts, isFainted)

-- | Returns valid switch actions for the given player slot.
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

-- | Swaps the Pokémon in the team.
swapPokemonInTeam :: Pokemon -> Pokemon -> Team -> Team
swapPokemonInTeam newPoke oldActive t = t
  { pokemon1 = if pokemon1 t == Just newPoke then Just oldActive else pokemon1 t
  , pokemon2 = if pokemon2 t == Just newPoke then Just oldActive else pokemon2 t
  , pokemon3 = if pokemon3 t == Just newPoke then Just oldActive else pokemon3 t
  , pokemon4 = if pokemon4 t == Just newPoke then Just oldActive else pokemon4 t
  , pokemon5 = if pokemon5 t == Just newPoke then Just oldActive else pokemon5 t
  }

-- | Applies a switch action by swapping the active Pokémon with the chosen one.
--   If any buggy behaviour happens with switches, we might need to be more careful
--   with this function. It should never be called without validSwitchActions being
--   called before it, but you never know.
applySwitchActionWithActor :: PlayerSlot -> Pokemon -> BattleM ()
applySwitchActionWithActor actorSlot newPoke = do
  bs <- get
  let currentPlayer = case actorSlot of
                        Player1 -> bs.player1
                        Player2 -> bs.player2
      currentActive = currentPlayer.active
      resetActive   = currentActive { boosts = defaultBoosts }
      newTeam       = swapPokemonInTeam newPoke resetActive currentPlayer.team
      newPlayer     = currentPlayer { active = newPoke, team = newTeam }
  logSwitch (T.pack $ show actorSlot) currentActive.name newPoke.name
  modify $ \bs' -> case actorSlot of
                    Player1 -> bs' { player1 = newPlayer }
                    Player2 -> bs' { player2 = newPlayer }

