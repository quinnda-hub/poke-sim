{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.MoveEffect.DelayedMove where

import           Pokemon.Battle.BattleState.Types    (DelayedMove (..),
                                                      Player (..),
                                                      PlayerSlot (..),
                                                      lastDamage)
import           Pokemon.Battle.Logging              (logDamage)
import qualified Pokemon.Battle.MoveEffect.Internal  as ME
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (basePower, name),
                                                      multihit)

import           Control.Monad.State.Class           (get, modify)
import           Data.Maybe                          (isJust)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      pokemonName)
import           Pokemon.Battle.Damage.Internal      (calculateDamage)

-- | If the move is Future Sight or Doom Desire, produce a delayed move effect step.
gatherDelayedMove :: Move -> Maybe ME.EffectStep
gatherDelayedMove mv
  | mv.name `elem` ["Future Sight", "Doom Desire"] = Just ME.DelayedMove
  | otherwise = Nothing

-- | Queue a delayed move in the BattleState.
--   This function captures a snapshot of the user and pushes a new DelayedMove
--   with a fixed delay onto the state's delayedMoves field.
queueDelayedMove :: Move -> Pokemon -> PlayerSlot -> BattleM ()
queueDelayedMove mv attacker target = do
  bs <- get
  let delay = 2
      uId   = case target of
                Player1 -> (Player2, pokemonName bs Player2)
                Player2 -> (Player1, pokemonName bs Player1)
      dm = DelayedMove { delayedMove    = mv
                       , userId         = uId
                       , userSnapshot   = attacker
                       , targetSlot     = target
                       , turnsRemaining = delay
                       }
  modify (\bs' -> bs' { delayedMoves = dm : bs'.delayedMoves })





