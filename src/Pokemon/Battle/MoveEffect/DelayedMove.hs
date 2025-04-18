{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.MoveEffect.DelayedMove where

import           Pokemon.Battle.BattleState.Types    (DelayedMove (..),
                                                      Player (..),
                                                      PlayerSlot (..),
                                                      PokemonID)
import           Pokemon.Battle.Logging              (logDamage, logMoveFail)
import qualified Pokemon.Battle.MoveEffect.Internal  as ME
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..), isFainted)
import           Pokemon.Data.Move.Internal          (Move (name))

import           Control.Monad.State.Class           (get, modify)
import           Data.List                           (partition)
import           Data.Maybe                          (fromMaybe)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      getPokemonForSlot,
                                                      pokemonName)
import           Pokemon.Battle.Damage.Internal      (calculateDamage)
import           Pokemon.Battle.MoveEffect.Damage    (applyDamageAndUpdateLast)

-- | If the move is Future Sight or Doom Desire, produce a delayed move effect step.
gatherDelayedMove :: Move -> Maybe ME.EffectStep
gatherDelayedMove mv
  | mv.name `elem` ["Future Sight", "Doom Desire"] = Just ME.DelayedMove
  | otherwise = Nothing

-- Applies a delayed move by enqueuing it.
applyDelayedMove :: Move -> Pokemon -> Pokemon -> PlayerSlot -> BattleM ()
applyDelayedMove mv attackerPokemon _ = queueDelayedMove mv attackerPokemon

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

-- | When a move's counter reaches 0, calculate and apply its damage.
triggerDelayedMove :: DelayedMove -> BattleM ()
triggerDelayedMove dm = do
  bs <- get
  -- Retrieve the target from the queued slot.
  let mTarget = getPokemonForSlot dm.targetSlot bs
  if isFainted mTarget
    then
    logMoveFail
    else do
      let mUser    = lookupActivePokemonById bs dm.userId
          attacker = fromMaybe dm.userSnapshot mUser
      dmg <- calculateDamage dm.delayedMove attacker mTarget
      logDamage dmg mTarget.name
      modify (applyDamageAndUpdateLast dm.targetSlot dmg)

-- | Processes all delayed moves in the BattleState: decrement their counters
--   and trigger those that are ready.
processDelayedMoves ::BattleM ()
processDelayedMoves = do
  modify (\bs -> bs { delayedMoves = map (\dm -> dm { turnsRemaining = dm.turnsRemaining - 1 }) (delayedMoves bs) })
  bs <- get
  let (ready, remaining) = partition (\dm -> dm.turnsRemaining <= 0) bs.delayedMoves
  mapM_ triggerDelayedMove ready
  modify (\bs' -> bs' { delayedMoves = remaining })

-- | Looks up the active Pokemon according to a PokemonID, which in this case is simply a
--   tuple of (PlayerSlot, PokemonName (=Text)).
lookupActivePokemonById :: BattleState -> PokemonID -> Maybe Pokemon
lookupActivePokemonById bs (pslot, pname) =
  case pslot of
    Player1 ->
      let p = bs.player1.active
      in if p.name == pname && not (isFainted p)
        then Just p
        else Nothing
    Player2 ->
          let p = bs.player2.active
          in if p.name == pname && not (isFainted p)
            then Just p
            else Nothing


