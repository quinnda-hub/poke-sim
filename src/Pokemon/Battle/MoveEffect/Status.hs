{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Status where

import           Data.Text                           as T
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState,
                                                      Player (..),
                                                      PlayerSlot (..), player1,
                                                      player2)
import           Pokemon.Battle.Logging              (logStatus)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (InflictStatus),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (basePower), Status,
                                                      status)

-- | If the move applies status, produce an EffectStep.
gatherStatus :: Move -> Maybe EffectStep
gatherStatus mv =
  case basePower mv of
    Just _  -> Just InflictStatus
    Nothing -> Nothing

-- | Applies status to the specified player slot.
applyStatus :: PlayerSlot -> Status -> BattleState -> BattleState
applyStatus Player1 sts bs = bs { player1 = applyStatusToActive sts bs.player1 }
applyStatus Player2 sts bs = bs { player2 = applyStatusToActive sts bs.player2 }

-- | Updates a player's active Pokémon with new HP after damage.
applyStatusToActive :: Status -> Player -> Player
applyStatusToActive sts p = p { active = p.active { pstatus = Just sts } }

-- | Applies a status effect and update the battle stat.
applyStatusForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyStatusForMove mv attacker defender targetSlots =
  applyEffectForMove mv attacker defender targetSlots
    (\mv' _ _ _ _ -> pure (status mv'))
    (\mStatus slot bs ->
       case mStatus of
         Just st -> applyStatus slot st bs
         Nothing -> bs)
    (\mStatus slot bs ->
       case mStatus of
         Just st ->
           let targetName = case slot of
                              Player1 -> bs.player1.active.name
                              Player2 -> bs.player2.active.name
           in logStatus (T.pack $ show st) targetName
         Nothing -> pure ())  -- Skip logging if no status.
