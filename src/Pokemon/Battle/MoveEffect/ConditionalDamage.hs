{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.MoveEffect.ConditionalDamage where

import           Pokemon.Battle.BattleState.Types    (Player (..),
                                                      PlayerSlot (..),
                                                      TurnContext (..))
import           Pokemon.Battle.Logging              (logDamage, logMoveFail)
import           Pokemon.Battle.MoveEffect.Internal  (Condition (..),
                                                      EffectStep (ConditionalDamage),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (category, name),
                                                      MoveCategory (..))

import           Control.Monad.State.Class           (get)
import           Pokemon.Battle.Action.Internal      (Action (..),
                                                      ActionType (..))
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..))
import           Pokemon.Battle.Damage.Internal      (calculateDamage)
import           Pokemon.Battle.MoveEffect.Damage    (applyDamageAndUpdateLast)

gatherConditionalDamage :: Move -> Maybe EffectStep
gatherConditionalDamage mv
  | mv.name == "Sucker Punch" = Just $ ConditionalDamage IsTargetAttacking
  | mv.name == "Thunderclap"  = Just $ ConditionalDamage IsTargetAttacking
  | otherwise                 = Nothing

-- | Map a conditional type to an actual predicate.
getPredicate :: Condition -> PlayerSlot -> (BattleState -> Bool)
getPredicate IsTargetAttacking = isPlayerAttacking

-- | Checks if the given player's chosen move qualifies as an attacking move.
isPlayerAttacking :: PlayerSlot -> BattleState -> Bool
isPlayerAttacking slot bs =
  let action = case slot of
                 Player1 -> bs.turnContext.player1Action
                 Player2 -> bs.turnContext.player2Action
      isAttackingMove :: Move -> Bool
      isAttackingMove m = case m.category of
                             Status -> False
                             _      -> True
  in case action of
       Just (Action { actionType = MoveAction m }) -> isAttackingMove m
       _                                           -> False

applyConditionalDamageForMove
  :: Move                  -- ^ The move being used.
  -> Pokemon               -- ^ The attacker.
  -> Pokemon               -- ^ The defender.
  -> [PlayerSlot]          -- ^ Target slots.
  -> Condition
  -> BattleM ()
applyConditionalDamageForMove mv attackerPokemon defenderPokemon targetSlots condition = do
  bs <- get
  if getPredicate condition bs.turnContext.defender bs
    then applyEffectForMove mv attackerPokemon defenderPokemon targetSlots
           (\mv' attackerPokemon' defenderPokemon' _ _ -> calculateDamage mv' attackerPokemon' defenderPokemon')
           (flip applyDamageAndUpdateLast)
           (\dmg slot bs' ->
             let targetName = case slot of
                                Player1 -> bs'.player1.active.name
                                Player2 -> bs'.player2.active.name
             in logDamage dmg targetName)
    else do
      logMoveFail
      return ()

