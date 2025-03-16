{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Status where

import           Control.Monad.State                 (get)
import           Data.Maybe                          (isJust, isNothing)
import           Data.Text                           as T (pack)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      getPokemonForSlot)

import           Control.Monad                       (when)
import           Pokemon.Battle.BattleState.Types    (Player (..),
                                                      PlayerSlot (..))
import           Pokemon.Battle.Logging              (logStatus)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (InflictStatus),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move, Status, moveType,
                                                      status)
import           Pokemon.Mechanics.PType             (PType (..))

-- | If the move applies status, produce an EffectStep.
gatherStatus :: Move -> Maybe EffectStep
gatherStatus mv =
  case status mv of
    Just _  -> Just InflictStatus
    Nothing -> Nothing

-- | Applies status to the specified player slot.
applyStatus :: PlayerSlot -> Status -> BattleState -> BattleState
applyStatus Player1 sts bs = bs { player1 = applyStatusToActive sts bs.player1 }
applyStatus Player2 sts bs = bs { player2 = applyStatusToActive sts bs.player2 }

-- | Updates a player's active Pokémon with new HP after damage.
applyStatusToActive :: Status -> Player BattleM -> Player BattleM
applyStatusToActive sts p =
  case p.active.pstatus of
    Just _  -> p
    Nothing -> p { active = p.active { pstatus = Just sts } }

-- | Checks if a Pokémon is immune to a status move based on its types.
isImmuneToStatus :: Pokemon -> Move -> Bool
isImmuneToStatus p mv =
  let mType = mv.moveType
      primaryType   = p.type1
      secondaryType = p.type2
      hasType t = primaryType == t || Just t == secondaryType
  in case mType of
       Fire     -> hasType Fire     -- Fire-type moves do not affect Fire-type Pokémon.
       Electric -> hasType Ground   -- Electric moves don't affect Ground-type Pokémon.
       Poison   -> hasType Steel    -- Poison moves don't affect Steel-type Pokémon.
       _        -> False

applyStatusForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyStatusForMove mv attacker defender targetSlots = do
  bsPre <- get
  let getPreStatus slot = case slot of
        Player1 -> bsPre.player1.active.pstatus
        Player2 -> bsPre.player2.active.pstatus
      logIfNeeded st slot bs =
        let targetPokemon = getPokemonForSlot slot bs
        in when (isNothing targetPokemon.pstatus && not (isImmuneToStatus targetPokemon mv)) $ do
             let targetName = case slot of
                   Player1 -> bs.player1.active.name
                   Player2 -> bs.player2.active.name
             logStatus (T.pack $ show st) targetName

  applyEffectForMove mv attacker defender targetSlots
    (\_ _ _ _ _ -> pure (status mv))
    (\mStatus slot bs ->
       case mStatus of
         Just st ->
           let targetPokemon = getPokemonForSlot slot bs
               preStatus     = getPreStatus slot
           in if isJust preStatus || isImmuneToStatus targetPokemon mv
                then bs
                else applyStatus slot st bs
         Nothing -> bs)
    (\mStatus slot bs ->
       case mStatus of
         Just st -> logIfNeeded st slot bs
         Nothing -> pure ())

