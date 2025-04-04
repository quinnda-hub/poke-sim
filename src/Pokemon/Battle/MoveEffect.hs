{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect
  ( gatherEffects
  , applySingleStep
  , applyMoveWithContext
  ) where

import           Control.Monad.State.Class              (get, modify)
import           Data.Maybe                             (catMaybes)
import           Pokemon.Battle.BattleState.Internal    (BattleM,
                                                         BattleState (..))
import           Pokemon.Battle.BattleState.Types       (PlayerSlot,
                                                         TurnContext (attacker, defender, moveModifiers))
import           Pokemon.Battle.Logging                 (logMiss)
import           Pokemon.Battle.MoveEffect.Boosts       (applyBoostsForMove,
                                                         gatherBoosts)
import           Pokemon.Battle.MoveEffect.Damage       (applyDamageForMove,
                                                         gatherDamage)
import           Pokemon.Battle.MoveEffect.Internal     (EffectStep (..),
                                                         checkAccuracy,
                                                         getAttackerAndDefender,
                                                         resolveTargets)
import           Pokemon.Battle.MoveEffect.RecoilDamage (applyRecoilForMove,
                                                         gatherRecoil)
import           Pokemon.Battle.MoveEffect.Status       (applyStatusForMove,
                                                         gatherStatus)
import           Pokemon.Battle.Pokemon.Internal        (Pokemon (name))
import           Pokemon.Data.Move.Internal             (Move (..), target)

--------------------------------------------------------------------------------
-- Effect Gathering
--------------------------------------------------------------------------------
-- | Collects all effect steps from a move.
gatherEffects :: Move -> [EffectStep]
gatherEffects mv =
  catMaybes [ gatherDamage mv
            , gatherRecoil mv
            , gatherStatus mv
            , gatherBoosts mv
            ]

--------------------------------------------------------------------------------
-- Effect Context Helper
--------------------------------------------------------------------------------
-- | Helper for retrieving the current battle context relevant to move execution.
getEffectContext :: Move -> BattleM (Pokemon, Pokemon, [PlayerSlot])
getEffectContext mv = do
  bs <- get
  let (attackerP, defenderP) = getAttackerAndDefender bs
      ts       = mv.target
      targets  = resolveTargets ts bs.turnContext
  pure (attackerP, defenderP, targets)

--------------------------------------------------------------------------------
-- Applying Single Effect Steps
--------------------------------------------------------------------------------
-- | Applies a single effect step using the current move context.
applySingleStep :: Move -> EffectStep -> BattleM ()
applySingleStep mv DoDamage = do
  (attackerP, defenderP, targets) <- getEffectContext mv
  applyDamageForMove mv attackerP defenderP targets

applySingleStep mv RecoilDamage = do
  (attackerP, defenderP, targets) <- getEffectContext mv
  applyRecoilForMove mv attackerP defenderP targets

applySingleStep mv InflictStatus = do
  (attackerP, defenderP, targets) <- getEffectContext mv
  applyStatusForMove mv attackerP defenderP targets

applySingleStep mv BoostStats = do
  (attackerP, defenderP, targets) <- getEffectContext mv
  applyBoostsForMove mv attackerP defenderP targets

applySingleStep _ (SetWeather w) =
  modify $ \bs -> bs { weather = w }

--------------------------------------------------------------------------------
-- Applying a Move Action
--------------------------------------------------------------------------------
-- | Applies all effect steps associated with a move.
applyMoveAction :: Move -> BattleM ()
applyMoveAction mv = do
  bs <- get
  let (attackerP, defenderP) = getAttackerAndDefender bs
  hit <- case mv.accuracy of
           Nothing   -> pure True
           Just acc' -> checkAccuracy acc' attackerP defenderP
  if hit
    then mapM_ (applySingleStep mv) (gatherEffects mv)
    else logMiss attackerP.name

--------------------------------------------------------------------------------
-- Applying a Move with Context
--------------------------------------------------------------------------------
-- | Applies a move given an explicit attackerP and defenderP.
applyMoveWithContext :: Move -> PlayerSlot -> PlayerSlot -> BattleM ()
applyMoveWithContext mv attackerPSlot defenderSlot = do
  -- Update the turn context with the explicit attackerP and defenderP.
  modify $ \bs ->
    bs { turnContext = (turnContext bs)
                     { attacker = attackerPSlot
                     , defender = defenderSlot
                     }
       }
  applyMoveAction mv
  -- Clear move modifiers immediately after processing this move.
  modify $ \bs' ->
    bs' { turnContext = (turnContext bs') { moveModifiers = [] } }

