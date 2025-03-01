{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect
  ( gatherEffects
  , applySingleStep
  , applyMoveWithContext
  ) where


import           Control.Monad.Trans.State           (get, modify)
import           Data.Maybe                          (catMaybes)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      PlayerSlot,
                                                      TurnContext (..))
import           Pokemon.Battle.Logging              (logMiss)
import           Pokemon.Battle.MoveEffect.Damage    (applyDamageForMove,
                                                      gatherDamage)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (..),
                                                      checkAccuracy,
                                                      getAttackerAndDefender,
                                                      resolveTargets)
import           Pokemon.Battle.MoveEffect.Status    (applyStatusForMove,
                                                      gatherStatus)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (name))
import           Pokemon.Data.Move.Internal          (Move (..), target)

-- | Collects all effect steps from a move.
gatherEffects :: Move -> [EffectStep]
gatherEffects mv =
  catMaybes [ gatherDamage mv
            , gatherStatus mv ]

-- | Applies a single effect step using the current move context.
applySingleStep :: Move -> EffectStep -> BattleM ()
applySingleStep mv DoDamage = do
  bs <- get
  let (attackerPokemon, defenderPokemon) = getAttackerAndDefender bs 
      ts      = mv.target
      targets = resolveTargets ts bs.turnContext
  applyDamageForMove mv attackerPokemon defenderPokemon targets

applySingleStep mv InflictStatus = do
  bs <- get 
  let (attackerPokemon, defenderPokemon) = getAttackerAndDefender bs 
      ts = mv.target
      targets = resolveTargets ts bs.turnContext
  applyStatusForMove mv attackerPokemon defenderPokemon targets

applySingleStep _ (SetWeather w) =
  modify $ \bs -> bs { weather = w }


-- | Applies all effect steps associated with a move.
applyMoveAction :: Move -> BattleM ()
applyMoveAction mv = do
  bs <- get
  hit <- maybe (pure True) checkAccuracy mv.accuracy
  if hit
    then mapM_ (applySingleStep mv) (gatherEffects mv)
    else do
      let (attackerPokemon, _) = getAttackerAndDefender bs
      logMiss attackerPokemon.name


-- | Applies a move given an explicit attacker and defender.
applyMoveWithContext :: Move -> PlayerSlot -> PlayerSlot -> BattleM ()
applyMoveWithContext mv attackerSlot defenderSlot = do
  -- Update the turn context with the explicit attacker and defender.
  modify $ \bs ->
    bs { turnContext = (turnContext bs)
                     { attacker = attackerSlot
                     , defender = defenderSlot
                     }
       }
  applyMoveAction mv
  -- Clear move modifiers immediately after processing this move.
  modify $ \bs' ->
    bs' { turnContext = (turnContext bs') { moveModifiers = [] } }

