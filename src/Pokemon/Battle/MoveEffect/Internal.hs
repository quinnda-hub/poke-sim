{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Internal
  ( EffectStep(..)
  , applyEffectForMove
  , resolveTargets
  , checkAccuracy
  , getAttackerAndDefender
  ) where

import           Control.Monad                       (forM_)
import           Control.Monad.Trans.State           (get, modify)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (Player1, Player2),
                                                      TurnContext (..),
                                                      applyModifiers, getRNG,
                                                      updateRNG)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Accuracy (..), Move (..),
                                                      Target (..))
import           Pokemon.Mechanics.Weather           (Weather)
import           System.Random                       (Random (randomR))

applyEffectForMove
  :: Move
  -> Pokemon  -- ^ Attacker
  -> Pokemon  -- ^ Defender
  -> [PlayerSlot]
  -> (Move -> Pokemon -> Pokemon -> PlayerSlot -> BattleState -> BattleM a)
       -- ^ Effect: computes an effect value (like damage) given the freshly modified move and state.
  -> (a -> PlayerSlot -> BattleState -> BattleState)
       -- ^ Modifier: updates the battle state using the computed effect value.
  -> (a -> PlayerSlot -> BattleState -> BattleM ())
       -- ^ Logger: logs the effect using the computed value.
  -> BattleM ()
applyEffectForMove mv attackerPokemon defenderPokemon targetSlots effectFn modifier logger =
  forM_ targetSlots $ \slot -> do
    bs <- get
    let mv' = applyModifiers bs.turnContext.moveModifiers mv
    effect <- effectFn mv' attackerPokemon defenderPokemon slot bs
    modify (modifier effect slot)
    bs' <- get
    logger effect slot bs'

--------------------------------------------------------------------------------
-- Effect Steps
--------------------------------------------------------------------------------

-- | Represents a single effect step during move usage.
data EffectStep
    = DoDamage            -- ^ Indicates that damage should be calculated and applied.
    | InflictStatus       -- ^ Inflicts a primary status effect.
    | BoostStats          -- ^ Raises or lowers the targets stats.
    | SetWeather Weather  -- ^ Change the battle's weather.

--------------------------------------------------------------------------------
-- Target Resolution and Damage Helpers
--------------------------------------------------------------------------------
-- | Determines which player(s) are affected based on the move's target
--   and the current turn context.
resolveTargets :: Target -> TurnContext -> [PlayerSlot]
resolveTargets t tc =
  case t of
    Self   -> [tc.attacker]
    Normal -> [tc.defender]
    _      -> [] -- Placeholder.

getAttackerAndDefender :: BattleState -> (Pokemon, Pokemon)
getAttackerAndDefender bs  =
  let attackerPokemon = case bs.turnContext.attacker of
                          Player1 -> bs.player1.active
                          Player2 -> bs.player2.active
      defenderPokemon = case bs.turnContext.defender of
                          Player1 -> bs.player1.active
                          Player2 -> bs.player2.active
  in (attackerPokemon, defenderPokemon)

--------------------------------------------------------------------------------
-- RNG checks
--------------------------------------------------------------------------------

checkAccuracy :: Accuracy -> BattleM Bool
checkAccuracy AlwaysHits = return True
checkAccuracy (Percent acc) = do
    rng0 <- getRNG
    let (roll, newRng) = randomR (1, 100) rng0
    updateRNG newRng
    return (roll <= acc)
