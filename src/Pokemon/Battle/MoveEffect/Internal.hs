{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.Internal
  ( EffectStep(..)
  , gatherEffects
  , applyMoveAction
  , resolveTargets
  , applyMoveWithContext
  ) where

import           Control.Monad                       (forM_)
import           Control.Monad.Trans.State           (get, modify)
import           Data.Maybe                          (catMaybes)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player (..),
                                                      PlayerSlot (Player1, Player2),
                                                      TurnContext (..),
                                                      applyModifiers, getRNG,
                                                      updateRNG)
import           Pokemon.Battle.Damage.Internal      (calculateDamage)
import           Pokemon.Battle.Logging              (logDamage, logMiss)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Accuracy (..), Move (..),
                                                      Status (..), Target (..),
                                                      basePower, target,
                                                      weatherEffect)
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
    modify (\bs' -> modifier effect slot bs')
    bs' <- get
    logger effect slot bs'


--------------------------------------------------------------------------------
-- Effect Steps
--------------------------------------------------------------------------------

-- | Represents a single effect step during move usage.
data EffectStep
    = DoDamage            -- ^ Indicates that damage should be calculated and applied.
    | InflictStatus
    | SetWeather Weather  -- ^ Change the battle's weather.

-- | Collects all effect steps from a move.
gatherEffects :: Move -> [EffectStep]
gatherEffects mv =
  catMaybes [ gatherDamage mv
            , gatherWeather mv ]

-- | If the move has a base power, produce a damage step.
gatherDamage :: Move -> Maybe EffectStep
gatherDamage mv =
  case basePower mv of
    Just _  -> Just DoDamage
    Nothing -> Nothing

-- | If the move sets weather, produce that effect.
gatherWeather :: Move -> Maybe EffectStep
gatherWeather mv =
  case mv.weatherEffect of
    Just w  -> Just (SetWeather w)
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Damage Application Helper
--------------------------------------------------------------------------------

-- | Shared helper to calculate and apply damage for a move.
applyDamageForMove :: Move -> Pokemon -> Pokemon -> [PlayerSlot] -> BattleM ()
applyDamageForMove mv attackerPokemon defenderPokemon targetSlots = do
  bs  <- get
  let tc  = bs.turnContext
      mv' = applyModifiers tc.moveModifiers mv
  dmg <- calculateDamage mv' attackerPokemon defenderPokemon
  forM_ targetSlots $ \slot -> do
    modify (applyDamage slot dmg)
    bs' <- get  -- Retrieve the updated battle state for logging.
    let targetName = case slot of
                       Player1 -> bs'.player1.active.name
                       Player2 -> bs'.player2.active.name
    logDamage dmg targetName

--------------------------------------------------------------------------------
-- Status
--------------------------------------------------------------------------------

-- | Shared helper to calculate and apply damage for a move.
applyStatusForMove :: Status  -> [PlayerSlot] -> BattleM ()
applyStatusForMove mv targetSlots = do
  forM_ targetSlots $ \slot -> do
    modify (applyStatus slot mv)


--------------------------------------------------------------------------------
-- Applying Effects
--------------------------------------------------------------------------------

-- | Applies a single effect step using the current move context.
applySingleStep :: Move -> EffectStep -> BattleM ()
applySingleStep mv DoDamage = do
  bs  <- get
  hit <- maybe (pure True) checkAccuracy mv.accuracy    -- Assuming Accuracy being Nothing means it also always hits. Will check later.
  let (attackerPokemon, defenderPokemon) = getAttackerAndDefender bs
      ts      = mv.target
      targets = resolveTargets ts bs.turnContext
  if hit
    then applyDamageForMove mv attackerPokemon defenderPokemon targets
    else logMiss attackerPokemon.name

applySingleStep mv InflictStatus = do
  bs  <- get
  hit <- maybe (pure True) checkAccuracy mv.accuracy
  let (attackerPokemon, _) = getAttackerAndDefender bs
      ts = mv.target
      targets = resolveTargets ts bs.turnContext
  if hit
    then case mv.status of
      Just s  -> applyStatusForMove s targets
      Nothing -> pure ()
      else logMiss attackerPokemon.name

applySingleStep _ (SetWeather w) =
  modify $ \bs -> bs { weather = w }

-- | Applies all effect steps associated with a move.
applyMoveAction :: Move -> BattleM ()
applyMoveAction mv = mapM_ (applySingleStep mv) (gatherEffects mv)

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

-- | Applies damage to the specified player slot.
applyDamage :: PlayerSlot -> Int -> BattleState -> BattleState
applyDamage Player1 dmg bs = bs { player1 = applyDamageToActive dmg bs.player1 }
applyDamage Player2 dmg bs = bs { player2 = applyDamageToActive dmg bs.player2 }

-- | Applies status to the specified player slot.
applyStatus :: PlayerSlot -> Status -> BattleState -> BattleState
applyStatus Player1 sts bs = bs { player1 = applyStatusToActive sts bs.player1 }
applyStatus Player2 sts bs = bs { player2 = applyStatusToActive sts bs.player2 }

-- | Updates a player's active Pokémon with new HP after damage.
applyStatusToActive :: Status -> Player -> Player
applyStatusToActive sts p = p { active = p.active { pstatus = Just sts } }

-- | Updates a player's active Pokémon with new HP after damage.
applyDamageToActive :: Int -> Player -> Player
applyDamageToActive dmg p =
  let oldHP = p.active.hp
      newHP = max 0 (oldHP - dmg)
  in p { active = p.active { hp = newHP } }

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
