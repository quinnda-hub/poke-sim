{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Internal module for Move Effect handling and application.
module Pokemon.Battle.MoveEffect.Internal where

import           Control.Monad            (forM_)
import           Control.Monad.Trans.State (modify, get)
import           Pokemon.Battle.BattleState.Internal
  ( BattleM
  , BattleState(..)
  , PlayerSlot (Player1Active, Player2Active)
  , TurnContext
  , attackerIndex
  , defenderIndex
  )
import           Pokemon.Data.Move.Internal
  ( Move()
  , basePower
  , Target(..)
  , target
  , weatherEffect
  )
import           Pokemon.Mechanics.Weather (Weather)


--------------------------------------------------------------------------------
-- Effect Steps
--------------------------------------------------------------------------------

-- | Represents a single step (or action) to be applied during move usage.
--   For example, 'DoDamage' or 'SetWeather'.
data EffectStep
    = DoDamage (BattleM Int) -- ^ A monadic action that computes damage.
    | SetWeather Weather     -- ^ Change the battle's weather.


--------------------------------------------------------------------------------
-- Gathering Effects from a Move
--------------------------------------------------------------------------------

-- | Collects all 'EffectStep' actions from a 'Move'.
--   This function inspects relevant fields (like base power, weather, etc.)
--   and converts them into step-by-step actions that can be applied later.
gatherEffects :: Move -> [EffectStep]
gatherEffects mv =
  maybeToList (gatherDamage mv)
    ++ maybeToList (gatherWeather mv)
  where
    -- Converts 'Maybe x' to '[x]'.
    maybeToList :: Maybe a -> [a]
    maybeToList = maybe [] pure

-- | If the 'Move' has a base power, produce a damage step.
gatherDamage :: Move -> Maybe EffectStep
gatherDamage mv =
  case basePower mv of
    Just bp -> Just (DoDamage (pure bp)) -- for now, just a constant action
    Nothing -> Nothing

-- | If the 'Move' sets weather, produce a 'SetWeather' step.
gatherWeather :: Move -> Maybe EffectStep
gatherWeather mv =
  case mv.weatherEffect of
    Just w  -> Just (SetWeather w)
    Nothing -> Nothing


--------------------------------------------------------------------------------
-- Applying Effects
--------------------------------------------------------------------------------

-- | Applies a single 'EffectStep' using the current 'Move' context.
--   This function may read from the 'BattleState' (for example, to see
--   which player is targeted) and then modify the state accordingly.
applySingleStep :: Move -> EffectStep -> BattleM ()
applySingleStep mv (DoDamage dmgAction) = do
  dmg <- dmgAction
  bs  <- get
  let ts      = mv.target
      targets = resolveTargets ts bs.turnContext
  forM_ targets $ \slot ->
    modify (applyDamage slot dmg)

applySingleStep _ (SetWeather w) =
  modify $ \bs -> bs { weather = w }

--------------------------------------------------------------------------------
-- Target Resolution
--------------------------------------------------------------------------------

-- | Determines which player(s) are affected based on the 'Target'
--   and the current 'TurnContext'.
resolveTargets :: Target -> TurnContext -> [PlayerSlot]
resolveTargets t tc =
  case t of
    -- For single battles, 'Self' typically refers to the attacker.
    Self ->
      case tc.attackerIndex of
        1 -> [Player1Active]
        2 -> [Player2Active]
        _ -> []
    -- For single battles, 'Normal' typically refers to the defender.
    Normal ->
      case tc.defenderIndex of
        1 -> [Player1Active]
        2 -> [Player2Active]
        _ -> []
    -- TODO: Handle multi-target, allies, etc.
    _ -> []


--------------------------------------------------------------------------------
-- Damage Helpers
--------------------------------------------------------------------------------

-- | Applies damage to the specified 'PlayerSlot'.
--   For now, it's just a placeholder. Extend this to subtract HP, etc.
applyDamage :: PlayerSlot -> Int -> BattleState -> BattleState
applyDamage = undefined
  -- Example:
  -- applyDamage Player1Active dmg bs = bs{ player1 = applyDamageToActive dmg bs.player1 }
  -- applyDamage Player2Active dmg bs = bs{ player2 = applyDamageToActive dmg bs.player2 }

{-
-- Hypothetical "applyDamageToActive" if you had HP in your Player record:
applyDamageToActive :: Int -> Player -> Player
applyDamageToActive dmg p =
  let oldHP = p.active.hp
      newHP = max 0 (oldHP - dmg)
  in p{ active = p.active{ hp = newHP } }
-}
