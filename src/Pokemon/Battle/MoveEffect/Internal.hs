{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Internal module for Move Effect handling and application.
module Pokemon.Battle.MoveEffect.Internal (applySingleStep,
                                           gatherEffects) where
import           Control.Monad.Trans.Class           (MonadTrans (lift))

import           Control.Monad                       (forM_)
import           Control.Monad.Trans.State           (get, modify)
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      Player, PlayerSlot (..),
                                                      TurnContext (..), active,
                                                      attacker, defender)
import           Pokemon.Battle.Damage.Internal      (calculateDamage)
import           Pokemon.Data.Move.Internal          (Move, Target (..),
                                                      basePower, target,
                                                      weatherEffect)
import           Pokemon.Mechanics.Weather           (Weather)
import qualified Data.Text as T
import Control.Monad.Trans.Writer (tell)
import Pokemon.Battle.Pokemon (Pokemon(..))


--------------------------------------------------------------------------------
-- Effect Steps
--------------------------------------------------------------------------------

-- | Represents a single step (or action) to be applied during move usage.
--   For example, 'DoDamage' or 'SetWeather'.
data EffectStep
    = DoDamage            -- ^ A monadic action that computes damage.
    | SetWeather Weather  -- ^ Change the battle's weather.


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
    Just _ -> Just DoDamage
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
applySingleStep mv DoDamage = do
  bs <- get
  -- Retrieve the attacker and defender based on the current turn context.
  let attackerPokemon = case bs.turnContext.attacker of
                          Player1 -> bs.player1.active
                          Player2 -> bs.player2.active
      defenderPokemon = case bs.turnContext.defender of
                          Player1 -> bs.player1.active
                          Player2 -> bs.player2.active

  dmg <- calculateDamage mv attackerPokemon defenderPokemon
  let ts      = mv.target
      targets = resolveTargets ts bs.turnContext
  forM_ targets $ \slot -> do 
    modify (applyDamage slot dmg)
    let targetName = case slot of 
                       Player1 -> bs.player1.active.name 
                       Player2 -> bs.player2.active.name 
    lift $ tell [ T.concat ["[Damage] ", T.pack (show dmg), " damage dealt to ", targetName] ]
    
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
    Self   -> [tc.attacker]
    -- For single battles, 'Normal' typically refers to the defender.
    Normal -> [tc.defender]
    -- TODO: Handle multi-target, allies, etc.
    _      -> []


--------------------------------------------------------------------------------
-- Damage Helpers
--------------------------------------------------------------------------------

-- | Applies damage to the specified 'PlayerSlot'.
--   For now, it's just a placeholder. Extend this to subtract HP, etc.
applyDamage :: PlayerSlot -> Int -> BattleState -> BattleState
applyDamage Player1 dmg bs = bs { player1 = applyDamageToActive dmg bs.player1 }
applyDamage Player2 dmg bs = bs{ player2 = applyDamageToActive dmg bs.player2 }

applyDamageToActive :: Int -> Player -> Player
applyDamageToActive dmg p =
  let oldHP = p.active.hp
      newHP = max 0 (oldHP - dmg)
  in p{ active = p.active{ hp = newHP } }
