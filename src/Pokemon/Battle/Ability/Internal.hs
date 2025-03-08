{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs          #-}

module Pokemon.Battle.Ability.Internal where

import           Data.Text                           (Text)
import           Pokemon.Battle.BattleState.Internal (BattleM, PlayerSlot)
import           Pokemon.Data.Move.Internal          (Move)

--------------------------------------------------------------------------------
-- Ability Triggers and Context
--------------------------------------------------------------------------------

-- | Defines when an ability should be triggered.
data AbilityTrigger
    = OnMoveUse   -- ^ When a move is used.
    | OnHit       -- ^ When the Pokémon is hit.
    | OnLowHP     -- ^ When the Pokémon reaches a low HP threshold.
    | OnSwitchOut -- ^ When the Pokémon is switched out.
    | Passive     -- ^ Always active.
    deriving (Show, Eq)

-- | Context provided to an ability when it triggers.
data AbilityContext = AbilityContext
  { triggeringMove :: Maybe Move  -- ^ The move that triggered the ability (if any).
  , abilityUser    :: PlayerSlot  -- ^ The Pokémon (or slot) using the ability.
  } deriving (Show)

--------------------------------------------------------------------------------
-- Ability Data Type
--------------------------------------------------------------------------------

-- | Type alias for an ability’s effect function.
--   The effect runs in a monadic context (BattleM) that handles battle state.
type AbilityEffectFunc = AbilityContext -> BattleM ()

-- | Represents an ability with a name, trigger conditions, and its effect.
data Ability = Ability
  { abilityName     :: Text              -- ^ The ability's name.
  , abilityTriggers :: [AbilityTrigger]  -- ^ When the ability should trigger.
  , effect          :: AbilityEffectFunc -- ^ The effect to execute when triggered.
  }

instance Show Ability where
  show :: Ability -> String
  show (Ability n triggers _) =
    "Ability { abilityName = " ++ show n ++ ", triggers = " ++ show triggers ++ " }"
