{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Ability where

import           Control.Monad                       (when)
import           Control.Monad.Trans.State           (get, modify)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import           Data.Text                           (Text)
import           Pokemon.Battle.Ability.Types        (Ability (..),
                                                      AbilityContext (..),
                                                      AbilityEffectFunc,
                                                      AbilityTrigger (..))
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      PlayerSlot,
                                                      getPokemonForSlot,
                                                      moveModifiers)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (basePower, category, moveType),
                                                      MoveCategory (Status))
import           Pokemon.Mechanics.PType             (PType (..))

triggerAbilityOnMoveUse :: PlayerSlot -> Move -> BattleM ()
triggerAbilityOnMoveUse slot mv = do
  bs <- get
  let p  = getPokemonForSlot slot bs
      an = p.ability
  case lookupAbility an of
    Nothing -> do pure ()
    Just a  ->
      when (OnMoveUse `elem` abilityTriggers a)
      $ let ctx = AbilityContext { triggeringMove = Just mv, abilityUser = slot }
        in effect a ctx

-- | A generic ability effect that boosts a move's power.
--   It activates if:
--     - The move is damaging (not Status).
--     - The Pokémon's HP is 1/3 or less.
--     - The move's type matches the given required type.
--   The boost factor is applied to the move's base power.
boostAbilityEffect :: PType       -- ^ Required move type (e.g. Fire for Blaze, Grass for Overgrow)
                   -> Double      -- ^ Boost factor (e.g. 1.5)
                   -> AbilityEffectFunc
boostAbilityEffect requiredType factor ctx = do
    bs <- get
    let p = getPokemonForSlot (abilityUser ctx) bs
    case triggeringMove ctx of
        Nothing -> pure ()
        Just move
          | move.category == Status       -> pure () -- Non-damaging move.
          | p.hp * 3 > p.maxHP            -> pure () -- HP is not low enough.
          | move.moveType /= requiredType -> pure () -- Move doesn't match required type.
          | otherwise -> do
              let originalPower = fromMaybe 0 move.basePower
                  boostedPower  = round (factor * fromIntegral originalPower)
                  boost m       = m { basePower = Just boostedPower }
              modify $ \bs' ->
                bs' { turnContext = (turnContext bs')
                            { moveModifiers = boost : moveModifiers (turnContext bs') } }
              pure ()

blazeEffect :: AbilityEffectFunc
blazeEffect = boostAbilityEffect Fire 1.5

-- | Blaze ability instance.
blazeAbility :: Ability
blazeAbility = Ability
  { abilityName     = "Blaze"
  , abilityTriggers = [OnMoveUse]
  , effect          = blazeEffect
  }

overgrowEffect :: AbilityEffectFunc
overgrowEffect = boostAbilityEffect Grass 1.5

-- | Overgrow ability instance.
overgrowAbility :: Ability
overgrowAbility = Ability
  { abilityName     = "Overgrow"
  , abilityTriggers = [OnMoveUse]
  , effect          = overgrowEffect
  }

  -- | A registry mapping ability names to their definitions.
abilityRegistry :: Map.Map Text Ability
abilityRegistry = Map.fromList
  [ (abilityName blazeAbility, blazeAbility)
  , (abilityName overgrowAbility, overgrowAbility)
  ]

-- | Helper function to look up an ability by its name.
lookupAbility :: Text -> Maybe Ability
lookupAbility n = Map.lookup n abilityRegistry
