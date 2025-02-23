{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Ability where

import           Control.Monad                       (when)
import           Control.Monad.Trans.State           (get, modify)
import           Data.Maybe                          (fromMaybe)
import           Pokemon.Battle.Ability.Types        (Ability (..),
                                                      AbilityContext (..),
                                                      AbilityEffectFunc,
                                                      AbilityTrigger (..))
import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState (..),
                                                      PlayerSlot,
                                                      getPokemonForSlot,
                                                      moveModifiers)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon(..))
import           Pokemon.Data.Move.Internal          (Move (basePower, category, moveType),
                                                      MoveCategory (Status))
import           Pokemon.Mechanics.PType             (PType (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)

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

blazeEffect :: AbilityEffectFunc
blazeEffect ctx = do
    bs <- get
    let p = getPokemonForSlot (abilityUser ctx) bs
    case ctx.triggeringMove of
        Nothing -> pure ()
        Just move
          | move.category == Status      -> pure () -- Move is not a damaging move.
          | p.hp  * 3 > p.maxHP          -> pure () -- HP is not low enough to activate.
          | move.moveType /= Fire        -> pure () -- Move is not a fire move.
          | otherwise -> do
            let originalPower = fromMaybe 0 move.basePower
                boostedPower  = round ((1.5 :: Double) * fromIntegral originalPower)
                boost m       = m { basePower = Just boostedPower }
            modify $ \bs' -> bs' { turnContext = (turnContext bs')
                                               { moveModifiers = boost : moveModifiers (turnContext bs') } }
            pure ()

-- | Blaze ability instance.
blazeAbility :: Ability
blazeAbility = Ability
  { abilityName     = "Blaze"
  , abilityTriggers = [OnMoveUse]
  , effect          = blazeEffect
  }

  -- | A registry mapping ability names to their definitions.
abilityRegistry :: Map.Map Text Ability
abilityRegistry = Map.fromList
  [ (abilityName blazeAbility, blazeAbility)
  ]

-- | Helper function to look up an ability by its name.
lookupAbility :: Text -> Maybe Ability
lookupAbility n = Map.lookup n abilityRegistry
