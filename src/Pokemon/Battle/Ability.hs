{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pokemon.Battle.Ability where

import           Control.Monad.Trans.State           (get)
import           Data.Maybe                          (fromMaybe)
import           Pokemon.Battle.Ability.Internal     (Ability (..),
                                                      AbilityContext (abilityUser, triggeringMove),
                                                      AbilityEffectFunc,
                                                      AbilityTrigger (..))
import           Pokemon.Battle.BattleState.Internal (getPokemonForSlot)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (hp, maxHP))
import           Pokemon.Data.Move.Internal          (Move (basePower, category, moveType),
                                                      MoveCategory (Special))
import           Pokemon.Mechanics.PType             (PType (..))

blazeEffect :: AbilityEffectFunc
blazeEffect ctx = do
    bs <- get
    let p = getPokemonForSlot (abilityUser ctx) bs
    case ctx.triggeringMove of
        Nothing -> pure ()
        Just move
          | move.category == Special     -> pure () -- Move is not a damaging move.
          | p.hp  * 3 > p.maxHP          -> pure () -- HP is not low enough to activate.
          | move.moveType /= Fire  -> pure () -- Move is not a fire move.
          | otherwise -> do
            let originalPower = fromMaybe 0 move.basePower
                boostedPower  = round (1.5 * fromIntegral originalPower)
              in undefined

-- | Blaze ability instance.
blazeAbility :: Ability
blazeAbility = Ability
  { abilityName     = "Blaze"
  , abilityTriggers = [OnMoveUse]  -- You could also include OnLowHP if you want to trigger earlier.
  , effect          = blazeEffect
  }
