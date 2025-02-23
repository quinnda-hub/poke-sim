{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Ability.Internal where

import           Control.Monad.State                 (MonadState (get), when)
import           Pokemon.Battle.Ability.Types        (Ability (abilityTriggers, effect),
                                                      AbilityContext (AbilityContext, abilityUser, triggeringMove),
                                                      AbilityTrigger (OnMoveUse))
import           Pokemon.Battle.BattleState.Internal (BattleM, PlayerSlot,
                                                      getPokemonForSlot)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move)

