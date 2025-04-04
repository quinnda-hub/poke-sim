{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.MoveEffect.RecoilDamage where

import           Pokemon.Battle.BattleState.Internal (BattleM, BattleState,
                                                      player1, player2)
import           Pokemon.Battle.BattleState.Types    (Player (..),
                                                      PlayerSlot (..))
import           Pokemon.Battle.Logging              (logDamage)
import           Pokemon.Battle.MoveEffect.Internal  (EffectStep (RecoilDamage),
                                                      applyEffectForMove)
import           Pokemon.Battle.Pokemon.Internal     (Pokemon (..))
import           Pokemon.Data.Move.Internal          (Move (recoil), multihit)

import           Data.Maybe                          (isJust)
import           Pokemon.Battle.Damage.Internal      (calculateDamage)

gatherRecoil :: Move -> Maybe EffectStep
gatherRecoil mv =
  case mv.recoil of
    Nothing -> Nothing
    Just _  -> Just RecoilDamage

