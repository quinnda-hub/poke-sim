{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.StatusEffects where

import           Control.Monad.State.Class             (MonadState (get))
import           Pokemon.Battle.BattleState.Internal   (BattleM,
                                                        BattleState (..))
import           Pokemon.Battle.BattleState.PlayerSlot (PlayerSlot (..))
import           Pokemon.Battle.BattleState.Types      (Player (..))
import           Pokemon.Battle.Pokemon                (Pokemon)

processStatusEffects :: BattleM ()
processStatusEffects = do
  bs <- get
  return ()

getActivePokemon :: BattleState -> PlayerSlot -> Pokemon
getActivePokemon bs Player1 = bs.player1.active
getActivePokemon bs Player2 = bs.player2.active
