{-# LANGUAGE Trustworthy #-}
module Pokemon.AI.Internal where

import           Pokemon.Battle.Action.Internal      (Action)
import           Pokemon.Battle.BattleState.Internal (BattleM)

-- Forward declaration of Strategy
type Strategy = [Action] -> BattleM Action

