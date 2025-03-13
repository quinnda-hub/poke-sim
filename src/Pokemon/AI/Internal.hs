module Pokemon.AI.Internal where

import           Pokemon.Battle.Action.Internal      (Action)
import           Pokemon.Battle.BattleState.Internal (BattleM, getRNG,
                                                      updateRNG)
import           System.Random                       (Random (randomR))

-- | A strategy is a function that, given a list of valid actions,
--   returns an action in the BattleM monad.
type Strategy = [Action] -> BattleM Action

-- | A strategy that chooses an action uniformly at random.
uniformStrategy :: Strategy
uniformStrategy actions = do
  rng0 <- getRNG
  let (idx, newRng) = randomR (0, length actions - 1) rng0
  updateRNG newRng
  return (actions !! idx)

