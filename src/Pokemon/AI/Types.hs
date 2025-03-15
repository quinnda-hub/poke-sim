module Pokemon.AI.Types (Strategy) where

import           Pokemon.Battle.Action.Internal (Action)

-- Polymorphic Strategy type, independent of any concrete monad.
type Strategy m = [Action] -> m Action

