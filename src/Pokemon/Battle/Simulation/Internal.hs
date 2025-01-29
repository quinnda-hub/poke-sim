{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.Battle.Simulation.Internal where 

import Pokemon.Battle.BattleState.Internal
  ( BattleM
  , BattleState(..)
  , Player(..)
  , PlayerSlot(..)
  , TurnContext(..)
  )

import Control.Monad.Trans.State (get, put)
import Control.Monad.Trans.Writer (tell)
import Data.Text (Text)
import qualified Data.Text as T
import Pokemon.Data.Move
import Pokemon.Battle.MoveEffect.Internal
  ( applySingleStep
  , gatherEffects
  )

-- | Apply the entire move (all effect steps) from the attacker to the defender. 
applyMove :: Move -> BattleM () 
applyMove mv = do 
    let steps = gatherEffects mv 
    mapM_ (applySingleStep mv) steps 

-- | Swaps the turnContext attacker/defender in a 1v1 scenario. 
swapAttackerDefender :: BattleState -> BattleState 
swapAttackerDefender bs = 
    let oldAtt = bs.turnContext.attacker 
        oldDef = bs.turnContext.defender
        newTC  = bs.turnContext{ attacker = oldDef, defender = oldAtt }
    in bs{ turnContext = newTC }
