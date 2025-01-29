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

import Pokemon.Data.Move.Internal ( Move )
import Pokemon.Battle.MoveEffect.Internal
  ( applySingleStep
  , gatherEffects
  )
import Pokemon.Battle.Pokemon.Internal (MoveSet(..),
                                        Pokemon(..))

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

-- | Increments the turn counter by 1. 
incrementTurn :: BattleState -> BattleState
incrementTurn bs = 
  bs{ turnNumber = bs.turnNumber + 1}

-- | Simply choose the first move a Pokemon has. 
chooseMove :: PlayerSlot -> BattleState -> Move 
chooseMove Player1 bs = 
  bs.player1.active.moves.move1
chooseMove Player2 bs = 
  bs.player2.active.moves.move1 
