{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Pokemon.AI.Internal where

import           Control.Monad                       (forM_)
import           Control.Monad.IO.Class              (liftIO)
import           Pokemon.AI.Types                    (Strategy)
import           Pokemon.Battle.Action.Internal      (Action (..),
                                                      ActionType (..))
import           Pokemon.Battle.BattleState.Internal (BattleM, getRNG,
                                                      updateRNG)
import           Pokemon.Data.Move.Internal          (Move (category),
                                                      MoveCategory (..))
import           Pokemon.Pretty                      (printAction)
import           System.Random                       (Random (randomR))

-- | A strategy that chooses an action uniformly at random.
uniformStrategy :: Strategy BattleM
uniformStrategy actions = do
  rng0 <- getRNG
  let (idx, newRng) = randomR (0, length actions - 1) rng0
  updateRNG newRng
  return (actions !! idx)

-- | Don't switch unless the active Pokemon faints.
--   Non-status moves are twice as likely to occur.
preferAttackStrategy :: Strategy BattleM
preferAttackStrategy actions = do
  let weightedActions = [ (act, if mv.category == Status then 1 else 2)
                        | act@(Action { actionType = MoveAction mv }) <- actions ]
  case weightedActions of
    [] -> error "preferAttackStrategy: no move actions available!" -- Just a placeholder.
    _  -> do
      let totalWeight = sum (map snd weightedActions)
      rng0 <- getRNG
      let (r, newRng) = randomR (0, totalWeight - 1) rng0
      updateRNG newRng
      return (chooseWeighted r weightedActions)

-- | Choose which action to take interactively.
--   Useful for examining state in ghci.
interactiveStrategy :: Strategy BattleM
interactiveStrategy actions = do
  liftIO $ putStrLn "Choose an action:"
  -- For each action, print its index and pretty-print the action.
  forM_ (zip [0 :: Int .. ] actions) $ \ (i, act) -> do
    liftIO $ putStr (show i ++ ": ")
    liftIO $ printAction act
  input <- liftIO getLine
  let idx = read input :: Int
  if idx < 0 || idx >= length actions
    then do
      liftIO $ putStrLn "Invalid choice. Defaulting to first action."
      return (head actions)
    else return (actions !! idx)

-- Choose an action based on weighted probabilities.
chooseWeighted :: Int -> [(Action, Int)] -> Action
chooseWeighted _ [] = error "chooseWeighted: empty list"
chooseWeighted r ((act, w):rest) =
  if r < w then act else chooseWeighted (r - w) rest


