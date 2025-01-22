module Pokemon.Battle.MoveEffect.Internal where

import Pokemon.Mechanics.Weather (Weather)
import Pokemon.Battle.BattleState.Internal (BattleM,
                                            BattleState(..), PlayerSlot)
import Control.Monad.Trans.State (modify, get)
import Pokemon.Data.Move.Internal ( Move, basePower, Target(..))

data EffectStep
    = DoDamage Target (BattleM Int)
    | SetWeather Weather

gatherEffects :: Move -> [EffectStep]
gatherEffects mv =
  maybeToList (gatherDamage mv) ++
  maybeToList (gatherWeather mv)
  where
    maybeToList = maybe [] pure

applySingleStep :: EffectStep -> BattleM ()
applySingleStep (DoDamage dmgAction) = do
    dmg <- dmgAction
    -- 1) read from the state who should be damaged. 
    bs  <- get 
    let whoGetshit = determinedefender (turnContext bs) 
    -- 2) apply damage to that side.
    modify $ \bs' -> applyDamage whoGetshit dmg bs' 
    

applySingleStep (SetWeather w) = do
    modify $ \bs -> bs { weather = w}

applyDamage :: a
applyDamage = undefined

-- If 'basePower' is Just, we do a 'DealDamage' effect:
gatherDamage :: Move -> Maybe EffectStep
gatherDamage mv = 
  case basePower mv of
    Just bp -> Just (DoDamage (pure bp))
    Nothing -> Nothing

gatherWeather = undefined 
