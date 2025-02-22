module Pokemon.Battle.Ability.Internal where 

import Data.Text (Text)
import Pokemon.Data.Move.Internal (Move)
import Pokemon.Battle.BattleState.Internal (PlayerSlot)

data AbilityTrigger 
    = OnMoveUse   -- ^ When a Pokemon uses a move. 
    | OnHit       -- ^ When a Pokemon is hit with a move. 
    | OnLowHP     -- ^ When a Pokemon reaches a certain HP threshold. 
    | OnSwitchOut -- ^ When a Pokemon is switched out.
    | Passive     -- ^ Ability that is always active. 
    deriving (Show, Eq)

data AbilityContext = AbilityContext
  { triggeringMove :: Maybe Move  -- ^ The move that triggered the ability, if any.
  , abilityUser    :: PlayerSlot  -- ^ User of the ability. 
  } deriving (Show)

type AbilityEffectFunc m a u = AbilityContext a u -> m ()

data Ability m a u = Ability 
  { abilityName     :: Text 
  , abilityTriggers :: [AbilityTrigger] 
  , effect          :: AbilityEffectFunc m a u 
  } 

instance Show (Ability m a u) where
  show :: Ability m a u -> String
  show (Ability name triggers _) =
    "Ability { abilityName = " ++ show name
    ++ ", triggers = " ++ show triggers
    ++ ", effect = <function> }"
