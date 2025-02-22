module Pokemon.Battle.Ability.Types where 

import Data.Text (Text)

data AbilityTrigger 
    = OnMoveUse   -- ^ When a Pokemon uses a move. 
    | OnHit       -- ^ When a Pokemon is hit with a move. 
    | OnLowHP     -- ^ When a Pokemon reaches a certain HP threshold. 
    | OnSwitchOut -- ^ When a Pokemon is switched out.
    | Passive     -- ^ Ability that is always active. 
    deriving (Show, Eq)

-- | The context passed to an ability effect.
--   Parameterized over both the move type (a) and the ability user type (u)
--   so that we don’t have to depend on battle state.
data AbilityContext a u = AbilityContext
  { triggeringMove :: Maybe a  -- ^ The move that triggered the ability, if any.
  , abilityUser    :: u        -- ^ User of the ability. 
  } deriving (Show)

type AbilityEffectFunc m a u = AbilityContext a u -> m ()

data Ability m a u = Ability 
  { abilityName     :: Text 
  , abilityTriggers :: [AbilityTrigger] 
  , effect          :: AbilityEffectFunc m a u 
  } 

instance Show (Ability m a u) where
  show (Ability name triggers _) =
    "Ability { abilityName = " ++ show name
    ++ ", triggers = " ++ show triggers
    ++ ", effect = <function> }"
