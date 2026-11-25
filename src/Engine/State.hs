-- | The battle state: plain strict data (no functions, names, maps or IO).
--
-- Mirrors the parts of Showdown's @Battle@, @Side@ and @Pokemon@ objects that
-- slice 0 touches. Pokemon are stored by their original team index ("orig");
-- 'sSlots' maps Showdown's current slot order (what @switch N@ indexes) to orig.
module Engine.State
  ( Player (..)
  , other
  , playerIx
  , Gender (..)
  , Stats (..)
  , MoveSlot (..)
  , MonStatus (..)
  , Mon (..)
  , Side (..)
  , RequestState (..)
  , Result (..)
  , Actor (..)
  , ActionKind (..)
  , Action (..)
  , Battle (..)
  , sideOf
  , modifySide
  , monOf
  , modifyMon
  , activeOrig
  , activeMon
  , activeActor
  , slotMon
  ) where

import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine.Format (Format)

-- | The two players (Showdown @p1@, @p2@).
data Player = P1 | P2
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The opposing player.
other :: Player -> Player
other P1 = P2
other P2 = P1

-- | 0 for 'P1', 1 for 'P2'.
playerIx :: Player -> Int
playerIx P1 = 0
playerIx P2 = 1

-- | Pokemon gender ('GenderNone' renders as the empty string in Showdown).
data Gender = GenderM | GenderF | GenderNone
  deriving (Eq, Show)

-- | Non-HP stats (Showdown @storedStats@).
data Stats = Stats
  { stAtk :: !Int
  , stDef :: !Int
  , stSpa :: !Int
  , stSpd :: !Int
  , stSpe :: !Int
  }
  deriving (Eq, Show)

-- | A move slot (Showdown @moveSlots@ entry).
data MoveSlot = MoveSlot
  { msMove :: !Int
  , msPP :: !Int
  , msMaxPP :: !Int
  }
  deriving (Eq, Show)

-- | Pokemon status. Slice 0 has only the @fnt@ marker Showdown sets in @checkFainted@.
data MonStatus = StatusNone | StatusFainted
  deriving (Eq, Show)

-- | One Pokemon.
data Mon = Mon
  { monSpecies :: !Int
  , monLevel :: !Int
  , monGender :: !Gender
  , monMaxHp :: !Int
  , monHp :: !Int
  , monStats :: !Stats
  , monType1 :: !Int
  , monType2 :: !Int
  -- ^ -1 when single-typed.
  , monMoves :: !(SV.Vector MoveSlot)
  , monSpeed :: !Int
  -- ^ Showdown's cached @pokemon.speed@ (refreshed only by @updateSpeed@ / @setSpecies@).
  , monFainted :: !Bool
  , monFaintQueued :: !Bool
  , monActive :: !Bool
  -- ^ @isActive@.
  , monSwitchFlag :: !Bool
  , monStatus :: !MonStatus
  -- ^ @pokemon.status@: 'StatusFainted' (@fnt@) from @checkFainted@ until the Pokemon is replaced.
  }
  deriving (Eq, Show)

-- | One side.
data Side = Side
  { sMons :: !(SV.Vector Mon)
  -- ^ Indexed by orig (original team position); never reordered.
  , sSlots :: !(U.Vector Int)
  -- ^ Current slot order: slot -> orig. In singles the active Pokemon is slot 0.
  , sLeft :: !Int
  -- ^ @pokemonLeft@.
  , sHasActive :: !Bool
  -- ^ @side.active[0]@ is set (true once the lead has switched in).
  }
  deriving (Eq, Show)

-- | Showdown's @battle.requestState@.
data RequestState = ReqNone | ReqTeamPreview | ReqMove | ReqSwitch
  deriving (Eq, Show)

-- | Outcome of the battle.
data Result = Ongoing | Won !Player | Tied
  deriving (Eq, Show)

-- | A Pokemon identified by owner and orig.
data Actor = Actor !Player !Int
  deriving (Eq, Show)

-- | Showdown action choices that appear in the queue (battle-queue.ts).
data ActionKind
  = KTeam
  | KStart
  | KInstaSwitch
  | KBeforeTurn
  | KRunSwitch
  | KSwitch
  | KMove
  | KResidual
  deriving (Eq, Show)

-- | A queued action with Showdown's sort key (order, priority, speed; subOrder and effectOrder are 0).
data Action = Action
  { acKind :: !ActionKind
  , acOrder :: !Int
  , acPriority :: !Int
  , acSpeed :: !Int
  , acActor :: !(Maybe Actor)
  -- ^ @action.pokemon@.
  , acArg :: !Int
  -- ^ Move slot (-1 = Struggle) for 'KMove'; target orig for switches; team index for 'KTeam'.
  }
  deriving (Eq, Show)

-- | The whole battle.
data Battle = Battle
  { bFormat :: !Format
  , bTurn :: !Int
  , bRequest :: !RequestState
  , bResult :: !Result
  , bMidTurn :: !Bool
  , bQueue :: !(SV.Vector Action)
  , bFaintQueue :: !(SV.Vector Actor)
  , bP1 :: !Side
  , bP2 :: !Side
  }
  deriving (Eq, Show)

-- | A side of the battle.
sideOf :: Player -> Battle -> Side
sideOf P1 = bP1
sideOf P2 = bP2
{-# INLINE sideOf #-}

-- | Update a side.
modifySide :: Player -> (Side -> Side) -> Battle -> Battle
modifySide P1 f b = b {bP1 = f (bP1 b)}
modifySide P2 f b = b {bP2 = f (bP2 b)}
{-# INLINE modifySide #-}

-- | A Pokemon by owner and orig. Origs are valid by construction.
monOf :: Battle -> Actor -> Mon
monOf b (Actor p o) = sMons (sideOf p b) SV.! o
{-# INLINE monOf #-}

-- | Update a Pokemon.
modifyMon :: Actor -> (Mon -> Mon) -> Battle -> Battle
modifyMon (Actor p o) f = modifySide p $ \s ->
  let m = f (sMons s SV.! o)
   in m `seq` s {sMons = sMons s SV.// [(o, m)]}
{-# INLINE modifyMon #-}

-- | Orig of the Pokemon in slot 0 of a side.
activeOrig :: Side -> Int
activeOrig s = case U.uncons (sSlots s) of
  Just (o, _) -> o
  Nothing -> 0
{-# INLINE activeOrig #-}

-- | The Pokemon in slot 0 (Showdown @side.active[0]@ once started).
activeActor :: Player -> Battle -> Actor
activeActor p b = Actor p (activeOrig (sideOf p b))
{-# INLINE activeActor #-}

-- | The Pokemon in slot 0.
activeMon :: Player -> Battle -> Mon
activeMon p b = monOf b (activeActor p b)

-- | The Pokemon in a given slot, if any.
slotMon :: Side -> Int -> Maybe Mon
slotMon s i = (sMons s SV.!?) =<< (sSlots s U.!? i)
