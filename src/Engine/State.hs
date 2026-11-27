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
  , Boosts (..)
  , noBoosts
  , boostOf
  , setBoostOf
  , MoveSlot (..)
  , MonStatus (..)
  , noItem
  , EffectState (..)
  , noEffectState
  , Volatile (..)
  , CondEntry (..)
  , Field (..)
  , noField
  , noCond
  , SwitchFlag (..)
  , Mon (..)
  , Side (..)
  , RequestState (..)
  , Result (..)
  , Actor (..)
  , ActionKind (..)
  , Action (..)
  , MoveInUse (..)
  , Battle (..)
  , takeEffectOrder
  , sideOf
  , modifySide
  , monOf
  , modifyMon
  , activeOrig
  , activeMon
  , activeActor
  , slotMon
  , willAct
  , willMove
  ) where

import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine.Format (Format)
import Engine.Rule (BoostStat (..))

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

-- | Stat stages (Showdown @pokemon.boosts@), each in [-6, 6]. Reset to 0 by @clearVolatile@: on switching
-- out and on fainting.
data Boosts = Boosts
  { bsAtk :: !Int
  , bsDef :: !Int
  , bsSpa :: !Int
  , bsSpd :: !Int
  , bsSpe :: !Int
  , bsAccuracy :: !Int
  , bsEvasion :: !Int
  }
  deriving (Eq, Show)

-- | All stages 0.
noBoosts :: Boosts
noBoosts = Boosts 0 0 0 0 0 0 0

-- | The stage of one stat.
boostOf :: BoostStat -> Boosts -> Int
boostOf s b = case s of
  BAtk -> bsAtk b
  BDef -> bsDef b
  BSpa -> bsSpa b
  BSpd -> bsSpd b
  BSpe -> bsSpe b
  BAccuracy -> bsAccuracy b
  BEvasion -> bsEvasion b
{-# INLINE boostOf #-}

-- | Set the stage of one stat.
setBoostOf :: BoostStat -> Int -> Boosts -> Boosts
setBoostOf s v b = case s of
  BAtk -> b {bsAtk = v}
  BDef -> b {bsDef = v}
  BSpa -> b {bsSpa = v}
  BSpd -> b {bsSpd = v}
  BSpe -> b {bsSpe = v}
  BAccuracy -> b {bsAccuracy = v}
  BEvasion -> b {bsEvasion = v}

-- | A move slot (Showdown @moveSlots@ entry).
data MoveSlot = MoveSlot
  { msMove :: !Int
  , msPP :: !Int
  , msMaxPP :: !Int
  , msDisabled :: !Bool
  -- ^ @moveSlot.disabled@ (true; never @'hidden'@). Set by @DisableMove@ handlers at @endTurn@. The slot object is
  -- shared with @baseMoveSlots@, so @clearVolatile@ does not reset it: a benched Pokemon keeps its flags until it
  -- is active at an @endTurn@.
  }
  deriving (Eq, Show)

-- | @pokemon.status@: none, the five major statuses of slice 1 (sleep is not in the slice), and the @fnt@
-- marker Showdown sets in @checkFainted@.
data MonStatus = StatusNone | StatusFainted | StatusBrn | StatusPar | StatusPsn | StatusTox | StatusFrz
  deriving (Eq, Show)

-- | Showdown's @EffectState@ (the object an effect keeps its data in), as one generic record.
-- A slot that Showdown's object does not have holds -1 ("key absent"); the meaning of 'esA' and 'esB' belongs
-- to the effect (toxic's @stage@ is the status state's 'esA').
data EffectState = EffectState
  { esOrder :: !Int
  -- ^ @effectOrder@ (@battle.initEffectState@, battle.ts 3381): the value of 'bEffectOrder' when the state was created
  -- for an id and a target that is a side or an active Pokemon, else 0.
  , esDuration :: !Int
  -- ^ @duration@; 0 = none.
  , esA :: !Int
  , esB :: !Int
  , esHasTarget :: !Bool
  -- ^ Whether @effectState.target@ is set, for a field state (weather, terrain, pseudo-weather): it has none when
  -- created (field.ts 70, 141, 200) and @runEvent@ sets it to the Field when it runs one of its callbacks
  -- (battle.ts 908-909); @clearEffectState@ keeps it (3394). It changes the in-place check of @fieldEvent@
  -- (battle.ts 548-561). Pokemon and side states are created with their target, and do not read this.
  }
  deriving (Eq, Show)

-- | A fresh @initEffectState({})@: no duration, no keys.
noEffectState :: EffectState
noEffectState = EffectState 0 0 (-1) (-1) False

-- | An entry of @pokemon.volatiles@: the condition id (a 'Engine.Event.CondId' index) and its state. The vector
-- keeps insertion order, as a JavaScript object does.
data Volatile = Volatile !Int !EffectState
  deriving (Eq, Show)

-- | An entry of @side.sideConditions@ or @field.pseudoWeather@: the field-condition id (an index into the @Dex@
-- field-condition table) and its state. The vector keeps insertion order, as a JavaScript object does. Layers (Spikes,
-- Toxic Spikes) are the state's 'esA', -1 when the key is absent.
data CondEntry = CondEntry !Int !EffectState
  deriving (Eq, Show)

-- | No weather, terrain: the id of "no condition".
noCond :: Int
noCond = -1

-- | Showdown's @battle.field@: the weather and terrain with their states (battle.ts @field@, field.ts), and the
-- pseudo-weathers in insertion order. 'noCond' ids mean none (Showdown's @''@).
data Field = Field
  { fWeather :: !Int
  , fWeatherState :: !EffectState
  , fTerrain :: !Int
  , fTerrainState :: !EffectState
  , fPseudo :: !(SV.Vector CondEntry)
  }
  deriving (Eq, Show)

-- | An empty field.
noField :: Field
noField = Field noCond noEffectState noCond noEffectState SV.empty

-- | @pokemon.item === ''@.
noItem :: Int
noItem = -1

-- | One Pokemon.
data Mon = Mon
  { monSpecies :: !Int
  , monLevel :: !Int
  , monGender :: !Gender
  , monMaxHp :: !Int
  , monHp :: !Int
  , monStats :: !Stats
  , monBoosts :: !Boosts
  , monType1 :: !Int
  , monType2 :: !Int
  -- ^ -1 when single-typed. Showdown's stored @pokemon.types@: nothing changes them yet. Read types through
  -- 'Engine.Damage.monTypes' (@getTypes()@), never from these.
  , monTeraType :: !Int
  -- ^ @pokemon.teraType@: the set's @teraType@, or the species' first type (pokemon.ts 473). A type id; Stellar is one.
  , monTerastallized :: !Bool
  -- ^ @pokemon.terastallized@ is set (to 'monTeraType'). Deleted when the Pokemon faints (battle.ts 2617); a switch
  -- keeps it.
  , monStellarBoosted :: !Int
  -- ^ @pokemon.stellarBoostedTypes@ (pokemon.ts 285) as a bit set over type ids: the types a Stellar Pokemon's
  -- boost has been spent on.
  , monMoves :: !(SV.Vector MoveSlot)
  , monSpeed :: !Int
  -- ^ Showdown's cached @pokemon.speed@ (refreshed only by @updateSpeed@ / @setSpecies@).
  , monFainted :: !Bool
  , monFaintQueued :: !Bool
  , monActive :: !Bool
  -- ^ @isActive@.
  , monSwitchFlag :: !SwitchFlag
  -- ^ @pokemon.switchFlag@ (pokemon.ts 162: @ID | boolean@).
  , monForceSwitchFlag :: !Bool
  -- ^ @pokemon.forceSwitchFlag@ (pokemon.ts 163): set on the target of a @forceSwitch@ move by @spreadMoveHit@ step 6
  -- (battle-actions.ts 1418-1420), consumed by the action tail's phazing step (battle.ts 2866-2873) in the same action, so it is
  -- never set at a decision point. Red Card (slice 6c) sets it too.
  , monStatus :: !MonStatus
  -- ^ @pokemon.status@: 'StatusFainted' (@fnt@) from @checkFainted@ until the Pokemon is replaced. A Pokemon
  -- that faints keeps its status until then (@faint@ does not touch it).
  , monStatusState :: !EffectState
  -- ^ @pokemon.statusState@. Its 'esA' is @stage@ (toxic's counter), or -1 when the key is absent. @setStatus@
  -- builds a fresh state (absent), toxic's @onStart@ sets the stage to 0, its @onSwitchIn@ resets it to 0 and each
  -- residual increments it. @fnt@ and the @status = ''@ of a replaced faint are plain assignments that leave
  -- @statusState@ alone, so a Pokemon that fainted badly poisoned keeps its stage.
  , monAbility :: !Int
  -- ^ An index into the @Dex@ ability table (@pokemon.ability@).
  , monBaseAbility :: !Int
  -- ^ @pokemon.baseAbility@ (pokemon.ts 442): the set's ability; the Tera forme change sets it to the forme's ability
  -- (1511, unconditionally). @clearVolatile@ restores 'monAbility' from it on a switch-out and on a faint (1544).
  , monSetAbility :: !Int
  -- ^ @toID(set.ability)@, which never changes. The faint path's formeRegression branch resets 'monBaseAbility' to it
  -- (battle.ts 2607-2611), before @clearVolatile@.
  , monAbilityState :: !EffectState
  , monItem :: !Int
  -- ^ An index into the @Dex@ item table, or 'noItem'.
  , monItemState :: !EffectState
  , monVolatiles :: !(SV.Vector Volatile)
  -- ^ @pokemon.volatiles@ in insertion order (flinch, duration 1: set by a flinch secondary, removed by the next
  -- @Residual@ and by @clearVolatile@).
  -- slice 7c
  , monLastMove :: !Int
  -- ^ @pokemon.lastMove@ (pokemon.ts 190): the 'Engine.Dex' move index of the move last used, -1 for @null@. Set by @moveUsed@ in
  -- @runMove@ once the move got past @BeforeMove@ and the PP check (a Struggle sets it too), cleared by @clearVolatile@ (a switch-out
  -- and a faint). Encore reads it.
  , monActiveTurns :: !Int
  -- ^ @pokemon.activeTurns@ (pokemon.ts 264): 0 when the Pokemon switches in (battle-actions.ts 157), +1 at each @endTurn@ while it
  -- is on the field and not fainted (battle.ts 1782). Taunt reads it.
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
  , sConds :: !(SV.Vector CondEntry)
  -- ^ @side.sideConditions@ in insertion order (hazards, screens, Tailwind).
  , sTeraUsed :: !Bool
  -- ^ A member has terastallized, so every member's @canTerastallize@ is null (battle-actions.ts 2004-2006).
  -- @canTerastallize@ itself is not stored: it is @teraType@ unless this is set (battle-actions.ts 1982-1988; no
  -- gen9ou Pokemon can hold a Z-crystal or Mega Evolve).
  }
  deriving (Eq, Show)

-- | Showdown's @battle.requestState@.
data RequestState = ReqNone | ReqTeamPreview | ReqMove | ReqSwitch
  deriving (Eq, Show)

-- | Outcome of the battle.
data Result = Ongoing | Won !Player | Tied
  deriving (Eq, Show)

-- | @pokemon.switchFlag@: whether the Pokemon is to be replaced at the next switch request. Showdown's value is @true@ after a
-- faint (@checkFainted@, battle.ts 2546) or an item, and the move's id after a self-switch (battle-actions.ts 1372); Eject Button,
-- Eject Pack and Emergency Exit test @=== true@ (slice 6c). Nothing else reads the id: @queue.resolveAction@ turns it into the
-- switch's @sourceEffect@ (battle-queue.ts 251), which only a string @selfSwitch@ (Baton Pass, Shed Tail: not supported) uses.
data SwitchFlag
  = NoSwitch
  | -- | @true@: a faint (and, in 6c, an item or ability).
    SwitchForced
  | -- | the move's id: a @selfSwitch@ move (U-turn, Flip Turn, Volt Switch, Teleport, Parting Shot, Chilly Reception).
    SwitchMove
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
  | KTerastallize
  | KPriorityChargeMove
  -- ^ @priorityChargeMove@ (order 107): a move's @priorityChargeCallback@, queued ahead of the move (battle-queue.ts 242-247).
  | KMove
  | KResidual
  deriving (Eq, Show)

-- | A queued action with Showdown's sort key (order, priority, speed; subOrder and effectOrder are 0). Priority is
-- scaled by 10 and speed by 2, so that fractional values stay exact integers.
data Action = Action
  { acKind :: !ActionKind
  , acOrder :: !Int
  , acPriority :: !Int
  -- ^ Scaled by 10 (fractional priority is +-0.1).
  , acSpeed :: !Int
  -- ^ Scaled by 2 (see 'Engine.Event.speedScale').
  , acActor :: !(Maybe Actor)
  -- ^ @action.pokemon@.
  , acArg :: !Int
  -- ^ Move slot (-1 = Struggle) for 'KMove'; target orig for switches; team index for 'KTeam'.
  }
  deriving (Eq, Show)

-- | The whole battle.
-- | The move in use, as @battle.activeMove@ and @battle.activePokemon@ (battle.ts 165, 375-387): the user, and the move's
-- @ignoreAbility@, which 'Engine.Damage.suppressingAbility' reads (Mold Breaker and its kin). The rest of the active move travels
-- as the move-in-use copy of 'Engine.Dex.MoveData'. Transient: it is set where Showdown calls @setActiveMove@ and cleared where
-- it calls @clearActiveMove@, so it is 'Nothing' at every decision point.
data MoveInUse = MoveInUse
  { muUser :: !Actor
  , muIgnoreAbility :: !Bool
  }
  deriving (Eq, Show)

data Battle = Battle
  { bFormat :: !Format
  , bTurn :: !Int
  , bRequest :: !RequestState
  , bResult :: !Result
  , bMidTurn :: !Bool
  , bQueue :: !(SV.Vector Action)
  , bFaintQueue :: !(SV.Vector Actor)
  , bActiveMove :: !(Maybe MoveInUse)
  -- ^ @battle.activeMove@ with @battle.activePokemon@: transient, 'Nothing' at every decision point (a property checks it).
  , bSpeedOrder :: !(U.Vector Int)
  -- ^ @battle.speedOrder@: the field position values of the Pokemon in play, in speed order, as of the last
  -- @runSwitch@ (battle-actions.ts 236). Empty before the first one (Showdown's is @[]@ too).
  , bField :: !Field
  , bEffectOrder :: !Int
  -- ^ @battle.effectOrder@: the counter of @initEffectState@ (battle.ts 3381-3392). It increments where a state is
  -- created with an id and a target that is a side or an active Pokemon: side conditions, statuses, volatiles, and
  -- the ability and item states of a switch-in.
  , bP1 :: !Side
  , bP2 :: !Side
  }
  deriving (Eq, Show)

-- | @battle.initEffectState@'s counter: the value for the new state, and the battle with the counter advanced.
takeEffectOrder :: Battle -> (Int, Battle)
takeEffectOrder b = (bEffectOrder b, b {bEffectOrder = bEffectOrder b + 1})

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

-- | @queue.willAct()@ (battle-queue.ts 315-322): the first queued @move@, @switch@ or @instaswitch@ action (@shift@ is
-- doubles-only). The running action has already left 'bQueue' (@queue.shift()@ in @battle.go@), as in Showdown, so
-- @Nothing@ means the running action is the last of the turn's actions.
willAct :: Battle -> Maybe Action
willAct b = SV.find (\ac -> acKind ac == KMove || acKind ac == KSwitch || acKind ac == KInstaSwitch) (bQueue b)

-- | @queue.willMove(pokemon)@ (battle-queue.ts 324-332): @Nothing@ for a fainted Pokemon, else its first queued @move@
-- action. A 'KPriorityChargeMove' is Showdown's @priorityChargeMove@ choice, not @move@, so it does not count.
willMove :: Battle -> Actor -> Maybe Action
willMove b a
  | monFainted (monOf b a) = Nothing
  | otherwise = SV.find (\ac -> acKind ac == KMove && acActor ac == Just a) (bQueue b)
