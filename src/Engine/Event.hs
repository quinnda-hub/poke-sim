{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The vocabulary of the event dispatcher (docs/handlers.md sections 1-3): events, effects, handler metadata
-- and handler bodies, as plain data. The dispatcher itself is "Engine.Dispatch".
module Engine.Event
  ( -- * Events
    EventId (..)
  , SortKind (..)
  , eventSort
  , eventPrefixed
  , eventName
  , Prefix (..)
  , prefixName
  , parseHandlerName
    -- * Effects
  , CondId (..)
  , condName
  , EffectKind (..)
  , EffectKey (..)
    -- * Handlers
  , HandlerSpec (..)
  , Body (..)
  , HandlerRule
  , NativeId (..)
  , Relay (..)
  , relayTruthy
  , nativeBody
    -- * Sort keys
  , speedScale
  , priorityScale
  , SortKey (..)
  , compareKey
  , compareLeftToRight
  ) where

import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Events

-- | The Showdown events the engine runs. An event is added only when a handler that the engine supports needs it
-- (docs/handlers.md section 1).
data EventId
  = EvBeforeMove
  | EvResidual
  | EvModifySpe
  | EvSwitchIn
  | EvStart
  | EvModifyMove
  | EvDamagingHit
  | EvAfterMoveSecondary
  | EvUpdate
  | EvBeforeTurn
  deriving (Eq, Show, Enum, Bounded)

-- | How @runEvent@ orders the handlers of an event (battle.ts 797-803).
data SortKind
  = -- | @speedSort@: @comparePriority@, with tie runs shuffled by random draws.
    SpeedSort
  | -- | @compareLeftToRightOrder@: a stable sort, never draws.
    LeftToRight
  deriving (Eq, Show)

-- | @['Invulnerability', 'TryHit', 'DamagingHit', 'EntryHazard'].includes(eventid)@ sorts left to right; every
-- other event is a @speedSort@. (@fastExit@ / @compareRedirectOrder@ is a flag of @priorityEvent@ for
-- @RedirectTarget@ and @ModifyTarget@, which are not events here yet.)
eventSort :: EventId -> SortKind
eventSort e = case e of
  EvDamagingHit -> LeftToRight
  _ -> SpeedSort

-- | Whether the @onAlly@, @onFoe@, @onAny@ and @onSource@ handlers of the event take part
-- (battle.ts @findEventHandlers@ 1055: not for @BeforeTurn@, @Update@, @Weather@, @WeatherChange@,
-- @TerrainChange@).
eventPrefixed :: EventId -> Bool
eventPrefixed e = case e of
  EvBeforeTurn -> False
  EvUpdate -> False
  _ -> True

-- | The event's name in Showdown (@on<Name>@ is a handler).
eventName :: EventId -> T.Text
eventName e = case e of
  EvBeforeMove -> "BeforeMove"
  EvResidual -> "Residual"
  EvModifySpe -> "ModifySpe"
  EvSwitchIn -> "SwitchIn"
  EvStart -> "Start"
  EvModifyMove -> "ModifyMove"
  EvDamagingHit -> "DamagingHit"
  EvAfterMoveSecondary -> "AfterMoveSecondary"
  EvUpdate -> "Update"
  EvBeforeTurn -> "BeforeTurn"

-- | Which Pokemon a handler reacts to: the holder (@onX@), its allies (@onAllyX@), its foes (@onFoeX@), the
-- source of the event (@onSourceX@) or anyone (@onAnyX@).
data Prefix = POwn | PAlly | PFoe | PSource | PAny
  deriving (Eq, Show, Enum, Bounded)

prefixName :: Prefix -> T.Text
prefixName p = case p of
  POwn -> ""
  PAlly -> "Ally"
  PFoe -> "Foe"
  PSource -> "Source"
  PAny -> "Any"

-- | Split a handler name of the export (@onBeforeMove@, @onFoeBeforeMove@) into its prefix and event, when the
-- engine knows the event.
parseHandlerName :: T.Text -> Maybe (Prefix, EventId)
parseHandlerName n = case T.stripPrefix "on" n of
  Nothing -> Nothing
  Just rest -> case [(p, e) | p <- [minBound .. maxBound], e <- [minBound .. maxBound], prefixName p <> eventName e == rest] of
    (x : _) -> Just x
    [] -> Nothing

-- ---------------------------------------------------------------------------
-- Effects

-- | The conditions the engine supports: the statuses of slice 1 and the flinch volatile. The index
-- ('fromEnum') is the id used in the state and in the @Dex@ condition table.
data CondId = CBrn | CPar | CPsn | CTox | CFrz | CFlinch
  deriving (Eq, Show, Enum, Bounded)

-- | The key of the condition in @conditions.json@ (data/conditions.ts).
condName :: CondId -> T.Text
condName c = case c of
  CBrn -> "brn"
  CPar -> "par"
  CPsn -> "psn"
  CTox -> "tox"
  CFrz -> "frz"
  CFlinch -> "flinch"

-- | What kind of effect holds a handler. Showdown's other kinds (ability, item, side and slot conditions,
-- weather, terrain, pseudo-weather, the move itself) join when their holders exist in the state.
data EffectKind = EffStatus | EffVolatile
  deriving (Eq, Show)

-- | An effect: its kind and its id (a 'CondId' index for both kinds so far).
data EffectKey = EffectKey !EffectKind !Int
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Handlers

-- | The metadata of one handler (battle.ts @resolvePriority@ 957-1032), from the exported data.
data HandlerSpec = HandlerSpec
  { hsOrder :: !Int
  -- ^ @on<Event>Order@; 0 when absent (the comparison reads 0 as @4294967296@, @order || 4294967296@).
  , hsPriority :: !Int
  -- ^ @on<Event>Priority@ times 'priorityScale'; default 0.
  , hsSubOrder :: !Int
  -- ^ @on<Event>SubOrder@, else the @effectTypeOrder@ fallback (battle.ts 973-1011).
  , hsBody :: !(Maybe Body)
  -- ^ 'Nothing' for the handler Showdown collects with an undefined callback because the effect has a duration
  -- (@getKey = 'duration'@, @Residual@ only): it counts the duration down and does nothing else.
  }
  deriving (Eq, Show)

-- | The vocabulary of data rules for handlers. Empty: a rule kind is added when at least three whitelisted
-- effects need it (docs/handlers.md section 3), and none do yet.
data HandlerRule

instance Eq HandlerRule where
  a == _ = case a of {}

instance Show HandlerRule where
  show a = case a of {}

-- | What a handler does.
data Body
  = BRule !HandlerRule
  | -- | A named Haskell function, run by 'Engine.Dispatch'.
    BNative !NativeId
  | -- | A constant (a non-function @on<Event>@ value, such as Lagging Tail's @onFractionalPriority: -0.1@).
    BConst !Relay
  deriving (Eq, Show)

-- | The value an event threads through its handlers (Showdown's @relayVar@). 'RNone' is the hidden @true@ of an
-- event run without one: it is not passed to the handlers and it is relayTruthy.
data Relay = RNone | RBool !Bool | RInt !Int
  deriving (Eq, Show)

-- | Showdown's truthiness of a relay value: @false@ and 0 are falsy.
relayTruthy :: Relay -> Bool
relayTruthy r = case r of
  RBool False -> False
  RInt 0 -> False
  _ -> True

-- | The handlers written in Haskell. Each is named after the condition and event it implements.
data NativeId
  = NLogOnly
  -- ^ A handler whose only effect is a log line (@this.add(...)@): brn, par, psn and frz @onStart@.
  | NBrnResidual
  | NPsnResidual
  | NToxStart
  | NToxSwitchIn
  | NToxResidual
  | NParModifySpe
  | NParBeforeMove
  | NFrzBeforeMove
  | NFrzModifyMove
  | NFrzAfterMoveSecondary
  | NFrzDamagingHit
  | NFlinchBeforeMove
  deriving (Eq, Show)

-- | The body of each handler the engine implements (written by hand: Showdown's handlers are code). A condition
-- is supported only when every handler the export lists for it has a body here.
-- Source: data/conditions.ts, @brn@ (lines 2-19), @par@ (20-47), @frz@ (92-122), @psn@ (123-137), @tox@
-- (138-163) and @flinch@ (198-206).
nativeBody :: CondId -> Prefix -> EventId -> Maybe Body
nativeBody c POwn e = BNative <$> case (c, e) of
  (CBrn, EvStart) -> Just NLogOnly
  (CBrn, EvResidual) -> Just NBrnResidual
  (CPar, EvStart) -> Just NLogOnly
  (CPar, EvModifySpe) -> Just NParModifySpe
  (CPar, EvBeforeMove) -> Just NParBeforeMove
  (CPsn, EvStart) -> Just NLogOnly
  (CPsn, EvResidual) -> Just NPsnResidual
  (CTox, EvStart) -> Just NToxStart
  (CTox, EvSwitchIn) -> Just NToxSwitchIn
  (CTox, EvResidual) -> Just NToxResidual
  (CFrz, EvStart) -> Just NLogOnly
  (CFrz, EvBeforeMove) -> Just NFrzBeforeMove
  (CFrz, EvModifyMove) -> Just NFrzModifyMove
  (CFrz, EvAfterMoveSecondary) -> Just NFrzAfterMoveSecondary
  (CFrz, EvDamagingHit) -> Just NFrzDamagingHit
  (CFlinch, EvBeforeMove) -> Just NFlinchBeforeMove
  _ -> Nothing
nativeBody _ _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Sort keys

-- | Speeds in sort keys are scaled by 2, so that the half-speed step of @SwitchIn@ (battle.ts 1027-1030,
-- @speed -= speedOrder.indexOf(pos) / (activePerHalf * 2)@, singles) is an integer.
speedScale :: Int
speedScale = 2

-- | Priorities in sort keys are scaled by 10, so that fractional priority (+-0.1: Quick Claw, Custap Berry,
-- Lagging Tail, Stall; battle-queue.ts 249) is an integer.
priorityScale :: Int
priorityScale = 10

-- | Showdown's sort key (battle.ts @comparePriority@ 409): order, priority, speed, subOrder, effectOrder.
data SortKey = SortKey
  { skOrder :: !Int
  , skPriority :: !Int
  , skSpeed :: !Int
  , skSubOrder :: !Int
  , skEffectOrder :: !Int
  }
  deriving (Eq, Show)

-- | @battle.comparePriority@: order ascending (0 = absent, last), priority descending, speed descending, then
-- subOrder and effectOrder ascending. 'LT' means the first goes first.
compareKey :: SortKey -> SortKey -> Ordering
compareKey a b =
  compare (order (skOrder a)) (order (skOrder b))
    <> compare (skPriority b) (skPriority a)
    <> compare (skSpeed b) (skSpeed a)
    <> compare (skSubOrder a) (skSubOrder b)
    <> compare (skEffectOrder a) (skEffectOrder b)
  where
    order = absentLast
{-# INLINE compareKey #-}

absentLast :: Int -> Int
absentLast o = if o == 0 then 4294967296 else o

-- | @battle.compareLeftToRightOrder@: order ascending, priority descending, then the index of the target. Singles
-- has one target (index 0), so the comparison ends at priority and the stable sort keeps collection order.
compareLeftToRight :: SortKey -> SortKey -> Ordering
compareLeftToRight a b =
  compare (absentLast (skOrder a)) (absentLast (skOrder b)) <> compare (skPriority b) (skPriority a)
