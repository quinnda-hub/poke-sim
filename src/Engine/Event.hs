{-# LANGUAGE OverloadedStrings #-}
-- | The vocabulary of the event dispatcher (docs/handlers.md sections 1-3): events, effects, handler metadata
-- and handler bodies, as plain data. The dispatcher itself is "Engine.Dispatch".
module Engine.Event
  ( -- * Events
    EventId (..)
  , SortKind (..)
  , eventSort
  , eventPrefixed
  , eventFastExit
  , eventName
  , Prefix (..)
  , prefixName
  , parseHandlerName
    -- * Effects
  , CondId (..)
  , condName
  , EffectKind (..)
  , ownPrefix
  , EffectKey (..)
    -- * Handlers
  , HandlerSpec (..)
  , Body (..)
  , HandlerRule (..)
  , RuleCond (..)
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
  , compareRedirect
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
  | EvBasePower
  | EvModifyAtk
  | EvModifySpA
  | EvModifyDef
  | EvModifySpD
  | EvModifyDamage
  | EvAfterMoveSecondarySelf
  | EvDisableMove
  | EvUpdate
  | EvBeforeTurn
  | EvRestart
  -- ^ @SideRestart@ / @FieldRestart@ (the handler is @onSideRestart@, prefix 'PSide').
  | EvPrepareHit
  | EvTryHitSide
  | EvTryHitField
  | EvEnd
  -- ^ @SideEnd@ / @FieldEnd@ (the handler is @onSideEnd@ / @onFieldEnd@).
  | EvDurationCallback
  -- ^ Not an event: the export lists a condition's @durationCallback@ as a handler, and the engine keeps it as the
  -- handler @(POwn, EvDurationCallback)@, run where side.ts @addSideCondition@ and field.ts @addPseudoWeather@ call it.
  | EvWeather
  -- ^ @Weather@ (@eachEvent('Weather')@ from a weather's @onFieldResidual@): the handler @onWeather@ of sandstorm.
  | EvWeatherChange
  | EvWeatherModifyDamage
  -- ^ @priorityEvent('WeatherModifyDamage', attacker, defender, move, damage)@ in @modifyDamage@.
  | EvImmunity
  -- ^ @runEvent('Immunity', pokemon, null, null, type)@ in @runStatusImmunity@; the relay value is the type or status key.
  | EvSetWeather
  deriving (Eq, Show, Enum, Bounded)

-- | How @runEvent@ orders the handlers of an event (battle.ts 797-803).
data SortKind
  = -- | @speedSort@: @comparePriority@, with tie runs shuffled by random draws.
    SpeedSort
  | -- | @compareLeftToRightOrder@: a stable sort, never draws.
    LeftToRight
  | -- | @compareRedirectOrder@ (@priorityEvent@, @fastExit@): a stable sort, never draws.
    RedirectSort
  deriving (Eq, Show)

-- | @['Invulnerability', 'TryHit', 'DamagingHit', 'EntryHazard'].includes(eventid)@ sorts left to right; every
-- other event is a @speedSort@. (@fastExit@ / @compareRedirectOrder@ is a flag of @priorityEvent@ for
-- @RedirectTarget@ and @ModifyTarget@, which are not events here yet.)
eventSort :: EventId -> SortKind
eventSort e = case e of
  EvDamagingHit -> LeftToRight
  EvWeatherModifyDamage -> RedirectSort
  _ -> SpeedSort

-- | @priorityEvent@ is @runEvent(..., fastExit = true)@: the event ends at the first handler that returns a value (not
-- @undefined@), and its handlers sort with @compareRedirectOrder@ (battle.ts 927-934, 797-803). Only
-- @WeatherModifyDamage@ is one here.
eventFastExit :: EventId -> Bool
eventFastExit e = e == EvWeatherModifyDamage

-- | Whether the @onAlly@, @onFoe@, @onAny@ and @onSource@ handlers of the event take part
-- (battle.ts @findEventHandlers@ 1055: not for @BeforeTurn@, @Update@, @Weather@, @WeatherChange@,
-- @TerrainChange@).
eventPrefixed :: EventId -> Bool
eventPrefixed e = case e of
  EvBeforeTurn -> False
  EvUpdate -> False
  EvWeather -> False
  EvWeatherChange -> False
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
  EvBasePower -> "BasePower"
  EvModifyAtk -> "ModifyAtk"
  EvModifySpA -> "ModifySpA"
  EvModifyDef -> "ModifyDef"
  EvModifySpD -> "ModifySpD"
  EvModifyDamage -> "ModifyDamage"
  EvAfterMoveSecondarySelf -> "AfterMoveSecondarySelf"
  EvDisableMove -> "DisableMove"
  EvUpdate -> "Update"
  EvBeforeTurn -> "BeforeTurn"
  EvRestart -> "Restart"
  EvPrepareHit -> "PrepareHit"
  EvTryHitSide -> "TryHitSide"
  EvTryHitField -> "TryHitField"
  EvEnd -> "End"
  EvDurationCallback -> "DurationCallback"
  EvWeather -> "Weather"
  EvWeatherChange -> "WeatherChange"
  EvWeatherModifyDamage -> "WeatherModifyDamage"
  EvImmunity -> "Immunity"
  EvSetWeather -> "SetWeather"

-- | Which Pokemon a handler reacts to: the holder (@onX@), its allies (@onAllyX@), its foes (@onFoeX@), the
-- source of the event (@onSourceX@) or anyone (@onAnyX@).
--
-- 'PSide' and 'PField' are the callbacks a side condition or a field condition keeps for itself:
-- @onSide<Event>@ and @onField<Event>@ (battle.ts @fieldEvent@ collects them with @findSideEventHandlers(side,
-- 'onSide' + eventid)@ and @findFieldEventHandlers(field, 'onField' + eventid)@, and @singleEvent('SideStart', ...)@
-- reads @onSideStart@). The plain @on<Event>@ of a side or field condition ('POwn') is what @runEvent@ and the
-- per-Pokemon collection of @fieldEvent@ (hazards' @onSwitchIn@) run.
data Prefix = POwn | PAlly | PFoe | PSource | PAny | PSide | PField
  deriving (Eq, Show, Enum, Bounded)

prefixName :: Prefix -> T.Text
prefixName p = case p of
  POwn -> ""
  PAlly -> "Ally"
  PFoe -> "Foe"
  PSource -> "Source"
  PAny -> "Any"
  PSide -> "Side"
  PField -> "Field"

-- | Split a handler name of the export (@onBeforeMove@, @onFoeBeforeMove@) into its prefix and event, when the
-- engine knows the event.
parseHandlerName :: T.Text -> Maybe (Prefix, EventId)
parseHandlerName "durationCallback" = Just (POwn, EvDurationCallback)
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

-- | What kind of effect holds a handler. Showdown's other kinds (slot conditions, the move itself) join when their
-- holders exist in the state.
data EffectKind
  = EffStatus
  | EffVolatile
  | EffAbility
  | EffItem
  | -- | A side condition (@side.sideConditions@): hazards, screens, Tailwind.
    EffSideCond
  | EffWeather
  | EffTerrain
  | -- | @field.pseudoWeather@: Trick Room, Gravity, ...
    EffPseudoWeather
  deriving (Eq, Show)

-- | The prefix of the callbacks an effect of this kind keeps for itself, as @singleEvent@ reads them: a side
-- condition's @SideStart@ is its @onSideStart@, a field condition's @FieldStart@ its @onFieldStart@.
ownPrefix :: EffectKind -> Prefix
ownPrefix k = case k of
  EffSideCond -> PSide
  EffWeather -> PField
  EffTerrain -> PField
  EffPseudoWeather -> PField
  _ -> POwn

-- | An effect: its kind and its id (a 'CondId' index for statuses and volatiles; an index into the @Dex@
-- ability, item or field-condition table for the others).
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

-- | The vocabulary of data rules for handlers: @chainModify(n, d)@ of the event's modifier when every
-- condition holds (docs/handlers.md section 3). A rule kind or condition kind exists only when at least three
-- whitelisted effects are expressed by it exactly; the justification is in "Engine.Effects".
data HandlerRule = RChainModify !Int !Int ![RuleCond]
  deriving (Eq, Show)

-- | A condition of a rule, on the event's move or the handler's holder.
data RuleCond
  = -- | @move.type === T@ (the type id).
    CMoveType !Int
  | -- | @holder.hp <= holder.maxhp * n / d@.
    CHolderHpAtMost !Int !Int
  | -- | The effective weather is one of these field-condition ids (@pokemon.effectiveWeather()@ or
    -- @field.isWeather(...)@: swiftswim, chlorophyll, sandrush, slushrush).
    CWeather ![Int]
  deriving (Eq, Show)

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
data Relay = RNone | RBool !Bool | RInt !Int | RText !T.Text
  -- ^ 'RText' is a type or status key (@Immunity@: @'frz'@, @'sandstorm'@).
  deriving (Eq, Show)

-- | Showdown's truthiness of a relay value: @false@ and 0 are falsy.
relayTruthy :: Relay -> Bool
relayTruthy r = case r of
  RBool False -> False
  RInt 0 -> False
  RText t | T.null t -> False
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
  | NTechnician
  | NWiseGlasses
  | NExpertBelt
  | NFilterLike
  | NLeftovers
  | NBlackSludge
  | NLifeOrbRecoil
  | NRockyHelmet
  | NFlameOrb
  | NToxicOrb
  | NAssaultVestDisable
  | NStealthRockSwitchIn
  | NLayerStart
  -- ^ @this.effectState.layers = 1@ (spikes, toxicspikes @onSideStart@).
  | NSpikesRestart
  | NToxicSpikesRestart
  | NSpikesSwitchIn
  | NToxicSpikesSwitchIn
  | NStickyWebSwitchIn
  | NToxicDebris
  | NLightClayDuration
  -- ^ reflect, lightscreen @durationCallback@: 8 with a Light Clay user, else 5.
  | NTailwindDuration
  | NTrickRoomDuration
  | NReflectDamage
  | NLightScreenDamage
  | NTrickRoomRestart
  | NWeatherDuration
  -- ^ A weather's @durationCallback@: 8 turns when the setter holds its rock (heatrock, damprock, smoothrock, icyrock).
  | NWeatherResidual
  -- ^ @onFieldResidual@: a log line, then @eachEvent('Weather')@.
  | NWeatherResidualIfActive
  -- ^ The same when @field.isWeather(own id)@ (sandstorm, snowscape).
  | NSandstormWeather
  -- ^ sandstorm @onWeather@: chip damage of @baseMaxhp / 16@ behind the weather immunity.
  | NSandstormSpD
  | NSnowscapeDef
  | NSunnyDayDamage
  | NRainDanceDamage
  | NSunnyDayImmunity
  | NSandRushImmunity
  | NHydration
  | NDrought
  | NDrizzle
  | NSandStream
  | NSnowWarning
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

-- | @battle.compareRedirectOrder@ (battle.ts 420): priority descending, speed descending, then (both holders being
-- Pokemon) the order of their ability states, which is not modelled: its only other user is @RedirectTarget@, and a
-- @WeatherModifyDamage@ list holds one handler.
-- LATENT: the ability-state tie-break of @compareRedirectOrder@.
compareRedirect :: SortKey -> SortKey -> Ordering
compareRedirect a b = compare (skPriority b) (skPriority a) <> compare (skSpeed b) (skSpeed a)
