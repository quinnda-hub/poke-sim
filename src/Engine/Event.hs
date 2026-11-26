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
  | EvTryHit
  -- ^ @runEvent('TryHit', targets, pokemon, move)@ (battle-actions.ts @hitStepTryHitEvent@): step 1 of @trySpreadMoveHit@.
  | EvSetStatus
  -- ^ @runEvent('SetStatus', pokemon, source, effect, status)@ in @setStatus@; the relay value is the status id.
  | EvTryAddVolatile
  -- ^ @runEvent('TryAddVolatile', pokemon, source, effect, status)@ in @addVolatile@; the relay value is the volatile id.
  | EvTerrainChange
  | EvUseItem
  -- ^ @runEvent('UseItem', pokemon, null, null, item)@ in @useItem@ (pokemon.ts 1844).
  | EvAfterUseItem
  -- ^ @runEvent('AfterUseItem', pokemon, null, null, item)@ in @useItem@ (1867): Unburden.
  | EvTakeItem
  -- ^ @runEvent('TakeItem', pokemon, source, null, item)@ in @takeItem@ (pokemon.ts 1878): Booster Energy (Paradox holders keep
  -- it) and Unburden (adds its volatile). Also @singleEvent('TakeItem', item, ...)@ on the item alone (Knock Off's @BasePower@,
  -- Trick, Switcheroo, Thief).
  | EvModifyType
  -- ^ @singleEvent('ModifyType', move, ...)@ then @runEvent('ModifyType', pokemon, target, move, move)@ in @useMoveInner@
  -- (battle-actions.ts 494, 497). Only a move's own handler is supported.
  | EvEffectiveness
  -- ^ @singleEvent('Effectiveness', move, null, pokemon, type, move, typeMod)@ then @runEvent('Effectiveness', ...)@ in
  -- @runEffectiveness@ (pokemon.ts 2237-2238), once per defending type. Only a move's own handler is supported.
  | EvModifyPriority
  -- ^ @singleEvent('ModifyPriority', move, ...)@ then @runEvent('ModifyPriority', ...)@ in @getActionSpeed@ (battle.ts
  -- 2699-2700). Only a move's own handler is supported.
  | EvBasePowerCallback
  -- ^ Not an event: a move's @basePowerCallback@, called directly by @getDamage@ (battle-actions.ts 1676). The export
  -- lists it as the handler @basePowerCallback@ and the engine keeps it as @(POwn, EvBasePowerCallback)@, as it does
  -- @durationCallback@.
  | EvDamageCallback
  -- ^ Not an event: a move's @damageCallback@, called directly by @getDamage@ (battle-actions.ts 1665).
  | EvHit
  -- ^ @singleEvent('Hit', moveData, {}, target, source, move)@ in @runMoveEffects@ (battle-actions.ts 1338), then
  -- @runEvent('Hit', target, source, move)@ (1342). Only a move's own @onHit@ is supported.
  | EvType
  -- ^ @runEvent('Type', pokemon, null, null, pokemon.types)@ in @getTypes@ (pokemon.ts 2165): every read of a Pokemon's
  -- types. Only the Roost volatile listens to it; 'Engine.Damage.monTypes' is the pure form of the event.
  | EvTry
  -- ^ @singleEvent('Try', move, null, pokemon, target, move)@ in @tryMoveHit@ (battle-actions.ts 889) and @trySpreadMoveHit@
  -- (656): the move's own @onTry@, whose falsy result fails the move (Aurora Veil). Only a move's own handler is supported.
  | EvAfterHit
  -- ^ @singleEvent('AfterHit', moveData, {}, target, pokemon, move)@ in @spreadMoveHit@ (battle-actions.ts 1186), once per
  -- damaged target after @DamagingHit@, while the user has HP. Only a move's own handler is supported.
  | EvAfterSubDamage
  -- ^ The move's @onAfterSubDamage@: Showdown runs it where a hit lands on a Substitute (@singleEvent('AfterSubDamage', ...)@ in
  -- substitute's @onTryPrimaryHit@, data/moves.ts 18375). Substitute is not supported, so it is never run; it is an event only so
  -- that the handlers of Rapid Spin and its kin parse.
  | EvTryImmunity
  -- ^ @singleEvent('TryImmunity', move, {}, target, pokemon, move)@ in @hitStepTryImmunity@ (battle-actions.ts 739): the move's
  -- own @onTryImmunity@ (Trick, Switcheroo), whose falsy result makes the target immune. Only a move's own handler is supported.
  | EvHitField
  -- ^ @singleEvent('HitField', moveData, {}, target, source, move)@ in @runMoveEffects@ (battle-actions.ts 1328-1331), for a
  -- move with target @all@. Only a move's own handler is supported.
  | EvAfterTerastallization
  -- ^ @runEvent('AfterTerastallization', pokemon)@ at the end of @terastallize@ (battle-actions.ts 2024). No supported effect
  -- listens to it, but the event point keeps its (draw-free, with fewer than two handlers) sort.
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
  EvTryHit -> LeftToRight
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
  EvTerrainChange -> False
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
  EvTryHit -> "TryHit"
  EvSetStatus -> "SetStatus"
  EvTryAddVolatile -> "TryAddVolatile"
  EvTerrainChange -> "TerrainChange"
  EvUseItem -> "UseItem"
  EvAfterUseItem -> "AfterUseItem"
  EvTakeItem -> "TakeItem"
  EvTryImmunity -> "TryImmunity"
  EvModifyType -> "ModifyType"
  EvEffectiveness -> "Effectiveness"
  EvModifyPriority -> "ModifyPriority"
  EvBasePowerCallback -> "BasePowerCallback"
  EvDamageCallback -> "DamageCallback"
  EvHit -> "Hit"
  EvType -> "Type"
  EvTry -> "Try"
  EvAfterHit -> "AfterHit"
  EvAfterSubDamage -> "AfterSubDamage"
  EvHitField -> "HitField"
  EvAfterTerastallization -> "AfterTerastallization"

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
parseHandlerName "basePowerCallback" = Just (POwn, EvBasePowerCallback)
parseHandlerName "damageCallback" = Just (POwn, EvDamageCallback)
parseHandlerName n = case T.stripPrefix "on" n of
  Nothing -> Nothing
  Just rest -> case [(p, e) | p <- [minBound .. maxBound], e <- [minBound .. maxBound], prefixName p <> eventName e == rest] of
    (x : _) -> Just x
    [] -> Nothing

-- ---------------------------------------------------------------------------
-- Effects

-- | The conditions the engine supports: the statuses of slice 1 and the flinch volatile. The index
-- ('fromEnum') is the id used in the state and in the @Dex@ condition table.
data CondId = CBrn | CPar | CPsn | CTox | CFrz | CFlinch | CProtosynthesis | CQuarkDrive | CUnburden | CRoost
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
  CProtosynthesis -> "protosynthesis"
  CQuarkDrive -> "quarkdrive"
  CUnburden -> "unburden"
  CRoost -> "roost"

-- | What kind of effect holds a handler. Showdown's other kinds (slot conditions, the move itself) join when their
-- holders exist in the state. A move is one for @onEffect@ events.
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
  | -- | A move, as the effect of a @runEvent@ with @onEffect@ (@BasePower@): its state is @initEffectState({})@, which
    -- has no id, so it is not in the state and the key's id is unused (the handler carries the move's spec).
    EffMove
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
data HandlerRule
  = RChainModify !Int !Int ![RuleCond]
  | -- | @pokemon.useItem()@ when every condition holds (the seeds: @onStart@ and @onTerrainChange@ under their terrain).
    RUseItem ![RuleCond]
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
  | -- | @this.field.isTerrain(...)@: the terrain is one of these field-condition ids (the seeds).
    CTerrain ![Int]
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
  | NEmbodyAspectTeal
  -- ^ embodyaspectteal @onStart@: +1 Speed once per ability state, for a terastallized Ogerpon-Teal-Tera.
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
  | NTerrainDuration
  -- ^ A terrain's @durationCallback@: 8 turns when the setter holds Terrain Extender, else 5.
  | NElectricTerrainSetStatus
  | NElectricTerrainTryAddVolatile
  | NElectricTerrainPower
  | NGrassyTerrainPower
  | NGrassyTerrainHeal
  | NMistyTerrainSetStatus
  | NMistyTerrainTryAddVolatile
  | NMistyTerrainPower
  | NPsychicTerrainTryHit
  | NPsychicTerrainPower
  | NOvercoatImmunity
  | NOvercoatTryHit
  | NLeafGuardSetStatus
  | NLeafGuardTryAddVolatile
  | NElectricSurge
  | NGrassySurge
  | NPsychicSurge
  | NMistySurge
  | NParadoxStart
  -- ^ protosynthesis, quarkdrive @condition.onStart@: @fromBooster@ when the effect is Booster Energy, @bestStat@.
  | NParadoxAtk
  | NParadoxDef
  | NParadoxSpA
  | NParadoxSpD
  | NParadoxSpe
  | NUnburdenSpe
  | NProtosynthesisStart
  | NProtosynthesisWeatherChange
  | NProtosynthesisEnd
  | NQuarkDriveStart
  | NQuarkDriveTerrainChange
  | NQuarkDriveEnd
  | NUnburdenAfterUseItem
  | NUnburdenEnd
  | NBoosterTakeItem
  -- ^ boosterenergy @onTakeItem@: a Paradox species cannot lose it.
  | NUnburdenTakeItem
  -- ^ unburden @onTakeItem@: adds the volatile.
  | NKnockOffPower
  -- ^ knockoff @onBasePower@: 1.5 when the target holds an item that can be taken.
  | NKnockOffAfterHit
  -- ^ knockoff @onAfterHit@: the target loses its item.
  | NTrickTryImmunity
  -- ^ trick, switcheroo @onTryImmunity@: not against Sticky Hold.
  | NTrickHit
  -- ^ trick, switcheroo @onHit@: the two Pokemon swap items.
  | NThiefAfterHit
  -- ^ thief @onAfterHit@: the user takes the target's item when it has none.
  | NPoltergeistTry
  -- ^ poltergeist @onTry@: fails against a target with no item.
  | NBoosterEnergyStart
  | NBoosterEnergyUpdate
  -- Moves' own handlers (slice 4a). Each is named for the move and the callback; a body shared by several moves is
  -- named for what it does.
  | NWeightPower
  -- ^ lowkick, grassknot @basePowerCallback@: the target's weight bands.
  | NHeavySlamPower
  | NHexPower
  | NStoredPower
  -- ^ storedpower, powertrip @basePowerCallback@: @move.basePower + 20 * pokemon.positiveBoosts()@.
  | NAcrobaticsPower
  | NRisingVoltagePower
  | NDynamaxTryHit
  -- ^ lowkick, grassknot, heavyslam @onTryHit@: fails against a Dynamaxed target; nothing is ever Dynamaxed here.
  | NFacadePower
  | NPsybladePower
  | NExpandingForcePower
  | NExpandingForceModifyMove
  -- ^ @move.target = 'allAdjacentFoes'@ under Psychic Terrain: the same single foe in singles.
  | NFickleBeamPower
  | NRainAccuracyModifyMove
  -- ^ thunder, hurricane @onModifyMove@: accuracy @true@ in rain, 50 in sun.
  | NBlizzardModifyMove
  | NBleakwindModifyMove
  | NWeatherBallType
  | NWeatherBallModifyMove
  | NTerrainPulseType
  | NTerrainPulseModifyMove
  | NIvyCudgelType
  -- ^ @onModifyType@ by the Pokemon's species name (Ogerpon's forms, which are not species here).
  | NFreezeDryEffectiveness
  | NFlyingPressEffectiveness
  | NGrassyGlidePriority
  | NHalfHpDamage
  -- ^ ruination, superfang @damageCallback@: @clampIntRange(floor(target.hp / 2), 1)@.
  | NVenoshockPower
  -- ^ venoshock, barbbarrage @onBasePower@: a poisoned target doubles the power.
  | NBrinePower
  -- Moves' own handlers (slice 4b).
  | NWeatherHeal
  -- ^ moonlight, synthesis, morningsun @onHit@: heal @modify(maxhp, factor)@, the factor by the weather.
  | NShoreUpHeal
  | NHealCureStatus
  -- ^ junglehealing, lunarblessing @onHit@: heal a quarter, then @cureStatus()@.
  | NStrengthSapHit
  | NPainSplitHit
  | NRoostStart
  -- ^ roost @condition.onStart@: false for a terastallized target (data/moves.ts 15449-15453), so the volatile is not added.
  | NRoostType
  -- ^ roost @condition.onType@: the Pokemon's types without Flying, applied by 'Engine.Damage.monTypes'.
  -- Moves' own handlers (slice 4c).
  | NHazardSweepAfterHit
  -- ^ rapidspin, mortalspin @onAfterHit@: the user's side loses its entry hazards.
  | NIceSpinnerAfterHit
  -- ^ icespinner @onAfterHit@: @this.field.clearTerrain()@.
  | NCeaselessEdgeAfterHit
  -- ^ ceaselessedge @onAfterHit@: Spikes on the foe's side.
  | NStoneAxeAfterHit
  -- ^ stoneaxe @onAfterHit@: Stealth Rock on the foe's side.
  | NInertAfterSubDamage
  -- ^ The @onAfterSubDamage@ of the moves above: it runs only where a hit lands on a Substitute, which is not modelled.
  | NDefogHit
  | NTidyUpHit
  | NClearSmogHit
  | NCourtChangeHitField
  | NHazeHitField
  | NScreenBreakTryHit
  -- ^ brickbreak, psychicfangs, ragingbull @onTryHit@: the target's side loses Reflect, Light Screen and Aurora Veil.
  | NRagingBullType
  -- ^ @onModifyType@ by the Pokemon's species name (Tauros's Paldean forms, which are not species here).
  | NAuroraVeilTry
  -- ^ auroraveil @onTry@: fails unless the weather is snow.
  | NAuroraVeilDamage
  -- ^ auroraveil @condition.onAnyModifyDamage@.
  deriving (Eq, Show)

-- | The body of each handler the engine implements (written by hand: Showdown's handlers are code). A condition
-- is supported only when every handler the export lists for it has a body here.
-- Source: data/conditions.ts, @brn@ (lines 2-19), @par@ (20-47), @frz@ (92-122), @psn@ (123-137), @tox@
-- (138-163) and @flinch@ (198-206); and the @condition@ blocks of the abilities protosynthesis, quarkdrive and
-- unburden (data/abilities.ts); and the @condition@ block of the move roost (data/moves.ts 15442-15463).
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
  (CProtosynthesis, EvStart) -> Just NParadoxStart
  (CProtosynthesis, EvEnd) -> Just NLogOnly
  (CQuarkDrive, EvStart) -> Just NParadoxStart
  (CQuarkDrive, EvEnd) -> Just NLogOnly
  (CUnburden, EvModifySpe) -> Just NUnburdenSpe
  -- roost: onStart logs (LATENT: it returns false for a Terastallized Pokemon; Tera is not modelled), onType removes Flying
  (CRoost, EvStart) -> Just NRoostStart
  (CRoost, EvType) -> Just NRoostType
  (_, EvModifyAtk) | paradox c -> Just NParadoxAtk
  (_, EvModifyDef) | paradox c -> Just NParadoxDef
  (_, EvModifySpA) | paradox c -> Just NParadoxSpA
  (_, EvModifySpD) | paradox c -> Just NParadoxSpD
  (_, EvModifySpe) | paradox c -> Just NParadoxSpe
  _ -> Nothing
nativeBody _ _ _ = Nothing

-- | The two Paradox abilities' volatiles share their bodies.
paradox :: CondId -> Bool
paradox c = c == CProtosynthesis || c == CQuarkDrive

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
