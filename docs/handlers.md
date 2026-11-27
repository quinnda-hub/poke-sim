# Handler system design

Status: **accepted**, 2026-11-25. These were decided with the owner:

- dispatch is generic;
- handler bodies are hybrid: data rules where they fit, named Haskell handlers
  where they don't;
- effect state is one generic record;
- a new rule kind needs at least three whitelisted users;
- slice 2 starts with a port that changes no behaviour (2a).

Built: 2a (the dispatcher, with statuses and flinch ported onto it) and 2b
(the first abilities and items). Line numbers are in the pinned Showdown under
`local/showdown/`.

## Why it matters for parity

Showdown runs abilities, items, statuses, volatiles, side and slot conditions,
weather, terrain and the move's own callbacks through one event system. Before
it runs the handlers of an event it sorts them, and a tie in the sort is
shuffled with random draws (`sim/battle.ts` `speedSort`, 434-466). So the
engine has to collect the same handlers, compute the same sort keys and sort
them the same way. A handler that is missed, or a key that is wrong, does more
than change one effect: it moves every draw after it.

Slice 1 placed the `BeforeMove` and `Residual` handlers by hand. That works for
three statuses and does not scale.

## 1. Events

`EventId` is a closed enumeration of the Showdown event names the engine uses,
such as `BasePower`, `ModifyAtk`, `ModifyDamage`, `Residual`, `SwitchIn`,
`Start`, `End`, `DamagingHit`, `BeforeMove` and `TryHit`. An event is added only
when a whitelisted mechanic needs it, like a rule kind.

Each event has a static description, taken from `sim/battle.ts`:

- **Sort kind.**
  - `SpeedSort`: shuffles ties and draws. This is the default.
  - `LeftToRight`: `Invulnerability`, `TryHit`, `DamagingHit` and
    `EntryHazard` use `compareLeftToRightOrder` and never draw (797-803).
  - `Redirect`: `priorityEvent` (`fastExit`) uses `compareRedirectOrder`, a
    stable sort that never draws.
- **Prefixed handlers.** Whether `onAlly`, `onFoe`, `onAny` and `onSource`
  handlers take part. They don't for `BeforeTurn`, `Update`, `Weather`,
  `WeatherChange` and `TerrainChange` (1055).
- **Relay type.** The type of the value threaded through the handlers (see
  section 4).

## 2. Effects and their metadata

An effect is a kind plus an integer id:

```haskell
data EffectKind = EStatus | EVolatile | EAbility | EItem | ESideCond | ESlotCond
                | EWeather | ETerrain | EPseudoWeather | EMove
data EffectKey = EffectKey !EffectKind !Int
```

The static table is in the `Dex` and is built when it loads. For each effect,
each `(Prefix, EventId)` it handles maps to:

```haskell
data HandlerSpec = HandlerSpec
  { hsOrder    :: !Int    -- on<Event>Order, or maxBound when absent ("|| 4294967296")
  , hsPriority :: !Int    -- on<Event>Priority, default 0
  , hsSubOrder :: !Int    -- on<Event>SubOrder, else the effectTypeOrder fallback (973-1011)
  , hsBody     :: !Body
  }
data Prefix = POwn | PAlly | PFoe | PSource | PAny
```

- **Where the metadata comes from.** `Order`, `Priority` and `SubOrder` come
  from the exported `data/gen9/*.json`, which already carries them (for
  example `onResidualOrder: 5` on Leftovers and `onModifyAtkPriority: 1` on
  Choice Band). Only the bodies are written by hand.
- **Which handlers an effect has.** Also from the export (`handlers`). A
  constant-valued handler like Lagging Tail's `onFractionalPriority: -0.1`
  becomes a constant body.
- **Support rule.** An effect is supported only when every handler the export
  lists for it has a body. This is the same total, conservative rule moves
  follow.

## 3. Bodies: data rules or named handlers

```haskell
data Body = BRule !HandlerRule     -- interpreted; plain data
          | BNative !NativeId      -- a named Haskell function, chosen from a closed enumeration
          | BConst !Relay          -- a constant (Lagging Tail)
```

`NativeId` is a closed enumeration, and its functions live in an engine table.
The state never holds a function.

- **The rule vocabulary for handlers** covers the patterns most whitelist
  usage depends on:
  - Modifiers. `chainModify n d` of a stat, base power or damage, under a
    condition:
    - move type, category or flag;
    - user or target status;
    - holder HP at or below a fraction;
    - super-effective;
    - holder is the user or the target.

    Examples: type-boosting items, Choice Band and Specs, Life Orb's damage,
    Expert Belt, the pinch abilities, Technician, Huge Power, Assault Vest's
    SpD.
  - Residual heal or damage by a fraction of max HP, with a type condition
    (Leftovers, Black Sludge).
  - Damage to an attacker that makes contact (Rocky Helmet).
  - Inflicting a status on the holder (the status orbs).
  - Setting weather or terrain on start, which runs the field's own rules.
- **Choosing between a rule and a native handler.** A body is a rule when the
  existing vocabulary expresses Showdown's handler exactly. A new rule kind is
  added only when at least three whitelisted effects need it. Anything else is
  a native handler. The count of native handlers is the measure of whether the
  vocabulary works (architecture decision 6). This criterion is what
  implementers follow without asking.

## 4. The dispatcher

```haskell
runEvent    :: EventId -> Target -> Maybe Actor -> SourceEffect -> Relay -> Eng m Relay
singleEvent :: EventId -> EffectKey -> Holder -> ... -> Eng m Relay
fieldEvent  :: EventId -> Eng m ()          -- Residual, SwitchIn
eachEvent   :: EventId -> Eng m ()          -- Update
```

Each mirrors its Showdown function step by step:

1. **Collect** in `findEventHandlers` order (1049-1116).
   - The target's own handlers come first: status, then volatiles in
     insertion order, then ability, item, species and slot conditions (1111).
     JavaScript objects keep insertion order, so the state must keep volatiles
     and side conditions in the order they were added.
   - Then ally and foe prefixed handlers, the source's `onSource`, the side,
     the field, and the format.
   - An inactive target with no active source contributes nothing (1070).
   - `runEvent` with `onEffect` puts the source effect's own callback first
     (787-795). A move's callbacks run this way.
2. **Keys**, as `resolvePriority` sets them (957-1032):
   - order, priority and subOrder from the spec;
   - speed is the holder's `speed`, and 0 for side and field holders;
   - `effectOrder` only for `SwitchIn` and `RedirectTarget`;
   - for `SwitchIn`, the fractional speed-order adjustment (see section 6).
3. **Sort** by the event's sort kind. Under `SpeedSort` the selection sort
   draws n-1 times for each tie run of length n, and never for fewer than two
   handlers.
4. **Run.** The gates are checked when a handler runs, not when it is
   collected:
   - a status that is no longer the holder's;
   - `ignoringAbility` (Neutralizing Gas, Gastro Acid);
   - `ignoringItem` (Klutz, Embargo, Magic Room);
   - Mold Breaker suppression;
   - Air Lock.

   The relay value is threaded through. A falsy return stops the event. For
   an integer relay, the accumulated modifier is applied at the end (919-922).

`Relay` is a small closed sum: `RInt !Int | RBool !Bool | RNone`, plus
constructors that later events need, such as a type or a move id. Bodies
return `Continue | Replace Relay | Stop`.

**Performance.** Collecting from at most about 20 holders, each a table index,
is cheap. If benchmarks show it matters, a per-Pokémon bitmask of the events
its effects handle can skip whole holders later. That would change no draw.

## 5. Modifier arithmetic

Modifiers are integers in units of 1/4096, as Showdown computes them
(2321-2356):

- `chain a b = (a * b + 2048) shiftR 12`
- `chainModify (n, d)`: the next factor is `trunc(n * 4096 / d)`
- `modify v m = trunc((trunc(v * m) + 2047) / 4096)`
- `finalModify` applies the accumulated modifier, then resets it to 1.

Rules write their fractions as integer pairs, for example 1.3 as (13, 10). A
hedgehog property checks the integer versions against the JavaScript
expressions. This resolves the latent "modifiers applied singly" difference.

## 6. Priority and speed keys

Keys are scaled integers, so the arithmetic stays exact:

- **Priority × 10.** Fractional priority is ±0.1 (Quick Claw, Custap Berry,
  Lagging Tail, Stall, Mycelium Might; `battle-queue.ts` 249,
  `battle.ts` ~2701).
- **Speed × 2.** At switch-in, `speed -= speedOrder.indexOf(pos) / 2` in
  singles (1027-1030).
- **`bSpeedOrder` joins the state.** It is set in `runSwitch` (Showdown
  `battle-actions.ts` 236).

This resolves the latent "switch-in speed order computed and discarded" and
"integer priority" differences.

## 7. State additions

All plain strict data:

- **Per Pokémon:**
  - an ability id and an `EffectState`;
  - an item id and an `EffectState`;
  - the existing status, plus a status `EffectState`;
  - volatiles as a small ordered vector of `(id, EffectState)`;
  - `lastItem` and `usedItemThisTurn`.
- **Per side:** side conditions as an ordered vector of `(id, EffectState)`,
  and slot conditions.
- **Field:** weather, terrain and pseudo-weathers, each with its state.
- **Battle:** `bEffectOrder`, the counter of `initEffectState` (3381-3392).
  Ability and item states created at switch-in take a value from it, because
  `isActive` is already set (`battle-actions.ts` 155, 185-186).

```haskell
data EffectState = EffectState
  { esOrder    :: !Int   -- effectOrder
  , esDuration :: !Int   -- 0 = none
  , esA, esB   :: !Int   -- effect-specific slots (layers, counter, stage, chosen stat, locked move, ...)
  }
```

One generic record with named accessors per effect keeps the state flat,
comparable and hashable. This was chosen over a sum type with one constructor
per effect, which would be more typed but would grow with every effect.
`monFlinch` and `monStatusStage` from slice 1 fold into this record.

## 8. What moves onto the dispatcher

- **Slice 1's hand-placed handlers.** Statuses become conditions with
  `BeforeMove`, `Residual`, `ModifySpe`, `SwitchIn` and damage handlers. Flinch
  becomes a volatile. The `fieldEvent` and `beforeMove` code in `Turn.hs`
  becomes generic calls.
- **Moves' own callbacks** (`onTry`, `onHit`, `onAfterHit`,
  `basePowerCallback`, `onModifyMove`, `condition.*`) become handlers of the
  `EMove` effect, run with `onEffect`. The `MoveRule` pipeline slots stay as
  they are.

## 9. Testing

- `engine-pool` also prints abilities and items. The slice generator draws
  them from the pool, where today every set has "No Ability" and no item.
- The parity pool check extends to abilities and items, with the same subset
  and union rule.
- The parity test compares the new state fields (ability, item, volatiles,
  side conditions, field). It stops checking that they hold defaults.
- Format rules that act during battle, such as gen9ou's Sleep Clause Mod and
  the endless battle clause, become live once their triggers exist. The
  generator does not validate sets, so legality is not in question, only
  battle-time rules.

## Proposed first slice (slice 2)

1. **2a, the dispatcher with no change in behaviour.** Build the dispatcher,
   then port slice 1's statuses and flinch onto it, plus the scaled priority
   and speed keys and `bSpeedOrder`. Parity must stay at 1400/1400 with no
   re-recording. That makes it a strong check of the dispatcher itself.
   `bEffectOrder` waits for the field-state slice. Before entry hazards no
   trace can observe it, and untested ordering code is worse than none.
2. **2b, the first data rules.** Add the modifier and residual items and
   abilities that need no field state: the type-boosting items, Life Orb,
   Expert Belt, Leftovers, Black Sludge, Rocky Helmet, the status orbs,
   Assault Vest, the pinch abilities, Technician and Huge Power. Record new
   traces with abilities and items.

   As built, 2b also covers these:
   - Wise Glasses, Pure Power, Swarm, Filter, Solid Rock and Prism Armor;
   - the six type items that are off the whitelist.

   Those that are off the whitelist reuse existing rules and natives and
   count toward no rule kind. The rule-bar table is in
   `src/Engine/Effects.hs`.
3. **Then the field-state slice** (weather, terrain, hazards, screens), which
   makes `effectOrder` live for hazards. Choice items wait for the choice-lock
   volatile, and Heavy-Duty Boots waits for hazards. See "Field state (slice
   3)" below.

## Field state (slice 3)

Decided 2026-11-25. In scope: weather, terrain, entry hazards, screens,
Tailwind and Trick Room. Removing hazards needs moves' own handler code
(`onAfterHit`, `onHit`), which comes in the next slice. That slice also takes
Aurora Veil (its move has an `onTry`), the screen breakers, and the moves that
depend on weather through `onModifyMove`.

Line numbers are in the pinned `local/showdown/sim/`.

### Holders and their sort keys

`resolvePriority` (battle.ts 957-1018) gives each holder its keys as follows:

| Holder | State | `effectOrder` (initEffectState, 3381-3392) | subOrder fallback | Speed |
|---|---|---|---|---|
| Side condition | `side.sideConditions[id]`, created with `target: side` (side.ts 428) | taken from the counter | 4 | 0, or the custom holder's speed |
| Weather | `field.weatherState`, no target (field.ts 70) | 0 | 5 (effect type Weather) | 0, or the custom holder's |
| Terrain | `field.terrainState`, no target (field.ts 141) | 0 | **0**: Terrain is not in the table | 0, or the custom holder's |
| Pseudo-weather | `field.pseudoWeather[id]`, no target (field.ts 200) | 0 | **2** while the state has no target, **5** once it has one (see below) | 0, or the custom holder's |

- **Custom holder.** `fieldEvent` collects side and field handlers a second
  time for each active Pokémon with that Pokémon as `customHolder`, with
  `on<Event>`, no duration key and no `end` (battle.ts 506-508). Those
  handlers sort at the Pokémon's speed, including the `SwitchIn` adjustment.
  That is how hazards run: gen 9 has no `EntryHazard` event (battle-actions.ts
  195), so each hazard is a side condition with `onSwitchIn`.
- **Owned callbacks.** `fieldEvent` also collects the holders' own
  `onSide<Event>` and `onField<Event>` callbacks, with the duration key for
  `Residual` and with `end` (battle.ts 495-498). That is where durations count
  down.
- **Through `runEvent`.** `findEventHandlers` collects side handlers as `on`,
  `onFoe` and `onAny` for the target's own side and the others, then field
  handlers as `on` (battle.ts 1083-1108), all at speed 0.

### A field state gains a target

`runEvent` sets `this.effectState.target = effectHolder` on every callback it
runs (battle.ts 908-909). For a field state the holder is the Field. So after
a weather's, terrain's or pseudo-weather's first callback through `runEvent`,
its state has a Field target, and that changes two things:

- **Sort key.** A pseudo-weather's subOrder fallback becomes 5 (resolvePriority
  995-998). Weather (always 5) and terrain (always 0) are not affected.
- **In-place check.** `fieldEvent` starts checking that the state is still in
  place before running its handler (battle.ts 548-561). Until then, no check is
  made.

Side conditions are unaffected, because their `runEvent` holder is the side,
which is already their target.

The state therefore needs one more bit per field state: whether it has a
target yet. Slice 3's only pseudo-weather, Trick Room, has no `runEvent`
handler, so the sort-key half cannot happen in slice 3. Weather and terrain do
have `runEvent` handlers (Sandstorm's `onModifySpD`, Snowscape's
`onModifyDef`, `WeatherModifyDamage`, the terrains' `onBasePower`, `onSetStatus`
and `onTryHit`). Phases 3c and 3d model the bit: it is set where `runEvent`
would set it, and it gates the in-place check.

**The bit is implemented but unverified.** No slice 3 trace changes when the
engine never sets it. The in-place check can only differ when a weather or
terrain state is replaced while one of its handlers is still queued in the
same `fieldEvent`. No weather or terrain has a `SwitchIn` handler, and nothing
supported sets weather or terrain during `Residual`. A slice that adds either
(a weather or terrain set at end of turn, a field state with a `SwitchIn`
handler) or a pseudo-weather with a `runEvent` handler, which makes the
sort-key half live, must record a case for it.

### Handler argument names

A handler's parameters are named for its own effect, not for the event.
`runEvent(eventid, target, source, ...)` calls each handler as
`(relayVar, target, source, effect)`. `modifyDamage` runs
`runEvent('ModifyDamage', attacker, defender, move, damage)`, so in a screen's
`onAnyModifyDamage(damage, source, target, move)` the parameter called `source`
is the attacker (the event's target), and `target` is the defender (the
event's source). Native handlers map them by position, as Showdown does, not
by name.

### Priority events stop at the first value

`priorityEvent` is `runEvent` with `fastExit` (battle.ts 950-953, 798,
921): the first handler that returns anything other than `undefined` ends the
event, and its value is the result. `WeatherModifyDamage`
(battle-actions.ts 1804) is one. A handler on a priority event therefore
returns a value only when it means to stop the event; `chainModify` returns
`undefined`, so the weathers' damage modifiers all run.

### Effect order

`bEffectOrder` joins the battle state. It increments exactly where
`initEffectState` increments its counter: for a state with an id and a
target, when the target is not a Pokémon or is an active one. That covers:

- side conditions;
- statuses (pokemon.ts 1754);
- volatiles (2021);
- ability and item states at switch-in (battle-actions.ts 185-186) and on
  `setAbility` and `setItem`.

Every site counts, even where only side conditions can observe the value
today (hazards on one side), so later uses such as Lightning Rod's
`RedirectTarget` inherit a correct counter.

### State

These follow section 7:

- **Per side:** an ordered vector of `(condition id, EffectState)` in
  insertion order. Layers go in `esA`.
- **Per field:** the weather id and its state, the terrain id and its state,
  and an ordered vector of pseudo-weathers.
- **Ids** index one table of field conditions, loaded from two sources:
  `conditions.json` (the weathers) and the `condition` blocks of moves that
  carry `sideCondition`, `terrain` or `pseudoWeather`. The handler lists and
  their metadata come from the export.

The parity test compares everything the trace records:
- `field.weather`, `weatherDuration`, `terrain`, `terrainDuration` and
  `pseudoWeather` (duration);
- each side's `sideConditions` (duration and layers).

It stops requiring them to be empty. Slot conditions are not recorded by the
trace format and are out of scope.

### Moves

- **Pipeline keys.** `sideCondition`, `weather`, `terrain` and
  `pseudoWeather` become `MoveRule` fields. They are applied in `moveHit` at
  battle-actions.ts ~1300-1320.
- **Side and field targets.** Moves with target `foeSide`, `allySide` or
  `all` take `tryMoveHit` (battle-actions.ts 573, 883-919): `Try`,
  `PrepareHit`, `TryHitSide` or `TryHitField`, then `moveHit`, with no
  accuracy or crit draw.
- **Condition handlers.** A move's `condition.*` handlers belong to the
  condition, not to the move. The move is supported when the condition is
  supported, meaning every listed handler has a body. Its other handlers
  (`onTry`, `onHit`, ...) still reject it.

### Phases

Each phase passes the full parity run before the next begins, and records
the traces that make its mechanics happen.

1. **3a: holders, effect order, hazards.**
   - the state, the condition table, collection and keys as above;
   - the move keys and the side-target path;
   - Stealth Rock, Spikes, Toxic Spikes, Sticky Web, Heavy-Duty Boots and
     Toxic Debris.
2. **3b: durations.**
   - Reflect, Light Screen, Light Clay, Tailwind and Trick Room: side and field
     residual countdown, `SideEnd` and `FieldEnd`, `durationCallback`, `onAny`;
   - Trick Room's speed in `getActionSpeed`.
3. **3c: weather.**
   - Sun, rain, sand and snow;
   - the setter moves, and the setter abilities, by running an ability's or
     item's `onStart` as its `SwitchIn` handler (getCallback 1025-1036);
   - the rocks, the `Weather` event and its chip damage, `WeatherModifyDamage`,
     the sand SpD and snow Def boosts, and the weather immunities;
   - Swift Swim, Chlorophyll, Sand Rush and Hydration.
4. **3d: terrain.**
   - The four terrains, their setter moves, the surge abilities and Terrain
     Extender;
   - grounding, the status and volatile blocks, Psychic Terrain's `TryHit`,
     Grassy Terrain's heal and base power;
   - Overcoat and Leaf Guard, which need the `TryHit`, `SetStatus` and
     `TryAddVolatile` event points that the terrains add.
   - Unverified until a source exists: Electric Terrain's sleep block and the
     yawn and confusion blocks are implemented, but no accepted move sets sleep,
     yawn or confusion, so no trace reaches them. A slice that accepts one
     must record it against these terrains.
5. **3e: weather- and terrain-driven boosts.**
   - Protosynthesis, Quark Drive, Booster Energy and the seeds;
   - `WeatherChange` and `TerrainChange`, `End`, and item use.

## A move's own handlers (slice 4)

Until slice 4 the rule translation rejects any move whose export lists handler
code, except a move's `condition.*` handlers when the move defines the field
condition. Slice 4 lets a move's own handlers in, on the same terms as an
ability's or item's: the move is an effect, its handlers have bodies in
`src/Engine/Effects.hs`, and a move is accepted only when every handler the
export lists for it has a body and every other key is translated.

### How Showdown calls them

There are three forms, and each call site is mirrored exactly.

- **`singleEvent` on the move.** Most of a move's handlers are called this
  way, with the move as the effect and no sort:
  - in useMove: `ModifyType` and `ModifyMove` (battle-actions.ts 494-495,
    each followed by its `runEvent` at 497-498; 459-467 are the Z-Move and Max
    Move paths), `TryMove` (554), `MoveFail` (595), `AfterMoveSecondarySelf`
    (606), `AfterMoveSecondary` (878) and `AfterMove` (368);
  - in the hit steps: `Try` and `PrepareHit` (656-657, 889-890),
    `TryImmunity` (739), `TryHit`, `TryHitSide` and `TryHitField`
    (1101-1105), `Hit`, `HitSide` and `HitField` (1328-1338), and `AfterHit`
    (1186). The move's own `TryHit` is not paired with the `runEvent` of the
    same name. `runEvent('TryHit')` is step 1 of trySpreadMoveHit (709),
    before accuracy. The move's `singleEvent('TryHit')` runs at the start of
    each `spreadMoveHit`, after accuracy and once per hit;
  - elsewhere: `Effectiveness` (pokemon.ts 2237), inside the type-effectiveness
    calculation, and `ModifyPriority` (battle.ts 2699), where an action's
    priority is computed.

  Most of these are followed by a `runEvent` of the same name for the other
  effects. A `singleEvent` has no sort and draws only if its body does, so a
  site with no supported move handler can be added when the first one needs
  it.
- **`runEvent` with `onEffect`.** `BasePower` (battle-actions.ts 1708),
  `ModifyTarget` (480) and `Damage` (battle.ts 2138) pass `onEffect`. The
  effect's own `on<Event>` callback is added to the collected handlers
  (battle.ts 785-795), and then the whole list is sorted as usual. Its sort
  keys come from resolvePriority:
  - order, priority and subOrder from the move's own
    `on<Event>Order`, `on<Event>Priority` and `on<Event>SubOrder`;
  - a subOrder fallback of 0, since `Move` is not in effectTypeOrder;
  - the speed of the event target, which is its effect holder;
  - effectOrder 0, because `initEffectState({})` has no id and does not
    advance the counter (battle.ts 3381-3391);
  - no `end`.

  For every supported effect the move's handler sorts strictly last. It has
  priority 0 and no order, and every other supported `BasePower` handler has
  an explicit order or a positive priority, so it ties with nothing. The
  property `moveBasePowerSortsLast` checks this. A future effect that breaks
  it makes the sort keys above observable.
- **Direct calls.** `basePowerCallback` (battle-actions.ts 1676),
  `damageCallback` (1665) and `beforeMoveCallback` (329) are plain function
  calls, outside the event system. They parse as `(POwn, Ev...Callback)`
  handlers, as `durationCallback` does, and run at their call site. A
  `damageCallback` returns from getDamage before the crit roll, `BasePower`,
  the random factor and every modifier, so Ruination and Super Fang make no
  draw in getDamage.

### The move in use

Showdown's handlers mutate an `ActiveMove`, a copy made when the move is used,
and later steps read the copy: `move.type`, `move.accuracy`,
`move.basePower`, `move.category`, `move.target` and others. The engine makes
the same copy: a per-use `MoveData` value created when the move is used,
changed only by the handlers Showdown lets change it, and read by every later
step of that use instead of the Dex's. It is never stored in `Battle`, since it
does not survive a decision point, so the state layout is unchanged.

### Data

Low Kick, Grass Knot and Heavy Slam read weights. `species.json` already
exports `weighthg`, so the Dex loads it. `getWeight` is
`max(1, runEvent('ModifyWeight', weighthg))` (pokemon.ts 706-709).

A Pokémon's types are computed when read, as `getTypes` does (pokemon.ts
2160-2168): the Tera type if terastallized, else the `Type` event over the
stored types, then Normal if none are left, then any added type. Every type
read in the engine (STAB, effectiveness, immunity, grounding, `hasType`) goes
through one function, `Engine.Damage.monTypes`, and the stored types are never
rewritten by an effect that only changes the reported types (Roost). The
recorder snapshots `getTypes()`, so the parity test compares `monTypes`.

Bodies that read per-turn history (Payback's `newlySwitched` and
`queue.willMove`, `hurtThisTurn`, `lastDamage`, `attackedBy`) need state the
engine does not hold, and wait for a phase that adds it.

### Phases

1. **4a: the mechanism, and the moves it unlocks on its own.**
   - Base-power callbacks: Low Kick, Grass Knot, Heavy Slam, Hex, Stored
     Power, Acrobatics and Rising Voltage.
   - `onBasePower`: Facade, Psyblade, Expanding Force, Misty Explosion and
     Fickle Beam (its `randomChance` is a draw inside `BasePower`).
   - Accuracy and type: Hurricane, Thunder, Blizzard and Bleakwind Storm
     (accuracy by weather), Weather Ball, Terrain Pulse and Ivy Cudgel (type
     changes), and Freeze-Dry (`Effectiveness`).
   - Grassy Glide (`ModifyPriority`) and Ruination (`damageCallback`).
2. **4b: healing and the stat overrides.** Moonlight, Synthesis, Morning Sun,
   Strength Sap, Pain Split and Roost (its type-changing volatile); Body
   Press, Psyshock, Foul Play, Sacred Sword, Scale Shot and level-based
   damage.
3. **4c: removing and moving field state.** Rapid Spin, Mortal Spin, Ice
   Spinner, Defog, Court Change, Haze, Ceaseless Edge and Stone Axe; Aurora
   Veil; the screen breakers.
   Found while building it:
   - `AfterHit` (battle-actions.ts 1184-1186) runs per hit, after
     `DamagingHit`, only while the user has HP: a Rocky Helmet or Rough Skin
     that has just brought it to 0 stops Rapid Spin's hazard removal.
   - `secondary: {}` (Ceaseless Edge, Stone Axe) is a secondary with no
     effect: it still draws `random(100)` (battle-actions.ts 1403).
   - `side.addSideCondition` with no source takes `battle.event.target`
     (side.ts 418), so a hazard set by an attack has the defender as source.
   - Court Change moves the state objects (data/moves.ts 3070-3093). A side's
     effect orders stay increasing only while every side condition the engine
     has is in its list; a side condition outside the list (none today) would
     stay behind, ahead of arrivals of lower order, and the playout invariant
     would need revisiting.
4. **4d: Knock Off and item removal** (`takeItem`, the `TakeItem` event, which
   makes Booster Energy's and Unburden's `onTakeItem` reachable).
   Found while building it:
   - `singleEvent('TakeItem', item, ...)` in Trick, Switcheroo and Thief
     passes the Pokémon that would receive the item as the handler's
     `source`, so Booster Energy also refuses a Paradox receiver.
   - A failed Trick or Thief restores only the item id
     (`target.item = yourItem.id`): the restored item's state stays cleared.
   - The `Paradox` tag comes from Showdown's species `tags`. In the pinned
     version Gouging Fire, Raging Bolt, Iron Boulder and Iron Crown have no
     tag, so Booster Energy can be taken from them and given to them.
   - The order of Trick's two takes and two sets changes only effect-order
     values that no trace observes; the property `itemMoves` checks it.

Each phase is its own PR, stacked on the one before.

## Terastallization (slice 5)

Designed 2026-11-26 from Showdown 0.11.11. gen9ou allows Terastallization
(its Standard ruleset has no Terastal Clause) and bans Tera Blast, so no
Stellar-type move can appear.

### The choice and the action

- The choice is `MoveTera n` (`move n terastallize`, side.ts 1268-1287). It is
  legal when the active Pokémon's `canTerastallize` is set: its Tera type,
  until any member of its side terastallizes (battle-actions.ts 1982-1988,
  2004-2006). The state keeps that as `sTeraUsed`, not per Pokémon. No gen9ou
  Pokémon holds a Z-crystal or can Mega Evolve, the other two null cases.
- `legalChoices` lists each move's Tera variant right after the move, as
  record.js does (`move 1`, `move 1 terastallize`, `move 2`, ...). A Pokémon
  locked into a move has none, and that includes Struggle: with no usable
  move the request sets `lockedMove = 'struggle'` (pokemon.ts 1124-1126), and
  `canTerastallize` is written only without a locked move (1158-1168).
- The move action brings a `KTerastallize` action, queued before it only
  when the Pokémon is not already terastallized (battle-queue.ts 230-236), at
  order 106 (battle-queue.ts 188): after switches, before every move whatever
  its priority. Two Tera actions sort by speed, and a tie draws.
- `terastallize()` (battle-actions.ts 1990-2026) draws nothing. It sets
  `terastallized`, clears the side's `canTerastallize`, and runs
  `AfterTerastallization`. Then the shared action tail runs, as after every
  action (its `Update` and faint checks).
- A fainted Pokémon loses `terastallized` (battle.ts 2617). A switch keeps it.

### What terastallized changes

- `getTypes` returns `[teraType]` before the `Type` event (pokemon.ts
  2161-2163), so Roost's type change no longer applies. Every type read goes
  through `monTypes` already (slice 4b), so effectiveness, immunities,
  grounding, hazards and status immunities follow.
- STAB (battle-actions.ts 1820-1850) reads both: the current types
  (`hasType`) and the types before Terastallization (`getTypes(false, true)`,
  the stored types with the `Type` event). A Tera type that is also an
  original type gives 2x. `ModifySTAB` runs after (Adaptability, LATENT if
  not supported).
- The 60 base-power floor (battle-actions.ts 1717-1726): a terastallized
  user's move of its Tera type with base power under 60, priority 0 or less,
  not multi-hit and not variable-power, is raised to 60.
- Roost's volatile fails on a terastallized Pokémon (data/moves.ts
  15449-15453). Setting or adding a type also fails (pokemon.ts 2140-2155);
  no supported move does either.
- Ogerpon's Tera changes its forme (battle-actions.ts 2010-2014): Ogerpon
  becomes Ogerpon-Teal-Tera, whose ability is Embody Aspect (Teal). An
  Ogerpon whose Tera type is not Fire, Grass, Rock or Water does not
  terastallize at all; the choice is accepted and nothing happens
  (1991-1995). Morpeko's and Terapagos's branches need forme changes that
  are not supported; base Terapagos terastallizes like any other Pokémon.
  The forme change is a permanent `formeChange` (pokemon.ts 1449-1518): the
  species becomes Ogerpon-Teal-Tera, `setAbility` runs (the old ability's
  `End`, a fresh state, Embody Aspect's `Start`) unless the current ability
  is `cantsuppress` (no supported ability is, so that branch is LATENT), and
  `baseAbility` becomes Embody Aspect either way. A
  fainting Ogerpon reverts to its set's species and ability (battle.ts
  2607-2611). The state keeps `monBaseAbility` and `monSetAbility` for this.

### The trace contract

- A trace recorded with Tera choices offered carries `tera: true`. Without
  it, the parity test compares the recorded legal lists with the engine's
  minus its `MoveTera` choices: the policy was never offered them, though
  Showdown allowed them.
- Every Pokémon's `teraType` and `terastallized` are compared at every
  decision point, in every trace.
- record.js takes an optional `teraChance`: the policy picks among the other
  choices, then takes a move's Tera variant with that probability, so Tera
  happens at varied turns. Without it, scenarios behave exactly as before.

### Phases

1. **5a: Terastallization without Stellar.** Everything above. A team whose
   Tera type is Stellar is rejected at `parseTeam`, as an unsupported input,
   until 5b.
2. **5b: Stellar.** `getTypes` keeps the original types; STAB is 2x for an
   original type and 1.2x (4915/4096) for any other, once per type
   (`stellarBoostedTypes`, kept as `monStellarBoosted`); the 60 floor
   applies to any type not yet boosted. A Stellar-type move against a
   terastallized target (typeMod 1) cannot occur in gen9ou.
   Found while building it:
   - `stellarBoostedTypes` is never cleared: only the constructor sets it
     (pokemon.ts 504), so a switch, a faint and a forme change keep it.
   - `move.stellarBoosted` lives on the move in use, so every hit of a
     multi-hit move keeps the boost. The engine carries it on the per-use
     `MoveData` copy (`mvStellarBoosted`), not in the battle state.
   - Struggle's type `???` is never boosted (the STAB block skips it), so a
     Stellar Pokémon's Struggle always gets the 60 floor.

## Mid-turn switching (slice 6)

Designed 2026-11-26 from Showdown 0.11.11. Baton Pass and Shed Tail are
banned in gen9ou, and Pursuit is a Past move.

### What already works

A request in the middle of a turn needs no new structure. `turnLoop` stops
when an action's tail makes a request and keeps the rest of the queue in
`bQueue`. `commitChoices` sorts only the new choices and puts them before the
saved queue, as Showdown does ("the rest of the turn is saved (and not
re-sorted)", battle.ts commitChoices). A switch chosen at a switch request is
an `instaswitch` (order 3), so it and the `runSwitch` it queues (101) run
before the rest of the turn. record.js and the parity test already handle a
decision point whose turn has not advanced and whose request is `switch`.
What is missing is anything that sets the switch flag from a move.

### Self-switch (6a)

- `selfSwitch: true` becomes a rule field. It has 6 whitelisted users:
  U-turn, Flip Turn, Volt Switch, Teleport, Parting Shot and Chilly Reception.
- `runMoveEffects` (battle-actions.ts 1349-1353) counts the move as having
  done something when the side can switch. At the end of the hits
  (1369-1372), if the move did anything, the user has HP and its side can
  switch, the flag is set. So a user that a Rocky Helmet or Rough Skin has
  just fainted never switches, and a move that fails (Volt Switch into a
  Ground type) sets nothing. Strictly, the flag is set before Rocky Helmet's
  `DamagingHit`, and the faint clears it (`faint()` sets `switchFlag =
  false`, pokemon.ts 1607); the outcome is the same.
- `switchFlag` holds the move id after a self-switch and `true` after a
  faint or an item. Eject Button, Eject Pack and Emergency Exit test
  `=== true`, so the state's flag tells the two apart (`SwitchFlag`).
- Every switch-out runs `runEvent('SwitchOut')` (battle-actions.ts 100). No
  supported effect listens until Regenerator and Natural Cure (6c), but the
  event point goes in now.
- When the flag turns into a request, the action tail runs
  `runPursuitActivation` for each flagged Pokémon with HP (battle.ts
  2954-2958): one `eachEvent('Update')`, which can draw, before the request.
  It sets `skipPursuit`, so the instaswitch's `switchIn` does not run it
  again. The engine derives this from the action kind: a plain switch runs
  it, an instaswitch whose outgoing Pokémon has HP does not. 6b's drags skip
  it too, and 6c's Eject Button and Emergency Exit must keep this true.
- The switched-out Pokémon's queued actions are cancelled
  (`queue.cancelAction`, battle-actions.ts 124-127).
- Own handler code:
  - Teleport's `onTry` fails it when the side cannot switch.
  - Parting Shot's `onHit` deletes `selfSwitch` from the move in use when
    neither drop happened (data/moves.ts 13182). That is a field of the
    per-use `MoveData` copy. With no switch target it still succeeds: an
    `onHit` that returns nothing gives `singleEvent` its relay value `true`
    (battle.ts 658), so the drops stay and the move does not fail.
  - Chilly Reception's `priorityChargeCallback` queues a
    `priorityChargeMove` action at order 107 (battle-queue.ts 189), which
    adds its volatile. The volatile's `onBeforeMove` only logs; its
    `duration: 1` lets the Residual remove it, so it is still in the state
    at a mid-turn switch request. That is a new action kind,
    `KPriorityChargeMove`.
- A fainted Pokémon in the active slot is not `isActive` (battle.ts 2615)
  but carries the faint's flag, so the invariants speak of the active slot.

### Forced switch (6b)

- `forceSwitch: true` becomes a rule field. It has 3 whitelisted users,
  Roar, Whirlwind and Dragon Tail, plus Circle Throw.
- The move sets the target's `forceSwitchFlag` after `runEvent('DragOut')`
  (battle-actions.ts 1413-1428). The action tail then drags in a random
  replacement before anything else (battle.ts 2866-2873).
- `dragIn` (battle-actions.ts 216-227) draws one `random(n)` over the
  benched Pokémon that are not fainted, in roster order (`possibleSwitches`,
  battle.ts 1589-1602). The switch-in runs at once rather than through the
  queue, and skips Pursuit's activation.

### Items and abilities (6c)

- Eject Button (0.036 of usage), Red Card (0.069) and Eject Pack (0.008),
  through the same flags. Emergency Exit and Wimp Out are not whitelisted.
- The SwitchOut listeners: Regenerator (0.34) and Natural Cure (0.03).
