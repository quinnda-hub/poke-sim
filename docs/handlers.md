# Handler system design

Status: **accepted**, 2026-11-25. These were decided with the owner:

- dispatch is generic;
- handler bodies are hybrid: data rules where they fit, named Haskell handlers
  where they don't;
- effect state is one generic record;
- a new rule kind needs at least three whitelisted users;
- slice 2 starts with a port that changes no behaviour (2a).

Not built yet. Line numbers are in the pinned Showdown under `local/showdown/`.

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
   and speed keys, `bSpeedOrder` and `bEffectOrder`. Parity must stay at
   1400/1400 with no re-recording. That makes it a strong check of the
   dispatcher itself.
2. **2b, the first data rules.** Add the modifier and residual items and
   abilities that need no field state: the type-boosting items, Life Orb,
   Expert Belt, Leftovers, Black Sludge, Rocky Helmet, the status orbs,
   Assault Vest, the pinch abilities, Technician and Huge Power. Record new
   traces with abilities and items.
3. **Then the field-state slice** (weather, terrain, hazards, screens), which
   makes `effectOrder` live for hazards. Choice items wait for the choice-lock
   volatile, and Heavy-Duty Boots waits for hazards.
