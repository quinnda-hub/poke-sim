# Roadmap

The goal is an agent that plays Smogon 6v6 Gen 9 OU at the highest level. The
order of work is fixed: an exactly correct, fast simulator first, and no bot
work until it exists.

Last updated 2026-11-25.

## Done

- **Reference pinned.** Pokémon Showdown 0.11.11 at commit `c7fa6f1`, format
  `gen9ou`.
- **Oracle tooling.** A trace recorder, a random battle generator for slice 0,
  and a data export, all under `tools/oracle/`. Battles replay deterministically
  with every draw recorded.
- **Data exported** to `data/gen9/` from the pinned build.
- **800 slice-0 battles recorded:** 500 random battles and 300 built to run
  out of PP, so that Struggle, recoil and double knockouts occur.
- **Slice 0 of the new engine core** under `src/Engine/`: team preview, plain
  damaging moves, Struggle and its recoil, switching, fainting, forced
  replacement and win detection. All 800 recorded battles replay with identical
  draws and identical state at 43,933 decision points (101,496 draws).
- **The test was tested.** Thirteen kinds of deliberate corruption of a trace
  are each caught, including a state value, a draw range, a draw result, a
  legal choice, an extra draw, the winner, a malformed draw, a missing side and
  a non-default value in a field the engine does not model.
- **Independent review** of slice 0. Its one blocking finding (Struggle was
  offered as a legal choice but crashed) is fixed, and a second pass found
  nothing blocking.
- **Usage whitelist** for October 2026, from Smogon's `gen9ou` stats at the
  1695 rating cutoff. A species is included at 0.5% weighted usage or more,
  and a move, item, ability or Tera type when it is on at least 1% of a
  whitelisted species' sets. Anything the pinned Showdown's `gen9ou` bans is
  excluded. The result is 102 species, 300 moves, 71 items, 85 abilities and
  19 Tera types. Roaring Moon is excluded as Uber. `whitelist-report` shows
  the engine covers 88 of the species and 25 of the moves, and no abilities,
  items or Terastallization. The 14 uncovered species are all alternate
  formes, which slice 0 leaves out.
- **Slice 1: rule vocabulary, stat stages and status.**
  - `src/Engine/Rule.hs` describes each move as a closed, function-free
    `MoveRule`, organised by Showdown's move pipeline slots. Translation from
    the exported data is total and conservative. Every field is translated or
    listed as inert with a reason, and anything else, including any handler
    code, rejects the move.
  - Effects:
    - drain, recoil, healing, multi-hit (fixed and 2 to 5 hits), and spread
      moves, which reach one foe in singles;
    - stat stages, with crits ignoring them where Showdown does, accuracy and
      evasion stages, and boosted speed;
    - burn, paralysis, poison, Toxic and freeze, flinch, thawing, and the
      `BeforeMove` and `Residual` events.
  - Coverage: the engine accepts 313 moves, 148 of the 300 whitelisted ones,
    or 51% by usage. Sleep is not in the slice, because no move the rules
    accept causes it.
- **Trace pools come from the engine.** `engine-pool` prints what the engine
  accepts, and `gen-slice1.js` records battles from that list, with a coverage
  tally from Showdown's log. Each trace set's pool must be accepted by the
  engine, and together the sets must cover everything it accepts. 600 slice-1
  battles are recorded. Parity checks 1,400 traces, 85,053 decision points and
  223,786 draws.
- **Slice 2a: the event dispatcher.** A generic `runEvent`, `singleEvent`,
  `fieldEvent` and `eachEvent` follow Showdown's handler collection, priority
  keys and speed sort (docs/handlers.md). Statuses and flinch moved onto it
  as conditions, with scaled priority and speed keys and `bSpeedOrder`. No
  behaviour changed: all 1,400 traces replay with no re-recording.
- **Slice 2b: the first abilities and items.**
  - Items: the 18 plain type-boosting items, Wise Glasses, Expert Belt, Life
    Orb, Leftovers, Black Sludge, Rocky Helmet, Flame Orb, Toxic Orb and
    Assault Vest.
  - Abilities: Blaze, Overgrow, Torrent, Swarm, Technician, Huge Power, Pure
    Power, Filter, Solid Rock and Prism Armor.
  - Events: `BasePower`, the four stat events, `ModifyDamage` with its
    `onSource` handlers, `AfterMoveSecondarySelf`, `DisableMove` at the end of
    the turn, and `DamagingHit` for Rocky Helmet.
  - Damage modifiers now chain with Showdown's 4096-unit rounding. The `contact`
    flag is translated, Struggle's included.
  - The handler rule vocabulary has one rule kind (`chainModify`) and two
    condition kinds (move type, holder HP at or below a fraction), each with at
    least three whitelisted users. The justifying table is in
    `src/Engine/Effects.hs`. Everything else is one of 11 native handlers.
  - Some supported effects are off the whitelist: Swarm, Pure Power, Filter,
    Solid Rock, Prism Armor, and six of the type items. They reuse existing
    rules and natives and count toward no rule kind.
  - 900 slice-2 battles are recorded: 600 random and 300 Struggle battles,
    with abilities and items drawn from the engine's pool. Parity checks
    2,300 traces, 135,341 decision points and 354,664 draws. Every accepted
    ability and item appears in some trace's teams.
  - Whitelist coverage: 21 of 71 items (37.1% by weight) and 5 of 85
    abilities (2.4%).
  - The independent review found one blocking defect. Struggle lost its
    contact flag, so Rocky Helmet never triggered on it, and no trace had
    Struggle with items. It is fixed, and the Struggle profile was added to
    cover it.
  - The parity test's long tier had been keeping every decoded trace alive,
    about 10 GB for 2,000 traces, and was killed by the OOM killer on the
    15 GB laptop. It now peaks at about 80 MB.
- **Slice 3: field state**, in five phases (design in docs/handlers.md,
  "Field state (slice 3)").
  - 3a: side and field holders, `effectOrder` (Showdown's `initEffectState`
    counter), and the entry hazards: Stealth Rock, Spikes, Toxic Spikes,
    Sticky Web, Heavy-Duty Boots and Toxic Debris. Side- and field-targeted
    moves take Showdown's `tryMoveHit` path.
  - 3b: durations and their countdown (`durationCallback`, `SideEnd`,
    `FieldEnd`), Reflect, Light Screen, Light Clay, Tailwind and Trick Room.
    Two screens on one side tie in `ModifyDamage` and are shuffled.
  - 3c: sun, rain, sand and snow, set by move or ability; an ability's or
    item's `onStart` runs as its `SwitchIn` handler. The rocks, sand chip and
    its immunities, the weather damage step, the sand and snow stat boosts,
    and the weather speed abilities and Hydration.
  - 3d: the four terrains, the surges and Terrain Extender, grounding, and
    the new `TryHit`, `SetStatus` and `TryAddVolatile` event points, with
    Overcoat, Leaf Guard and the powder flag.
  - 3e: item use (`useItem`), Protosynthesis, Quark Drive, Booster Energy,
    the seeds and Unburden; ability and item `End` at switch-out and faint.
  - 1,500 slice-3 battles are recorded, rebalanced and re-recorded in each
    phase, with a tally from Showdown's own events in which every counter is
    non-zero. Parity checks 3,800 traces, 263,530 decision points and
    579,496 draws. Each phase's deliberate mutations were caught, or shown to
    be unobservable with a reason.
  - The parity test now accepts a trace cut off at the recorder's turn limit:
    it is checked up to the cut, and an empty step must be refused for the
    missing choice. Before, the three such battles would have been skipped.
  - Implemented but unverified, because nothing accepted reaches them: the
    field-state target bit (docs/handlers.md), and Electric and Misty
    Terrain's sleep, Yawn and confusion blocks.
  - Whitelist coverage: 162 of 300 moves (56.7% by weight), 22 of 85
    abilities (30.0%) and 31 of 71 items (68.2%).
  - Independent reviews of 3a and of 3b-3e found nothing blocking. Their
    should-fix findings are fixed: a move-acceptance hole for weather moves
    with unrelated `condition` blocks, and weak trace coverage of switch-in
    priority (now caught by 154 traces, from 2).
- **Slice 4a: a move's own handlers** (design in docs/handlers.md, "A move's
  own handlers (slice 4)").
  - Moves are effects with bodies, on the same support rule as abilities and
    items. Showdown's three call forms are mirrored: `singleEvent` on the
    move, `runEvent` with `onEffect` for `BasePower`, and the direct
    `basePowerCallback` and `damageCallback`. A per-use copy of the move
    carries what `ModifyType` and `ModifyMove` change, and never enters the
    battle state.
  - New event points: `ModifyType`, `ModifyMove` (its result now counts),
    `PrepareHit` on the Pokémon-target path, the move's own `TryHit` per hit,
    `Effectiveness` and `ModifyPriority`. Species weights are loaded.
  - 26 more moves accepted (358 in all): Low Kick, Grass Knot, Heavy Slam,
    Hex, Stored Power, Power Trip, Acrobatics, Rising Voltage, Facade,
    Psyblade, Expanding Force, Fickle Beam, Thunder, Blizzard, Bleakwind
    Storm, Weather Ball, Terrain Pulse, Ivy Cudgel, Freeze-Dry, Flying Press,
    Grassy Glide, Ruination, Super Fang, Venoshock, Barb Barrage and Brine.
    Hurricane waits for confusion, and Misty Explosion for `selfdestruct`.
  - 600 slice-4 battles are recorded. Parity checks 4,400 traces, 311,612
    decision points and 681,577 draws.
- **Slice 4b: healing, Roost and the stat overrides.**
  - Healing moves' own `onHit`: Moonlight, Synthesis, Morning Sun, Shore Up,
    Strength Sap, Pain Split, Jungle Healing and Lunar Blessing. A heal that
    returns `NOT_FAIL` at full HP still counts as a hit, unlike the `heal`
    key.
  - Roost and the `Type` event. Types are computed per read, as Showdown's
    `getTypes` does (pokemon.ts 2160-2168), and every type read goes through
    that one function. The parity test compares it with the recorded
    `getTypes()`; before, it compared the stored types, which was right only
    while nothing changed them.
  - New move-rule fields: offensive and defensive stat overrides, the
    offensive Pokémon override, `ignoreDefensive`, `ignoreEvasion`,
    `selfBoost` (once, after the hit loop) and fixed or level damage.
  - 22 more moves (380 in all), Howl and Life Dew among them through the
    `allies` target. The slice-4 set is re-recorded at 900 battles. Parity
    checks 4,700 traces, 351,543 decision points and 768,002 draws.
- **Slice 4c: removing and moving field state.**
  - Hazard removal: Rapid Spin, Mortal Spin and Ice Spinner (`onAfterHit`,
    gated on the user's HP after `DamagingHit`), Defog and Tidy Up.
  - Court Change (side conditions swap sides as the same states: layers,
    durations and effect order kept), Haze and Clear Smog.
  - Ceaseless Edge and Stone Axe (hazards from an attack), Aurora Veil (needs
    snow; steps aside for Reflect or Light Screen of the move's category),
    Brick Break, Psychic Fangs and Raging Bull (screens break before the
    damage).
  - New event points: `Try`, `AfterHit` and `HitField`, plus a damaging
    move's own `onHit`. 14 more moves (394 in all). The slice-4 set is
    re-recorded at 1,200 battles. Parity checks 5,000 traces, 361,128
    decision points and 737,441 draws.
- **Slice 4d: Knock Off and item removal.**
  - `takeItem` and `setItem` as Showdown runs them, with the `TakeItem`
    event (pokemon.ts 1873-1913). Booster Energy refuses to leave or reach a
    species with the `Paradox` tag, now exported from Showdown's `tags`.
    Unburden activates on every `TakeItem`, even a refused one.
  - Knock Off (its boost asks only the item's own `TakeItem`, and nothing is
    taken if Rocky Helmet has just fainted the user), Trick, Switcheroo,
    Thief and Poltergeist (its `onTry`, at battle-actions.ts 656).
  - New event points: `TakeItem`, `TryImmunity`, and the move's `Try` on
    Pokémon-targeted moves. 5 more moves (399 in all). The slice-4 set is
    re-recorded at 1,500 battles. Parity checks 5,300 traces, 391,683
    decision points and 785,342 draws.
- **Slice 5a: Terastallization without Stellar.**
  - The `MoveTera` choice, legal until the side has terastallized and never
    while locked into a move (Struggle included); its action runs at order
    106, after switches and before every move, and two of them sort by speed.
  - Types become the Tera type, STAB counts the original types too (2x when
    the Tera type is one of them), the 60 base-power floor, Roost failing,
    and Tera cleared on faint but kept through switches.
  - Ogerpon's Tera forme change to Ogerpon-Teal-Tera with Embody Aspect
    (Teal), which re-boosts on every switch-in, and its reversion on faint.
    The state gains `monBaseAbility` and `monSetAbility` for it.
  - The trace contract: a `tera: true` flag, `teraType` and `terastallized`
    compared everywhere, and `teraChance` in the recorder. A Stellar Tera type
    is rejected until 5b.
  - A new set, slice 5, of 1,000 battles with Tera offered. Parity checks
    6,300 traces, 476,038 decision points and 942,236 draws.
- **Slice 5b: Stellar.** A Stellar Pokémon keeps its types; its STAB is 2x
  for an original type and 1.2x for any other, once per type and for every
  hit of that move, and the floor applies to any type not yet boosted. The
  boosted types are never cleared. Slice 5 is re-recorded at 1,200 battles
  with about a quarter of sets Stellar. Parity checks 6,500 traces, 492,063
  decision points and 983,270 draws.
- **Slice 6a: moves that switch the user out.** U-turn, Flip Turn, Volt
  Switch, Teleport, Parting Shot and Chilly Reception (405 moves accepted).
  A move's `selfSwitch` sets a three-way switch flag; the turn pauses for a
  switch request and resumes with the saved queue. New: the `SwitchOut`
  event point and the priority-charge action. A new set, slice 6, of 1,000
  battles. Parity checks 7,500 traces, 594,043 decision points and
  1,147,453 draws.
- **Slice 6b: forced switches.** Roar, Whirlwind, Dragon Tail and Circle
  Throw (409 moves accepted). The target's own force-switch flag, the
  `DragOut` event points, and the phazing step of the action tail, which
  draws one replacement uniformly from the bench (even from one) and
  switches it in at once. Slice 6 is re-recorded at 1,500 battles. Parity
  checks 8,000 traces, 630,135 decision points and 1,198,145 draws.
- **Slice 6c: switch-out abilities and switching items.** Regenerator,
  Natural Cure, Eject Button and Red Card (33 abilities, 42 items). No new
  state or event points: native bodies on `SwitchOut` and
  `AfterMoveSecondary`. Every switch-out now clears both switch flags, as
  `clearVolatile` does, and Life Orb skips its recoil for an attacker that
  Red Card forces out. Slice 6 is re-recorded at 2,000 battles. Parity
  checks 8,500 traces, 688,609 decision points and 1,295,923 draws.
- **Slice 6d: Eject Pack.** Three new event points at Showdown's positions:
  `AfterBoost` (in `boostMon`, now the full `battle.boost` with the capped
  table), the item's `Use` (in `useItem`) and `AfterMove` (after `useMove`).
  The pack's `eject` flag is the item state's `esA`. Step 3 is done. Slice 6
  is re-recorded at 2,500 battles. Parity checks 9,000 traces, 734,495
  decision points and 1,377,962 draws.
- **Test tiers.** The fast tier (`POKESIM_TIER=fast`) replays every 10th trace
  and runs the hedgehog properties at 100 tests each, in about 10 s. The long
  tier, which gates merges into main, replays everything and runs the
  properties at 2000 tests each.

### Benchmark log

Single core, random playouts, `cabal bench`. Numbers are only comparable on the
same machine.

| Date | Slice | Machine | Battles/s | Decision steps/s |
|---|---|---|---|---|
| 2026-11-25 | 0, first working version, not optimised | Core i7-10710U laptop, on battery | 4,001 | 245,865 |
| 2026-11-25 | 0, with Struggle and a fully strict state, not optimised | Core i7-10710U laptop | 3,748 | about 230,000 |

The second row is slower because the state moved from lazy lists to strict
vectors that are copied on update. That was done for correctness of the state
rules, not speed; choosing faster structures is the first optimisation task.

Slice 1 phases 1 and 2 measured 3,648 and 3,569 battles/s on the same laptop.
The benchmark still plays slice-0 teams, so these rows measure overhead added
to the plain path, not the cost of the new mechanics. After phase 3 the laptop
was too thermally noisy to read: the same binary ranged from 1,700 to 2,800
battles/s. Two things are open: a run on the workstation, and benchmark teams
that use the slice-1 pool.

Slice 2a measured about 5% slower than slice 1 (3,377 against 3,552 battles/s,
alternated runs).

**Slice 2b costs about 38% on the plain path.** On 2026-11-25 the laptop was
throttling: one binary ranged from 476 to 787 battles/s. So 2a (`5799461`) and
2b ran concurrently on two pinned cores, swapping cores between runs, on
slice-0 teams. Over four runs, 2b's throughput was 0.616-0.626 of 2a's. On
slice-2 teams 2b played about 250 battles/s in the same throttled state.

The likely cause is the new event points: `BasePower`, `ModifyAtk`/`SpA`/`Def`/`SpD`,
`ModifyDamage`, `AfterMoveSecondarySelf` and `DisableMove`. They collect and
sort handlers on every hit and every turn, even when nothing is attached. Every
Pokémon now also has an ability, so the early exit in handler collection is
gone.

The event points must stay, since Showdown runs them. The owner decided to
optimise later, as its own step: draws and state unchanged, numbers before and
after, measured on the workstation. The per-Pokémon bitmask of handled events
(docs/handlers.md section 4) is the first candidate.

**Slice 3a costs about another 13%.** This was measured on 2026-11-25 the
same way, concurrently on two pinned cores with slice-0 teams, `main`
(`d378a2b`) against 3a. The ratio over four runs was 0.870-0.876. The laptop
was still throttled, at about 220 battles/s for `main`.

The cause is the same: every event now also collects side and field holders,
even when none are attached. Cumulatively, 3a runs at about 0.54 of slice
2a's throughput. The same optimisation step covers it.

Slices 3b-3e were not benchmarked on the laptop. Measure slice 3 as a whole
against `main` on the workstation at the start of the optimisation step.

**Slice 4a costs about 9%** with slice-3 teams: 161.5 battles/s on `main`
(`2fcedaf`) against 146.9 and 146.4 on 4a, run one after the other on the
laptop rather than concurrently on pinned cores. The cost is `runEvent`s that
no supported effect can listen to: `ModifyPriority` on every queue refresh,
and `ModifyType`, `ModifyMove` and `PrepareHit` on every move use. An exact
skip, for an event with no listener anywhere in the Dex, is a candidate for
the optimisation step: an event with no handlers sorts and draws nothing.

**Showdown's own throughput, as the reference.** Measured on 2026-11-26 with
`tools/bench/showdown-bench.js` against `cabal bench` on slice 4b, on the same
teams (the first 20 traces of a set), alternating runs on one pinned laptop
core:

| Teams | Engine steps/s | Showdown steps/s | Engine battles/s | Showdown battles/s |
|---|---|---|---|---|
| slice 3 | 9,200-11,000 | 800-990 | 118-139 | 9.9-12.4 |
| slice 4 | 11,400-11,600 | 1,110-1,120 | 115-117 | 10.6-10.7 |

On 2026-11-26, after slice 5b (Terastallization done), the same method gave:

| Teams | Engine steps/s | Showdown steps/s | Engine battles/s | Showdown battles/s |
|---|---|---|---|---|
| slice 3 | 11,200-11,400 | 1,080 | 147-149 | 13.8 |
| slice 5 | 11,400-11,800 | 860 | 150-156 | 11.2 |

After slice 6a, specialising the engine to its two chance monads and skipping
events no effect in play listens to (no change to draws or state) gave, on
the same core with the 6a build interleaved:

| Teams | 6a steps/s | Now steps/s | 6a battles/s | Now battles/s |
|---|---|---|---|---|
| slice 3 | 10,900 | 31,400-31,700 | 143 | 408-411 |
| slice 5 | 10,300-11,400 | 27,600-32,200 | 136-150 | 371-434 |
| slice 6 | 9,700-9,900 | 27,600-28,800 | 93-94 | 269-281 |

That clears the 12,000 floor; the 50,000 target is still ahead. The profile
puts most of what is left in collecting event handlers (about 65% of the
allocation) and in the engine monad's binds.

After slice 6c, collecting handlers strictly, back to front, and skipping an
effect with no entry for the event and prefix (a mask per prefix) cut the
allocation per decision step by about 31%. Measured with the 6c build
interleaved, on one core:

| Teams | 6c steps/s | Now steps/s | 6c kB/step | Now kB/step | Ratio |
|---|---|---|---|---|---|
| slice 3 | 24,300-30,400 | 28,000-31,600 | 122-123 | 85 | 1.04-1.15 |
| slice 5 | 26,300-26,600 | 29,000-29,700 | 123 | 84-85 | 1.10-1.12 |
| slice 6 | 28,700-33,900 | 31,800-37,400 | 113-114 | 79-80 | 1.10-1.13 |

The handler order is unchanged. The parity test cannot see two reorderings
of it (status and volatiles on one Pokémon, and the two sides' conditions),
because no supported pair of those effects ties in sort order.

**First workstation numbers** (2026-11-27, `banana`, Core Ultra 7 265K, one
P-core pinned, at `e7dd587`, slice 6c with the handler change). The three
rounds per set agree within 1%, where the laptop varies by up to 30%, so
compare builds here from now on.

| Teams | Engine steps/s | Engine battles/s | Showdown steps/s | Multiple |
|---|---|---|---|---|
| slice 3 | 78,100-78,600 | 1,014-1,020 | 3,804 | about 21x |
| slice 5 | 82,100-82,300 | 1,106-1,108 | 3,490 | about 24x |

That clears the 50,000 target on the machine it is defined for. GC takes
3.5% of the time. `perf stat` counts 11.1-11.9 M user instructions per
battle (150-155 k per decision step, including start-up), at 2.2
instructions per cycle. pikapro, a Rust engine with the same parity
contract, publishes 1.58 M per gen9ou battle, about 7x fewer (see
`local/research/pikapro/REPORT.md`). Showdown gains more from the faster
core than the engine does, so the multiple over it is lower than the
laptop's 30x.

At slice 5b the engine was about 10-12 times faster per step than Showdown;
it is now about 30 times faster. Showdown builds a battle in
about 0.9 ms, so its time goes into the turns, at about 1 ms per decision
step: it runs its full event system, logging and request building on every
turn.

## Known gaps in slice 0

- **The 1000-turn tie is implemented but never exercised.** No recorded battle
  runs that long.
- **A true tie is never exercised.** When both sides faint at once Showdown
  still names a winner, and the 46 recorded double knockouts all do.
- **The speed cap of 10,000 is implemented but cannot be reached** at levels up
  to 100.
- **Knockout damage beyond remaining HP is not observable**, so the exact damage
  of a knockout hit is checked only through its draws.
- **Nothing is optimised.** The state is strict but uses copied vectors, and the
  speed sort works on a temporary list.

## Latent differences from Showdown

An independent review found places where the engine is correct for slice 0 but
will be wrong once later mechanics arrive. Each is marked in the code with a
`-- LATENT:` comment. They must be resolved in the slice that makes them live.

| Difference | Becomes live with |
|---|---|
| Showdown breaks ties between `SwitchIn` and `RedirectTarget` handlers by `effectOrder`, the order in which their effects were created (`battle.ts` `resolvePriority`). The engine has no `effectOrder`. Residual handlers do not use it. | Entry hazards (several on one side tie), Lightning Rod |
| Speeds pass through the `ModifySpe` event, but no supported item or ability handles it, and Trick Room needs pseudo-weather state. | Choice Scarf, speed abilities, Tailwind, Trick Room |
| The residual event has status, flinch and item handlers. The field and sides have none yet. | Weather, terrain, screens, any end-of-turn ability |
| `BeforeMove` has only freeze, flinch and paralysis handlers. | Sleep, confusion, Taunt, Disable, Protect |
| Status immunity is by type only. It ignores abilities, Misty Terrain and Safeguard. | Status-blocking abilities, Misty Terrain, Safeguard |
| Multi-accuracy moves check accuracy per hit with a floating-point table, which differs from exact division (accuracy 35 at stage -4 gives 14, not 15). | Triple Axel, Population Bomb |
| Type checks use the species' types; Showdown uses the current types. Ground immunity ignores grounding. | Terastallization, type-changing moves, Levitate, Air Balloon |
| Move priority is the base priority only. | Prankster, Gale Wings, Grassy Glide |
| A move's own callbacks (`onEffect`, such as its `BasePower` callback) are not run. Moves with handler code are rejected. | The first move with handler code |
| An ability's or item's `onStart` is not run as its `SwitchIn` handler (`battle.ts` `getCallback`). | Intimidate, weather-setting abilities |
| An ability's `End` handler on an inactive holder would be skipped; `singleEvent` exempts `End`. | The first `End` event |
| `fieldEvent` checks that an effect is still in place by its id, where Showdown compares the state object. | An effect removed and re-added within one residual |
| Forced switches by the opponent skip a step that voluntary switches run. | Roar, Whirlwind, Dragon Tail |
| A faint that causes another faint while faints are being resolved is not handled. | Destiny Bond and similar |
| The battle state has no ordering or hash, which search will want for a table of seen positions. | The first search code |

## Next: the simulator

Each step is gated by the parity test. See
[oracle-testing.md](oracle-testing.md).

The mechanics come in this order (decided 2026-11-26). Each one gets real
Gen 9 OU battles simulated sooner, and each makes the later ones cheaper.

1. **Finish slice 4: 4d, Knock Off and item removal.** Done 2026-11-26.
2. **Terastallization.** Done 2026-11-26 (slices 5a and 5b). Before it, no
   real OU battle could be simulated. It changed `getTypes`, STAB and the
   request, and added the Tera choice to `legalChoices`.
3. **Moves that switch mid-turn.** Done 2026-11-27 (slices 6a-6d). U-turn (0.73 of usage), Flip Turn,
   Volt Switch, Chilly Reception, and the forced switches of Roar, Whirlwind
   and Dragon Tail. They need a switch request inside the turn, which is a
   change to the turn loop (lead-owned).
4. **Volatile conditions.** Protect (its stall counter is a draw), Substitute,
   Encore and Taunt, and Sucker Punch, which reads the target's queued action.
   Substitute is the first escape hatch: a mechanic that resists the rule
   vocabulary. The Choice items come with the `choicelock` volatile (Choice
   Band, Specs and Scarf are not in the pool yet), and with them Knock Off,
   Trick and Thief against a Choice item, which 4d could not record.
5. **Full support for the catalog.** Every move, ability, item and species
   legal in Gen 9 OU: abilities first (30% of their usage is supported),
   then the remaining items and moves. Transform, Illusion and the forme
   changes are escape hatches. Today 210 of the whitelist's 300 moves are
   accepted (74.7% by usage), 22 of 85 abilities (30.0%), 31 of 71 items
   (68.2%) and 88 of 102 species (85.3%), at slice 4c.

   Beyond the catalog, Gen 9 OU also needs the format's own rules, wherever
   no trace has exercised them yet (its clauses, such as Sleep Clause Mod and
   the Endless Battle Clause). Other singles tiers (Ubers, UU and below) then
   differ mostly in bans. Doubles is a separate body of work (spread moves,
   ally targeting, redirection, Commander) and out of scope.
   **Timeline** (estimated 2026-11-26, from the velocity so far). The new
   engine's first commit was 2026-11-25 12:42. Slices 0 to 4c, about 16
   phases, landed within 25 hours, about one phase per 1.5 hours with an
   implementer running overnight. The work left in steps 1-5 is about 55-75
   phases:

   | Work | Phases |
   |---|---|
   | 4d | 1 |
   | Terastallization | 2-3 |
   | Moves that switch mid-turn | 2-3 |
   | Volatile conditions | 3-4 |
   | Abilities (about 280 left) | 20-25 |
   | Moves (about 290 left that need code) | 15-20 |
   | Items (about 200 left, many with no battle effect) | 5-8 |
   | Transform, Illusion, forme changes, format clauses | 4-6 |
   | Test infrastructure (delta snapshots, a faster long tier) | 2-3 |

   That is 5-6 days at the current pace. The milestones assume the pace
   drops: the long tier and the traces grow with every phase, each commit
   waits for approval, and steps 3-5 change the turn loop and the hit path.

   | Milestone | After | Estimate |
   |---|---|---|
   | Real Gen 9 OU battles end to end | Steps 1-4 | 2026-11-29 to 2026-12-01 |
   | The whole whitelist accepted | Part of step 5 | about 2026-12-03 |
   | Full Gen 9 singles | Step 5 | 2026-12-10 to 2026-12-17 (late December at worst) |

   Compare progress against this table at the end of each slice, and record
   the slip or gain beside it.

   | Date | Done | Against the estimate |
   |---|---|---|
   | 2026-11-26 | Slice 4c | Baseline |
   | 2026-11-26 | Slice 4d | On pace: step 1 done the day it was estimated |
   | 2026-11-26 | Slice 5a | Ahead: most of step 2 done the same day |
   | 2026-11-26 | Slice 5b | Ahead: step 2 done, about 3 days before its milestone window |
   | 2026-11-26 | Slice 6a | Ahead: first of step 3's three phases done the same day |
   | 2026-11-26 | Slice 6b | Ahead: two of step 3's three phases done the same day |
   | 2026-11-26 | Slice 6c | Ahead: step 3 all but Eject Pack (6d) |
   | 2026-11-27 | Slice 6d | Ahead: step 3 done, about 2-4 days before the steps 1-4 milestone window |

6. **Per-player view.** What each side can observe, as a first-class value.
7. **Showdown seed compatibility.** Implement Showdown's own generators so a
   battle can be reproduced from its seed alone.

**Cheaper event points, alongside the mechanics, when the workstation is
free.** Slice 2b costs about 38% on the plain path and 3a about another
13% (see the benchmark log). Optimise with draws and state unchanged,
measured on the workstation.

Throughput is counted in decision steps per second per core (one `step`
resolving a turn or a forced switch), since that is what search and
training consume. Battles per second follow at about 80 steps per battle
with slice-3 teams. Slice 4a runs at about 11,500 steps/s (146 battles/s)
on one laptop core.

| | Steps/s per core | Battles/s per core | Purpose |
|---|---|---|---|
| Floor | 12,000 | 150 | Do not fall below as coverage grows |
| Target | 50,000 | 600 | Comfortable search on one machine |
| Stretch | 100,000 | 1,200 | Deeper search and more sampled worlds |

- **Why these numbers.** Self-play training needs little: about a billion
  steps a week is under 2,000 steps/s in all, and a batched network on the
  GPU is the tighter limit, at about 5,000 steps/s per core across the
  workstation's 20 cores. Test-time search sets the bar. A search of
  10⁶ simulated steps in 5 s on 20 cores needs 10,000 steps/s per core,
  and deeper search, more sampled worlds or a shorter budget multiply that.
- **How to measure.** On the workstation, concurrently on pinned cores
  against a baseline (the benchmark log's method), with teams that use the
  engine's full coverage. Per-step cost rises as mechanics are added, so
  simplified teams flatter it.
- **Reference.** Showdown's own throughput on the same recorded scenarios,
  so that the engine's speed is also stated as a multiple of the
  reference's.

At 9,200-11,600 steps/s on slice-3 and slice-4 teams, the engine is already
at or below the floor, so this is due.

Alongside:

- **Monthly whitelist refresh.** Regenerate the whitelist when each month's
  stats appear and rerun `whitelist-report`. Teams are not yet rejected for
  being outside the whitelist, because the slice 0 traces use species from
  outside it. That check is added once the engine covers the whitelist.
- **Scenario generation from rules.** For each rule, generate battles that
  exercise it with each random outcome forced both ways, and report coverage
  per rule.
- **Trace size.** 9,000 battles take about 5.5 GB. The long tier takes about
  190 s (most of the growth is the slice 6 property playouts) since parity checks traces on several threads (it was 325 s on one,
  and grows with every set). About 60% of a trace's time is decoding its
  JSON, so delta snapshots or a compact cache are the next step before the
  catalog work.
- **Benchmark tracking.** Record throughput at each slice so slowdowns are
  measured when they appear.
- **Retire the legacy simulator** once the new core covers what it does.

## Later: the bot

Not started, and not to be started before the simulator is correct. The
reasoning and sources are in [research.md](research.md).

1. **Open-team search.** Both teams visible; regret matching at each turn; a
   hand-written evaluation. A playable baseline that needs no training.
2. **Belief model.** A prior over opponent sets from usage statistics, updated
   from what the battle reveals.
3. **Self-play policy.** Regularised policy-gradient self-play, sized to one
   workstation.
4. **Test-time search** over worlds sampled from the belief model.
5. **Evaluation** against existing bots, and on the Showdown ladder under its
   bot registration policy.

## Open questions

- How fast the Haskell engine is on the workstation, and after optimisation.
  The only measurement so far is an unoptimised first version on a laptop.
- How much of the mechanics list the rule vocabulary covers before escape
  hatches dominate.
- Whether the reduced-scale training budget is enough for a policy worth
  searching on top of.
- How long Gen 9 OU remains the current metagame.
