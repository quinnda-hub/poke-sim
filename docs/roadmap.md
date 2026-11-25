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
- **Test tiers.** The fast tier (`POKESIM_TIER=fast`) replays every 10th trace
  and runs the hedgehog properties at 100 tests each, in about 7 s. The long
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

1. **Field state:** weather, terrain, hazards, screens. This makes
   `effectOrder` live for hazards. Choice items wait for the choice-lock
   volatile, and Heavy-Duty Boots waits for hazards.
2. **Cheaper event points.** Slice 2b costs about 38% on the plain path (see
   the benchmark log). Optimise with draws and state unchanged, measured on
   the workstation.
3. **More abilities and items** on the handler system, in usage order. Each
   one is a small task with an objective pass or fail, which suits delegation.
4. **Terastallization.**
5. **Escape hatches** for the mechanics that resist the rule vocabulary, such
   as Substitute and Transform.
6. **Per-player view.** What each side can observe, as a first-class value.
7. **Showdown seed compatibility.** Implement Showdown's own generators so a
   battle can be reproduced from its seed alone.

Alongside:

- **Monthly whitelist refresh.** Regenerate the whitelist when each month's
  stats appear and rerun `whitelist-report`. Teams are not yet rejected for
  being outside the whitelist, because the slice 0 traces use species from
  outside it. That check is added once the engine covers the whitelist.
- **Scenario generation from rules.** For each rule, generate battles that
  exercise it with each random outcome forced both ways, and report coverage
  per rule.
- **Trace size.** 2,300 battles take about 930 MB. Snapshots should become
  deltas.
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
