# Testing against Pokémon Showdown

The engine's correctness is defined by one test: given the same teams, the same
choices and the same random draws, it must make the same draws in the same order
as Pokémon Showdown and reach the same state at every decision point.

Showdown is the referee, not the cartridge games. The ladder, the usage
statistics and every opponent the eventual bot will meet all run on Showdown.

## The pinned reference

| | |
|---|---|
| Project | [smogon/pokemon-showdown](https://github.com/smogon/pokemon-showdown) |
| Version | 0.11.11 |
| Commit | `c7fa6f16503de510c319239b5b2d29d4f7abd8c4` |
| Format | `gen9ou` |
| Location | `local/showdown` (gitignored) |

`data/gen9/meta.json` records the version and commit the checked-in data was
exported from. Changing the pin means re-exporting the data and re-recording
every trace.

The reference format is `gen9ou`, not `gen9customgame`. Custom Game replaces
Showdown's number truncation with plain `Math.trunc`; real formats wrap to
unsigned 32 bits and truncate some values to 16 or 13 bits. Testing against
Custom Game would validate the wrong arithmetic.

### Setting it up

Requires Node 22.18 or newer.

```sh
mkdir -p local/showdown && cd local/showdown
git init -q
git remote add origin https://github.com/smogon/pokemon-showdown.git
git fetch -q --depth 1 origin c7fa6f16503de510c319239b5b2d29d4f7abd8c4
git checkout -q FETCH_HEAD
npm ci --ignore-scripts --no-audit --no-fund
node build
```

The checkout plus `node_modules` and the build take about 300 MB. Treat it as
read-only: never edit it, and write throwaway scripts somewhere else.

## Why this works

- **All randomness goes through one function.** `PRNG.random(from, to)` in
  `sim/prng.ts` is the only place Showdown draws. `randomChance`, `sample` and
  `shuffle` all call it.
- **The generator can be injected.** `new Battle({ prng })` accepts any `PRNG`
  subclass, so a subclass can record every draw, or return scripted results.
- **Battles are deterministic.** The same seed, teams and choices replay
  identically.

The engine mirrors this with a single chance primitive, `roll lo hi`, that
returns a uniform integer in `[lo, hi)`. Under test, a replay interpreter feeds
it the recorded results and checks that each requested range matches the
recorded one. The first mismatched range points at the exact step where the
engine and Showdown part ways.

## Tools

All under `tools/oracle/`. Each finds Showdown through `$SHOWDOWN_DIR`, falling
back to `local/showdown`.

| Tool | Purpose |
|---|---|
| `record.js <scenario.json> [out.json]` | Play one battle on Showdown and write its trace. |
| `gen-slice0.js <outDir> <count> [firstSeed] [format] [profile]` | Generate random slice-0 battles and record each. Deterministic per seed. |
| `gen-slice1.js <outDir> <count> <poolFile> [firstSeed]` | Generate random slice-1 battles from the engine's own pool and record each, then print a coverage tally from Showdown's log. Deterministic per seed and pool. |
| `gen-slice3.js <outDir> <count> <poolFile> [firstSeed]` | Slice 1's teams with hazard, screen, Tailwind and Trick Room setters, Light Clay, Flying, Poison and Steel types, Shedinja, low levels, and abilities and items weighted toward Toxic Debris and Heavy-Duty Boots, recorded, then a tally of the hazard outcomes (each hazard's damage and layers, absorption, immunities, Boots blocks, hazard KOs, the order of several hazards on one switch-in, screens against each category and crits, both screens on one side, expiries, Trick Room restarts and speed ties under it, weather setters as moves and abilities, rocks, sand chip and immunities, weather speed abilities and Hydration, terrain setters, Terrain Extender, grounding, Overcoat and Leaf Guard, Protosynthesis and Quark Drive with and without Booster Energy, the seeds and Unburden; about one battle in six pairs a faster Paradox or seed holder against a slower opposing setter, so switch-in priority decides the order). Deterministic per seed and pool. Re-record the whole set when the pool changes. |
| `gen-slice5.js <outDir> <count> <poolFile> [firstSeed]` | Slice 3's and slice 0's teams with a Tera type on every set (about half from usage, the rest uniform over the 18 types; about a quarter of sets are then made Stellar), recorded with Tera choices offered (`tera: true`, a `teraChance` of 0.1-0.5), plus scripted double Teras, Trick Room Teras and resistance duels, then a tally of each Tera branch. Deterministic per seed and pool. |
| `gen-slice4.js <outDir> <count> <poolFile> [firstSeed]` | Slice 3's teams, with move slots weighted toward the moves with handler code and the conditions their bodies branch on (weights, statuses, boosts, held and used items, weather, terrain, grounding), recorded, then a tally of each branch from Showdown's own events. Deterministic per seed and pool. |
| `gen-slice2.js <outDir> <count> <poolFile> [firstSeed] [profile]` | Slice 1's teams (or, with `struggle`, slice 0's Struggle teams) with abilities and items from the engine's pool, recorded, then a tally of the outcomes that distinguish each handler, observed by wrapping Showdown's event methods. Deterministic per seed and pool. |
| `export-dex.js [outDir] [mod]` | Export species, moves, abilities, items, types and natures to `data/gen9/`. |

### Recording traces

```sh
node tools/oracle/gen-slice0.js local/traces/slice0 500
node tools/oracle/gen-slice0.js local/traces/slice0 300 1 gen9ou struggle
```

The generator has two profiles, because random battles alone never reach some
in-scope behaviour:

| Profile | Teams | What it reaches |
|---|---|---|
| `random` (default) | Six random Pokémon with four random plain moves | Speed ties, priority, never-miss and high-crit moves, immunities, varied levels and spreads |
| `struggle` | One to three Ghost types with one low-PP Normal or Fighting move | PP running out, Struggle and its recoil, both sides fainting at once, both sides replacing at once, Shedinja's fixed 1 HP |

Slice 1 takes its pool from the engine instead of computing one:

```sh
cabal run -v0 engine-pool > local/traces/slice1-pool.json
node tools/oracle/gen-slice1.js local/traces/slice1 600 local/traces/slice1-pool.json
```

`engine-pool` prints the moves and species the engine accepts. Only the
choice of battles comes from the engine. The expected draws and states still
come from Showdown, so a wrong rule cannot agree with itself. When the engine
accepts more moves, abilities or items, only the newest slice's set is
recorded again, from a fresh pool. Earlier sets stay as recorded: each set's
pool must still be accepted by the engine, and the newest set's pool covers
what is new. Until it is re-recorded the parity test fails, because some
accepted move is in no set's pool list. The long tier also requires every
accepted move, species, ability and item to appear in some trace's teams. A third of the move slots are drawn from
moves with the slice's mechanics, so that rare cases are reached. The coverage
tally (multi-hit counts, drain at full HP, recoil knockouts, a failed heal,
spread moves into an immunity, every pool move used) should show none at zero.

Slice 2 adds abilities and items, drawn from the pool, to the same teams:

```sh
cabal run -v0 engine-pool > local/traces/slice2-pool.json
node tools/oracle/gen-slice2.js local/traces/slice2 600 local/traces/slice2-pool.json
node tools/oracle/gen-slice2.js local/traces/slice2 300 local/traces/slice2-pool.json 1 struggle
```

Slice 3 (field state) records from the same kind of pool:

```sh
cabal run -v0 engine-pool > local/traces/slice3-pool.json
node tools/oracle/gen-slice3.js local/traces/slice3 1500 local/traces/slice3-pool.json
```

Slice 4 (a move's own handlers) records the same way:

```sh
cabal run -v0 engine-pool > local/traces/slice4-pool.json
node tools/oracle/gen-slice4.js local/traces/slice4 1500 local/traces/slice4-pool.json
```

Slice 5 (Terastallization) records with Tera choices offered:

```sh
cabal run -v0 engine-pool > local/traces/slice5-pool.json
node tools/oracle/gen-slice5.js local/traces/slice5 1200 local/traces/slice5-pool.json
```

Its teams put the moves with handler code where their bodies branch: weights
across the weight-based bands, statused targets and users, boosts, items held
and used up, each weather and terrain, grounded and ungrounded targets, and
Grassy Glide in priority ties. Its tally counts each branch from Showdown's
own events.

Slice 3's teams are built so that hazards are set, stack and fire on switch-in, often two or three on one side. The order of several hazards on one side is visible only through effects on later draws and state, such as a knockout before the next hazard or a Poison type absorbing Toxic Spikes. The trace has no battle log, so a wrong order fails only the traces where it changes HP or a side condition.

The set writes four lists: `slice2-moves.json`, `slice2-species.json`,
`slice2-abilities.json` and `slice2-items.json`. A set without the last two
draws only "No Ability" and no item. The pool check and the long tier's
coverage check cover abilities and items as well as moves and species.

The two profiles reach different things:
- **`random`:** biases ability draws toward observable cases (a pinch ability
  with a move of its type, Technician with a weak move) and item draws
  toward the residual items, so that two of them tie on speed.
- **`struggle`:** puts the items and abilities that act on Struggle in play:
  Rocky Helmet (Struggle makes contact), Life Orb, Technician and Huge Power.

The tally should show no counter at zero. The wrappers only observe: they
pass every argument through and run nothing Showdown would not run.

Traces live under `local/` and are not committed. Slice 0's 800 battles take
about 280 MB, slice 1's 600 about 320 MB, slice 2's 900 about 340 MB and slice 3's 1,500
about 1.0 GB, slice 4's 1,500 about 1.0 GB and slice 5's 1,200 about 769 MB. They
are reproducible from the seeds.

To isolate one behaviour, write a scenario by hand and give it explicit choices:

```json
{
  "format": "gen9ou",
  "seed": "gen5,0001000200030004",
  "policySeed": 1,
  "teams": [[{ "species": "Pikachu", "ability": "No Ability", "item": "",
               "moves": ["tackle"], "nature": "Hardy", "level": 100 }],
            [{ "species": "Snorlax", "ability": "No Ability", "item": "",
               "moves": ["tackle"], "nature": "Hardy", "level": 100 }]],
  "choices": [["team 1", "team 1"], ["move 1", "move 1"]]
}
```

When `choices` runs out, a seeded random policy picks among the legal choices.

### Trace format

```
{ traceVersion, showdownVersion, format, seed, policySeed,
  teams: [[set, ...], [set, ...]],
  steps: [ { draws, state, legal, chosen }, ... ],
  truncated, finalDraws, final }
```

A trace recorded with Tera choices offered also has `tera: true`, after
`policySeed`.

- **`steps[i].draws`**: the draws made since the previous decision point, each
  `[lo, hi, result]`. For step 0 these are the draws made while the battle was
  constructed.
- **`state`**: the snapshot at the decision point, before `chosen` is applied.
- **`legal`**: the legal choice strings for each side. An empty list means that
  side is not being asked. At team preview it is `["team"]`.
- **`chosen`**: the choice each side made, or `null`.
- **`finalDraws`, `final`**: the draws after the last choice, and the end state.
- **`tera`**: present and true only when the policy was offered
  `move N terastallize` choices. Without it the parity test compares the
  legal lists with the engine's minus its Tera choices, which Showdown allowed
  but the recorder did not list. Scenarios may set `teraChance`: the policy
  then picks among the other choices and takes a move's Tera variant with
  that probability.
- **`truncated`**: true when the recorder stopped at its turn limit (300 by
  default). It stops at the top of a decision point, so `final` is a battle
  that has not ended. The parity test checks such a trace like any other, up
  to the cut, and then requires an empty step to be refused for the missing
  choice, without a draw, instead of as a finished battle. Generators keep
  truncated traces; skipping them would drop the longest battles from the set.

Pokémon inside `state.sides[i].pokemon` are listed in Showdown's current slot
order, which is the order `switch N` indexes. Showdown reorders slots when a
Pokémon switches in. `orig` is the index into the original team.

### Exporting data

```sh
node tools/oracle/export-dex.js
```

Showdown implements much behaviour as handler code, which cannot be exported as
data. For those entries the export lists the handler names under `handlers`.
That list is the to-do list for hand-written rules. At the pinned commit:

| | Total | With handler code |
|---|---|---|
| Moves | 938 | 419 |
| Abilities | 321 | 306 |
| Items | 587 | 326 |

## Running the parity test

```sh
cabal test parity --test-show-details=direct
```

`POKESIM_TIER=fast` replays a fixed sample of each set, the traces at every
10th position in its sorted list. Each sampled trace is checked in full, and
the pool checks always cover every set. `long`, the default when the variable
is unset, replays every trace. The summary line names the tier and the sampled
and total counts for each set.

The test reads trace sets from `$POKESIM_TRACES`, defaulting to `local/traces`.
Each subdirectory is one set, such as `slice0` or `slice1`. Plain files there,
such as the pool file, are ignored. It fails if there are no sets or a set has
no traces.

Besides replaying each trace, it checks that:

- every draw is three integers with a non-empty range;
- the trace is version 1, format `gen9ou`, with two sides in every snapshot;
- every snapshot field the engine does not model holds its default value, so a
  trace that uses a mechanic outside the current slice fails loudly;
- each set's `<set>-moves.json` and `<set>-species.json`, written by its
  generator, list only moves and species the engine accepts, and together the
  sets list everything it accepts, so the engine accepts exactly what it has
  been tested on;
- `step` returns the right error for each kind of bad input.

## Debugging a divergence

1. Read the failure: it names the trace, the step, the first differing field
   and the draw index.
2. A range mismatch means the engine made a draw Showdown did not, or skipped
   one. A state mismatch with matching draws means the arithmetic or the
   bookkeeping differs.
3. Reduce it. Record a short scenario with explicit choices that shows the same
   difference.
4. Read the Showdown code path. To find which call made a draw, wrap the
   generator in a throwaway script and print a stack trace per draw.
5. Fix the engine. Never loosen the test: no skipped traces, no ignored fields,
   no tolerance on draw counts.

## Things Showdown does that are easy to get wrong

- **Draws happen where no effect is attached.** `eachEvent` speed-sorts the
  active Pokémon before dispatching, even to nothing. Exact speed ties cost a
  draw each time, for example at the `Update` event after every action and
  every hit.
- **`pokemon.speed` is a cache.** It is refreshed only by `updateSpeed()` at
  specific points. Stale or initial values can tie when real speeds differ.
- **A 100%-accurate move still draws for accuracy.** Only `accuracy: true`
  skips the draw.
- **Type immunity is checked before accuracy.** An immune target causes no
  draws at all.
- **Switches always precede moves.** Priority only orders actions of the same
  kind.
- **Gender is drawn** when neither the set nor the species fixes it.
