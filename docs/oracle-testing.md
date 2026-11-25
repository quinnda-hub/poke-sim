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
| `gen-slice0.js <outDir> <count> [firstSeed] [format]` | Generate random slice-0 battles and record each. Deterministic per seed. |
| `export-dex.js [outDir] [mod]` | Export species, moves, abilities, items, types and natures to `data/gen9/`. |

### Recording traces

```sh
node tools/oracle/gen-slice0.js local/traces/slice0 500
```

500 battles take about 240 MB, so traces live under `local/` and are not
committed. They are reproducible from the seeds.

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

- **`steps[i].draws`**: the draws made since the previous decision point, each
  `[lo, hi, result]`. For step 0 these are the draws made while the battle was
  constructed.
- **`state`**: the snapshot at the decision point, before `chosen` is applied.
- **`legal`**: the legal choice strings for each side. An empty list means that
  side is not being asked. At team preview it is `["team"]`.
- **`chosen`**: the choice each side made, or `null`.
- **`finalDraws`, `final`**: the draws after the last choice, and the end state.

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

The test reads traces from `$POKESIM_TRACES`, defaulting to
`local/traces/slice0`. It fails if the directory is missing or empty.

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
