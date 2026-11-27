# poke-sim

A Pokémon battle simulator in Haskell, built to reproduce
[Pokémon Showdown](https://github.com/smogon/pokemon-showdown)'s Gen 9 OU
singles exactly and to run fast enough for game-tree search and self-play.

The long-term goal is an agent that plays Smogon 6v6 Gen 9 OU at the highest
level. The simulator comes first.

## Status

Early. The reference tooling works, and the engine core matches Showdown
exactly on everything it accepts.

- **Working:** recording battles on a pinned Showdown build with every random
  draw captured, and exporting game data from that build.
- **Working:** the new engine core, through slice 2b. It covers:
  - team preview, damaging moves, Struggle, switching, fainting and forced
    replacement;
  - 313 moves described as data;
  - stat stages, status conditions and flinch;
  - an event dispatcher that mirrors Showdown's;
  - the first 28 items and 10 abilities.

  It replays 2,300 recorded battles with identical random draws and identical
  state.
- **Not started:** field effects (weather, terrain, hazards, screens), most
  abilities and items, Terastallization, and everything about the bot.

See [docs/roadmap.md](docs/roadmap.md).

## How correctness is tested

Given the same teams, the same choices and the same random draws, the engine
must make the same draws in the same order as Showdown and reach the same state
at every decision point. Battles are recorded on Showdown and replayed through
the engine; any difference is a failure.

See [docs/oracle-testing.md](docs/oracle-testing.md).

## Getting started

Requirements: GHC 9.4.8, cabal 3.14, and Node 22.18 or newer for the reference
tooling.

```sh
cabal build all
```

To run the parity test, first set up the pinned Showdown checkout (the steps
are in [docs/oracle-testing.md](docs/oracle-testing.md)), then:

```sh
node tools/oracle/gen-slice0.js local/traces/slice0 500
node tools/oracle/gen-slice0.js local/traces/slice0 300 1 gen9ou struggle
cabal run -v0 engine-pool > local/traces/slice1-pool.json
node tools/oracle/gen-slice1.js local/traces/slice1 600 local/traces/slice1-pool.json
cabal run -v0 engine-pool > local/traces/slice2-pool.json
node tools/oracle/gen-slice2.js local/traces/slice2 600 local/traces/slice2-pool.json
node tools/oracle/gen-slice2.js local/traces/slice2 300 local/traces/slice2-pool.json 1 struggle
cabal run -v0 engine-pool > local/traces/slice3-pool.json
node tools/oracle/gen-slice3.js local/traces/slice3 1500 local/traces/slice3-pool.json
cabal run -v0 engine-pool > local/traces/slice4-pool.json
node tools/oracle/gen-slice4.js local/traces/slice4 1500 local/traces/slice4-pool.json
cabal run -v0 engine-pool > local/traces/slice5-pool.json
node tools/oracle/gen-slice5.js local/traces/slice5 1200 local/traces/slice5-pool.json
cabal run -v0 engine-pool > local/traces/slice6-pool.json
node tools/oracle/gen-slice6.js local/traces/slice6 2000 local/traces/slice6-pool.json
cabal test parity --test-show-details=direct
```

## Layout

| Path | Contents |
|---|---|
| `src/Engine/` | The new engine core. |
| `src/Pokemon/`, `src/MyLib.hs`, `app/` | The original simulator, kept building until the new core replaces it. |
| `tools/oracle/` | Node scripts that record Showdown battles and export its data. |
| `data/gen9/` | Game data exported from the pinned Showdown build. |
| `data/*.json`, `data/*.txt` | Data and sample teams for the original simulator. |
| `docs/` | Design, testing, roadmap and research notes. |
| `local/` | Not committed: the Showdown checkout and recorded battles. |

## Documentation

- [Architecture](docs/architecture.md): the engine's design decisions.
- [Testing against Showdown](docs/oracle-testing.md): setup, tools, trace
  format, debugging.
- [Roadmap](docs/roadmap.md): done, in progress, next.
- [Research notes](docs/research.md): prior work and the planned method for the
  bot.

## Licence

BSD-3-Clause. See [LICENSE](LICENSE).

Game data under `data/` is derived from Pokémon Showdown, which is MIT
licensed. Pokémon is a trademark of Nintendo, Creatures Inc. and GAME FREAK
inc.; this project is not affiliated with them.
