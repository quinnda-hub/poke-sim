@.claude/delegation.md

# poke-sim

A Pokémon battle simulator in Haskell, built to sit under search and self-play
for an agent that plays Smogon Gen 9 OU. The order of work is fixed: an exactly
correct, fast simulator first, and no bot work until it exists.

## Commands

- Build: `cabal build all`
- Parity test: `cabal test parity --test-show-details=direct`
- Benchmark: `cabal bench`
- Record traces: `node tools/oracle/gen-slice0.js local/traces/slice0 500`
- Re-export game data: `node tools/oracle/export-dex.js`

Toolchain: GHC 9.4.8, cabal 3.14, Node 22.18 or newer.

The parity test needs the pinned Showdown checkout and recorded traces, both
under `local/`. If either is missing, set them up from `docs/oracle-testing.md`.

## How correctness is defined

Pokémon Showdown is the referee: version 0.11.11, commit `c7fa6f1`, format
`gen9ou`. Given the same teams, choices and random draws, the engine must make
the same draws in the same order and reach the same state at every decision
point. A change to engine behaviour is verified by the parity test passing, and
by nothing else.

- When a trace diverges, read the Showdown source under `local/showdown/sim/`
  to learn what it does. Do not guess from game knowledge; Showdown differs
  from the cartridge games and from intuition.
- Never loosen the parity test to make it pass: no skipped traces, no ignored
  fields, no tolerance on draw counts.
- Use `gen9ou`, not `gen9customgame`, as the reference format. Custom Game uses
  different number truncation.

## Things that are not obvious from the code

- Two simulators coexist. `src/Engine/` is the new core and where work happens.
  `src/Pokemon/`, `src/MyLib.hs` and `app/` are the legacy simulator: keep it
  compiling, do not extend it, do not fix its known bugs.
- Two sets of data coexist. `data/gen9/` is generated from the pinned Showdown
  by `tools/oracle/export-dex.js`; regenerate it, never edit it by hand. The
  files directly under `data/` belong to the legacy simulator.
- `local/` is gitignored. It holds the Showdown checkout (`local/showdown`,
  read-only) and recorded traces (`local/traces`, about 240 MB per 500
  battles). Write throwaway scripts elsewhere.
- Scope is bounded by Gen 9 OU usage. Do not implement mechanics for other
  generations or build a generation-agnostic framework.

## Further reading

Read these when the task touches them; they are not loaded automatically.

- `docs/architecture.md`: the engine's design decisions and the reasons.
- `docs/oracle-testing.md`: setting up Showdown, recording traces, the trace
  format, debugging a divergence.
- `docs/roadmap.md`: what is done, in progress and next.
- `docs/research.md`: prior work and the planned method for the bot, with
  sources.

## Project delegation notes

- Keep in the lead (unresolved judgment specific to this project):
  - The parity contract: what the parity test checks, the trace format, and the
    tools under `tools/oracle/`. An implementer who thinks the test or a trace
    is wrong reports it with evidence and does not change it.
  - The reference pin: Showdown version, commit and format, and re-exporting
    `data/gen9/`.
  - The engine's public interface and state layout: `newBattle`, `request`,
    `legalChoices`, `step`, the `MonadChance` class, and what the battle state
    may contain.
  - The turn loop's structure: adding, removing or reordering event points, and
    any change to where sorts and draws happen that is not forced by a specific
    Showdown code path.
  - The rule vocabulary: adding a rule kind, and choosing between a rule and a
    hand-written escape hatch for a mechanic.
  - Scope: what a slice covers, and what the usage whitelist includes.
  - Any trade of exactness for speed, such as skipping draws that cannot affect
    the outcome.
  - Deleting or changing the legacy simulator.

  Delegate freely once these are settled for the task: implementing a mechanic
  against recorded traces, tracking down a divergence, and benchmark-backed
  optimisation that leaves draws and state unchanged.
