---
paths:
  - "src/Engine/**"
  - "test/**"
  - "bench/**"
---

# Engine code rules

These apply to the new engine core. The reasons are in `docs/architecture.md`.

## State

- The battle state is plain strict data: no functions, no `Text` or `String`,
  no `Map`, no lazy fields, no IO. It derives `Eq` and `Show`.
- Species, moves, types, abilities and items appear in the state as integer
  ids. Names and other static data live in the `Dex` tables.

## Randomness

- Every random outcome goes through `roll lo hi` from `Engine.Chance`. Do not
  use another generator inside engine code.
- A draw must match Showdown's: same range, same position in the sequence.
  Adding, removing or reordering a draw is a behaviour change, even when the
  outcome looks the same.

## Ordering

- Keep the turn loop's event points (`Update`, `BeforeTurn`, `SwitchIn`,
  `Residual` and any added later) performing the sorts Showdown performs there,
  even when nothing is attached.
- Comment each event point and each non-obvious step with the Showdown file and
  function it mirrors, for example `-- sim/battle.ts eachEvent`.

## Arithmetic

- Use exact integer arithmetic. Showdown computes in JavaScript doubles and
  truncates through `battle.trunc`; when translating a step, check that the
  integer version agrees, including the 16-bit and 32-bit wraps `gen9ou` uses.

## Tests

- The parity test is the acceptance test. Run it after any engine change:
  `cabal test parity --test-show-details=direct`.
- When adding a mechanic, add recorded Showdown battles that exercise it. Do
  not write expected values by hand, and do not derive them from the engine.
- A failure message must name the trace, the step, the first differing field
  and the draw index.

## Performance

- No partial functions on reachable paths.
- Do not optimise without a benchmark number before and after.
