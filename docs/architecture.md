# Engine architecture

This describes the new engine core under `src/Engine/` and the decisions behind
it. The older simulator under `src/Pokemon/` is described at the end.

Status: slices 0, 1 and 2 (2a and 2b) are built and replay 2,300 recorded
Showdown battles with identical draws and state. Decisions 1 to 5 and 7 are
implemented. Rules as data (decision 6) cover moves, and the first abilities
and items run on the event dispatcher (docs/handlers.md), with data rules where
they fit and native handlers where they don't. The usage
whitelist (decision 8) is generated, but teams are not yet checked against it.
See [roadmap.md](roadmap.md).

## Goals, in order

1. **Correctness.** Identical behaviour to Pokémon Showdown's `gen9ou` format,
   proven by replaying recorded battles. See
   [oracle-testing.md](oracle-testing.md).
2. **Speed.** The engine exists to sit under game-tree search and self-play,
   which need millions of steps.

## Decisions

### 1. Draw-for-draw parity

The engine makes the same random draws, with the same ranges, in the same order
as Showdown. This is stricter than matching outcomes, and it is what makes
"100% accurate" a test that can pass or fail. A mismatched draw identifies the
exact step that diverged.

### 2. One chance primitive

All randomness goes through one operation:

```haskell
class Monad m => MonadChance m where
  roll :: Int -> Int -> m Int   -- uniform in [lo, hi)
```

Engine code is written once against this class. The interpreter decides what a
roll means:

| Interpreter | Use |
|---|---|
| Replay | Feed recorded results and check each requested range. Used by the parity test. |
| Sample | Draw from a fast seeded generator. Used by playouts and self-play. |
| Enumerate (later) | Branch on every outcome with its probability. Used by search. |

### 3. State is plain data

The battle state has no functions, no text, no maps, no lazy fields and no IO.
Species, moves and types are small integer ids that index immutable tables in a
separate `Dex` value. The state derives `Eq` and `Show`.

This is what makes a position cheap to copy for search, and possible to hash,
compare and serialise.

### 4. A pure step function

```haskell
newBattle    :: MonadChance m => Dex -> Format -> Team -> Team -> m Battle
request      :: Battle -> Request
legalChoices :: Battle -> Player -> [Choice]
step         :: MonadChance m => Dex -> Battle -> (Maybe Choice, Maybe Choice)
             -> m (Either StepError Battle)
```

`newBattle` stops at team preview. `step` applies both sides' choices and runs
to the next decision point or the end. A side that is not being asked passes
`Nothing`. Players, strategies and logging live outside the engine.

The battle carries its `Format`, and its pending action queue, because Showdown
can stop for a forced replacement in the middle of a turn and resume the same
queue afterwards.

`step` returns an error, makes no draws and changes nothing when the battle is
already over, a side that must choose gives no choice, a choice is not legal, or
a side that is not being asked gives one. A silent no-op would let a search loop
forever on the same position.

Teams can only be built through `parseTeam`, which rejects anything the engine
cannot simulate exactly: moves and species outside the tested pool, any ability
or item, and levels outside 1 to 100. A set the engine accepts is a set it matches Showdown on.

### 5. Showdown's ordering, not Showdown's machinery

To match Showdown's draws, the turn loop has to perform the same sorts at the
same points. Speed ties cost a draw wherever Showdown sorts, including event
points that have nothing attached. So the engine keeps an action queue with
Showdown's sort key and named event points (`Update`, `BeforeTurn`, `SwitchIn`,
`Residual`), each commented with the Showdown function it mirrors.

What it does not copy is how Showdown dispatches: looking up handler functions
by string name across the Pokémon, its status, ability, item, side and field,
then building and sorting a list. Parity needs the same order, not the same
mechanism. Handlers can be resolved to integer-indexed tables when a battle
starts.

Showdown also makes draws that cannot affect the outcome, such as the accuracy
roll for a 100%-accurate move. They cost nanoseconds when sampling. Search that
enumerates outcomes will need them labelled so it can collapse them.

### 6. Rules as data

Moves, abilities and items are to be described by values of a closed rule type
(trigger, condition, effect) that the engine interprets. Rules contain no
functions, so they can be listed, compared and counted.

- A rule kind is added only when a mechanic on the usage whitelist needs it.
- One-off mechanics get a named escape hatch written directly in Haskell. The
  number of escape hatches shows whether the vocabulary is working.
- Rule data generates test scenarios, never expected results. Showdown supplies
  the expected result. Generating both from one source would let a wrong rule
  agree with itself.

Moves are built this way since slice 1 (`src/Engine/Rule.hs`). A `MoveRule`
is organised by the slot of Showdown's move pipeline each part runs in:

- the hit count;
- the target;
- `runMoveEffects` on the target (heal, stages, status);
- `selfDrops`;
- `secondaries`, each with its chance;
- drain;
- recoil;
- freezing and thawing.

Its slots are what the architecture calls triggers, for moves. Abilities and
items will add triggers that are not move slots.

A move is accepted only when its whole exported entry is accounted for. Every
field is translated or listed as inert with a reason. Any other field, and any
handler code, rejects the move. `executable` separates what the vocabulary can
describe from what the engine can run yet.

The trace generator takes its pool from the engine (`engine-pool`). That is the
"rule data generates test scenarios" half. The expected results still come only
from Showdown.

Abilities and items are not built yet. The design for them is in
[handlers.md](handlers.md). Showdown dispatches them through sorted events, so
the engine uses one generic dispatcher that mirrors Showdown's collection and
sort, including the draws its tie shuffles make. Handler bodies are either data
rules or named Haskell handlers. A new rule kind needs at least three
whitelisted users.

### 7. Format differences are parameters

`Format` carries what differs between formats, starting with the truncation
rule. Only `gen9ou` exists. Other generations would be further instances, with
their own data tables, damage formula and ordering rules. There is no
generation-agnostic framework and none is planned until a second generation is
actually needed.

### 8. Scope by usage

"100% accurate" is bounded by a whitelist of species, moves, abilities and
items taken from the monthly Gen 9 OU usage statistics. Inside the whitelist,
zero divergences are tolerated. Outside it, teams are rejected.

The whitelist is `data/usage/gen9ou.json`, generated by
`tools/usage/gen-whitelist.js` from Smogon's chaos statistics for `gen9ou` at
the 1695 rating cutoff:

- A species is included at a weighted usage of 0.5% or more.
- A move, item, ability or Tera type is included when it is on at least 1% of a
  whitelisted species' sets.
- Anything the pinned Showdown's `gen9ou` rule table bans is moved to an
  `excluded` section, because the stats can include species that were banned
  partway through the month. Only single-entity bans are checked.
- The file covers the latest month only. Regenerating it prints what was added
  and dropped, and git keeps the earlier months.

`whitelist-report` checks the whitelist against what the engine accepts
(`supportedSpecies`, `supportedMoves`). It lists the gaps in usage order with
Showdown's handler names, which makes it the to-do list for mechanics. Teams
are not yet rejected for being outside the whitelist, because the slice 0
traces use species from outside it.

## Data

`data/gen9/` is exported from the pinned Showdown build by
`tools/oracle/export-dex.js`. The engine's tables and the test's referee share
one source. The older files directly under `data/` (`moves.json`,
`pokedex.json`, `learnsets.json`) are earlier Showdown exports used only by the
legacy simulator.

## The legacy simulator

`src/Pokemon/`, `src/MyLib.hs` and `app/Main.hs` are the original simulator,
written before any reference was chosen. It still builds and is left untouched
until the new core overtakes it. Its parsers (PokePaste, the type chart) are
candidates for reuse.

It cannot serve search as it stands:

- The only entry point runs a whole battle. There is no single-step function.
- Each player's strategy is a closure stored inside the state.
- The monad is `StateT` over `WriterT` over `IO`, and it prints every turn.
- Randomness can only be sampled, never enumerated or forced.
- Every strategy can read the whole state. There is no per-player view.

Known bugs, found by reading and not fixed:

- `applyStatusEffects` (burn and poison residual damage) has no callers.
- `applyHealToActive` uses `max` where it needs `min`, so any heal sets HP to at
  least full (`src/Pokemon/Battle/MoveEffect/GenericSelfHeal.hs`).
- `applyMultiHitDamage` has no callers, so multi-hit moves deal no damage.
- Speed ties always resolve in player 1's favour.
- There is no test suite; `test/Main.hs` was a placeholder.
