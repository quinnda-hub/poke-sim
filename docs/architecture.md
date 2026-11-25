# Engine architecture

This describes the new engine core under `src/Engine/` and the decisions behind
it. The older simulator under `src/Pokemon/` is described at the end.

Status: the first slice is being built. Nothing below has been demonstrated
against Showdown yet; see [roadmap.md](roadmap.md) for what has.

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
legalChoices :: Dex -> Battle -> Player -> [Choice]
step         :: MonadChance m => Dex -> Battle -> (Maybe Choice, Maybe Choice) -> m Battle
```

`step` applies both sides' choices and runs to the next decision point or the
end. A side that is not being asked passes `Nothing`. Players, strategies and
logging live outside the engine.

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

This is not built yet. Slice 0 has no rules, only plain damaging moves.

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
