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
| The switch-in speed order is computed and discarded; Showdown keeps it to order switch-in effects. Priority and speed keys are integers, and Showdown uses fractional values. This is a change of representation, not a local fix. | The first switch-in ability or item; Quick Claw |
| Speeds at team preview and in the action queue do not pass through speed modifiers. | Choice Scarf, paralysis, speed stages |
| The residual and switch-in field events are empty; Showdown collects and sorts handlers there. | Any end-of-turn or switch-in effect |
| Type checks use the species' types; Showdown uses the current types. Ground immunity ignores grounding. | Terastallization, type-changing moves, Levitate, Air Balloon |
| Move priority is the base priority only. | Prankster, Gale Wings, Grassy Glide |
| Damage modifiers are applied singly; Showdown chains several with its own rounding. | Abilities and items that modify damage |
| Forced switches by the opponent skip a step that voluntary switches run. | Roar, Whirlwind, Dragon Tail |
| A critical hit must ignore the attacker's stat drops and the defender's stat boosts. | Stat stages |
| A faint that causes another faint while faints are being resolved is not handled. | Destiny Bond and similar |
| The battle state has no ordering or hash, which search will want for a table of seen positions. | The first search code |

## Next: the simulator

Each step is gated by the parity test. See
[oracle-testing.md](oracle-testing.md).

1. **Rule vocabulary.** A closed rule type for the declarative move fields the
   export already carries: secondary effects, stat changes, status, drain,
   recoil, multi-hit, healing.
2. **Status conditions and stat stages**, with their end-of-turn effects.
3. **Field state:** weather, terrain, hazards, screens.
4. **Abilities and items** as rules, in usage order. Each one is a small task
   with an objective pass or fail, which suits delegation.
5. **Terastallization.**
6. **Escape hatches** for the mechanics that resist the rule vocabulary, such
   as Substitute and Transform.
7. **Per-player view.** What each side can observe, as a first-class value.
8. **Showdown seed compatibility.** Implement Showdown's own generators so a
   battle can be reproduced from its seed alone.

Alongside:

- **Monthly whitelist refresh.** Regenerate the whitelist when each month's
  stats appear and rerun `whitelist-report`. Teams are not yet rejected for
  being outside the whitelist, because the slice 0 traces use species from
  outside it. That check is added once the engine covers the whitelist.
- **Scenario generation from rules.** For each rule, generate battles that
  exercise it with each random outcome forced both ways, and report coverage
  per rule.
- **Trace size.** 800 battles take about 280 MB. Snapshots should become
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
