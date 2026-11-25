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
- **500 slice-0 battles recorded** (26,079 turns, 84,652 draws).

## In progress

- **Slice 0 of the new engine core** under `src/Engine/`: plain damaging moves,
  switching, fainting, forced replacement and win detection. It is done when
  all 500 recorded battles replay with identical draws and identical state, and
  a single-core playout benchmark reports throughput.

## Next: the simulator

Each step is gated by the parity test. See
[oracle-testing.md](oracle-testing.md).

1. **Usage whitelist.** Generate the list of species, moves, abilities and
   items from the monthly Gen 9 OU usage statistics. Regenerate it each month
   so newly needed mechanics show up as a report.
2. **Rule vocabulary.** A closed rule type for the declarative move fields the
   export already carries: secondary effects, stat changes, status, drain,
   recoil, multi-hit, healing.
3. **Status conditions and stat stages**, with their end-of-turn effects.
4. **Field state:** weather, terrain, hazards, screens.
5. **Abilities and items** as rules, in usage order. Each one is a small task
   with an objective pass or fail, which suits delegation.
6. **Terastallization.**
7. **Escape hatches** for the mechanics that resist the rule vocabulary, such
   as Substitute and Transform.
8. **Per-player view.** What each side can observe, as a first-class value.
9. **Showdown seed compatibility.** Implement Showdown's own generators so a
   battle can be reproduced from its seed alone.

Alongside:

- **Scenario generation from rules.** For each rule, generate battles that
  exercise it with each random outcome forced both ways, and report coverage
  per rule.
- **Trace size.** 500 battles take 240 MB. Snapshots should become deltas.
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

- How fast the Haskell engine is. Unmeasured until slice 0 has its benchmark.
- How much of the mechanics list the rule vocabulary covers before escape
  hatches dominate.
- Whether the reduced-scale training budget is enough for a policy worth
  searching on top of.
- How long Gen 9 OU remains the current metagame.
