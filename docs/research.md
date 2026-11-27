# Research notes

What is known about prior work, and what it implies for this project. Gathered
on 2026-11-25; Jaxcalibur and PokeRus added on 2026-11-27. These are notes, not decisions; decisions are in
[architecture.md](architecture.md) and [roadmap.md](roadmap.md).

How far each claim was checked:

- **[checked]** read at the primary source and re-checked for this document.
- **[read]** read at the primary source during the research pass, not
  re-checked.
- **[reported]** self-reported by an author on a forum, or seen only in a
  summary or abstract.

## Where competitive Pokémon AI stands

The organisers of the NeurIPS 2025 PokéAgent Challenge describe the state of
the art as "less than superhuman". [read] Since then two bots trained by
self-play have reached #1 on a Gen 9 ladder:
- Jaxcalibur, in Random Battles, before the bot policy;
- PokeRus, in OU, after it, against an opt-in pool that includes other bots.
Neither has been tested in a controlled match against a top OU player.

| System | Method | Hidden information | Strength |
|---|---|---|---|
| [Foul Play](https://pmariglia.github.io/posts/foul-play) | Root-parallel MCTS with decoupled UCT and a hand-written evaluation, on the Rust `poke-engine` | Samples full opponent sets from usage stats, forum teams and replays; narrows them by inference | Gen 9 OU: 80% GXE, peak 1879, top 100. Gen 3 OU: 71% GXE, peak 1815, rank 4. Won the PokéAgent Gen 9 OU track 50–14. [checked, as of May 2025] |
| [Metamon](https://arxiv.org/abs/2504.04395) | Offline RL with transformers, no search | Implicit, from the history | Best public model (Dec 2025): GXE 82% Gen 1, 63% Gen 3, 71% Gen 9. Gen 1 ensembles have held #1 on the human ladder. [read] |
| [PokéChamp](https://arxiv.org/abs/2503.04094) | LLM-driven minimax | LLM prediction | Projected Elo 1300–1500 in Gen 9 OU. [read] |
| Jett Wang, [MIT thesis](https://dspace.mit.edu/handle/1721.1/153888) | PPO actor-critic guiding MCTS | — | Gen 4 random battles, peak rank 8 at 1693. [reported, abstract only] |
| [Jaxcalibur](https://jaxcalibur.github.io/) | 8.5M-parameter transformer trained by PPO self-play on its own JAX GPU engine; at test time, regret-matching pUCT over sampled worlds | Network heads predict each opposing Pokémon's item, ability, moves and Tera type; search samples 32 worlds from them | Gen 9 Random Battles: #1 on and off in August 2026, peak Elo 2557, GXE 95.3; won a best-of-nine 5–3 against the top human, 2026-10-04. Before the bot policy. [checked] |
| [PokeRus](https://www.smogon.com/forums/threads/pokerus-selfplay-teambuilder-bot-reached-1-on-gen9-ou-ladder.3790884/) | Transformer policy trained by self-play RL, no search, about 0.2 s a move; a second model builds teams, rewarded by their self-play win rate [reported] | Not stated | Gen 9 OU: #1 at Elo 1955 by 2026-11-04 [read]; Elo 2099, GXE 90.8%, 703–218 on 2026-11-27 [checked]. After the bot policy, so not comparable with Foul Play's numbers. |

Before the bot policy, the strongest verified approach in OU was Foul Play's
search with a hand-written evaluation. What it lacks:

- A learned evaluation.
- Equilibrium mixing. Decoupled UCT has no convergence guarantee in
  simultaneous-move games; regret matching and Exp3 do
  ([Lisý et al. 2013](https://arxiv.org/abs/1310.8613)). [reported]
- Sound reasoning about hidden information. Sampling opponent sets cannot
  reason about concealing your own sets or about what the opponent believes.

## Two self-play bots to model

Both are closed: no code, weights or paper. Notes and saved sources are in
`local/research/jaxcalibur/` and `local/research/pokerus/`.

**Jaxcalibur** has the only technical write-up of the two
([jaxcalibur.github.io](https://jaxcalibur.github.io/)). It is the nearest
template for this project's bot, even though it plays Random Battles.
- **Network** [checked]:
  - a non-causal transformer of 8.5M parameters, with separate input weights
    per token type;
  - tokens for the field, the active Pokémon, the moves, both teams (the
    opponent's masked until revealed) and the 16 latest events;
  - 24 matchup tokens, carrying damage calculations, added halfway through;
  - heads for the policy (with a Tera variant of each move) and the win
    probability;
  - a head for the opponent's next action, which the author says made training
    about 3x more efficient [reported];
  - heads predicting each opposing Pokémon's item, ability, moves and Tera
    type.
- **Training** [checked]:
  - PPO with GAE on an undiscounted win/loss reward;
  - an entropy bonus and a KL term toward uniform, so rarely right moves keep
    some probability;
  - pure self-play, with no imitation and no replays;
  - about 100 million games in a week on one H100.
- **Search**, at test time only [checked]:
  - edges are sampled with π ∝ max(Q − V, 0) + c·P/√N, which tends to regret
    matching as visits grow;
  - 32 worlds sampled from the prediction heads, 20,480 rollouts to depth 4
    (about two turns), a few seconds on a GPU;
  - no chance nodes: each rollout gets a fresh seed, and statistics live in a
    hash table keyed on bucketed state (HP in 10 bins). Its own nodes are keyed
    on revealed information only;
  - the simulated opponent searches only until Jaxcalibur does something
    surprising (a switch to an unrevealed Pokémon, or a move it rates under
    65%), and from then on plays from the network prior;
  - the author puts search at 100–150 Elo over the network alone [reported].
- **Engine.** A vmapped JAX engine, built for throughput, with "~30 known
  inaccuracies" against Showdown [reported]. It plays live through poke-env.
- **Weaknesses it reports** [reported]:
  - set attributes are predicted independently, so impossible sets come out
    (Choice Band with Swords Dance);
  - the value is trained from one side's view;
  - OU would need many more mechanics and EV prediction.

**PokeRus** has only a five-post forum thread. It says that:
- the policy is a transformer, trained by self-play RL, with no search yet;
- a separate teambuilder is rewarded by its teams' self-play win rate, and has
  generated "over 50 million teams".
Everything about its design is [reported]. Its rating is [checked] (see the
table). Its sample team uses unusual EV spreads, so the teambuilder seems to
search outside usage sets.

What they add to the picture:

- Policy-gradient self-play, with no imitation and no search during training,
  gave most of the strength in both. That agrees with Rudolph et al. and
  Ataraxos (below). Search is a test-time addition.
- One GPU-week was enough for Random Battles. OU has wider hidden information
  (EV spreads, chosen sets), so its belief model is harder. It should predict
  whole sets jointly, seeded from usage statistics, not one attribute at a
  time.
- Search with resampled seeds and a hash table needs only sampled chance,
  cheap copies and a state hash, not exact enumeration of outcomes.
- Exactness was not what limited strength: Jaxcalibur's engine is inexact. This
  project keeps exactness as a correctness choice, and must compete on
  throughput by running many games across CPU cores, not by vectorising one
  engine on a GPU. For scale: Jaxcalibur's run averaged about 165 games/s,
  training included [derived from 100M games in a week].

## Equilibrium methods in Pokémon

- No CFR, Deep CFR or ReBeL work on OU singles was found.
- PokaiTrainer ran CFR over public belief states in VGC doubles: 59% of 150
  best-of-three sets against humans, peak 1492. [reported by the research pass
  as arXiv 2608.29197]
- An R-NaD attempt on Showdown ([baskuit/Dolly](https://github.com/baskuit/Dolly))
  was abandoned for lack of compute and simulator speed. [read]
- [baskuit/pinyon](https://github.com/baskuit/pinyon) implements bandit-based
  simultaneous-move search for perfect-information games. [read]

## Existing engines

| Engine | Language | Coverage | Throughput |
|---|---|---|---|
| Pokémon Showdown | TypeScript | Everything | About 16 Gen 1 battles per second in pkmn's benchmark [derived] |
| [pkmn/engine](https://github.com/pkmn/engine) | Zig | Gen 1 only; matches Showdown exactly | 10,000 Gen 1 battles in 195 ms against 618 s for patched Showdown [read] |
| [poke-engine](https://github.com/pmariglia/poke-engine) | Rust | Gens 1–9 with Terastallization, per its build features | No published benchmark |
| [advsim](https://github.com/zkingston/advsim) | Python, NVIDIA Warp (GPU) | Gen 3 random battles only | "About 745K decisions/s on an RTX 4070" in a PPO smoke run; zero divergences from Showdown 0.11.11 over 100,000+ battles [checked] |

Two engines show that exact parity with Showdown and high speed are
compatible. Neither covers Gen 9 OU with explicit chance and a per-player view.

## Data

- **Usage statistics** ([smogon.com/stats](https://www.smogon.com/stats/)):
  monthly, per format, at several rating cutoffs (0/1500/1695/1825 for Gen 9
  OU). The "chaos" JSON gives per-species marginals for abilities, items,
  spreads, moves, Tera types, teammates and checks. It gives no joint
  distribution over complete sets, so a belief model has to reconstruct sets.
  [read]
- **Volume, October 2026:** 608,936 Gen 9 OU battles; 198,165 Gen 3 OU. [read]
- **Replays:** Showdown's replay API serves `.json`, `.log` and `.inputlog`.
  Public replays show the spectator view only. Metamon's raw replay set holds
  2.68M battles, over 1.2M of them Gen 9 OU. [read]

## Ladder policy

Showdown published an initial bot policy on 2026-10-09
([thread](https://www.smogon.com/forums/threads/3789528/)): bots must be
registered, and an unregistered bot is bannable. From 2026-10-20, humans opt in
to playing bots, and bot games do not affect human ratings. Ladder numbers from
before and after are therefore not comparable. [read]

## Methods for large imperfect-information games

The hidden information here is unusual: it is fixed before the game, split per
Pokémon, revealed piece by piece, and almost everything observed is public.

**Ruled out**

- **ReBeL and Student of Games.** Both need an input that grows with the number
  of private states consistent with the public history, and both papers say so.
  Compute was 720 V100s and roughly 3,500 TPUv4 actors. [read]
- **Deep CFR, DREAM, ESCHER.** In the largest comparison
  ([Rudolph et al.](https://arxiv.org/abs/2502.08938), 7,000 runs), generic
  policy-gradient methods matched or beat NFSP, PSRO, ESCHER and R-NaD. [read]
- **LAMIR** ([arXiv 2510.05048](https://arxiv.org/abs/2510.05048)). It does not
  model chance nodes. [read]

**Closest template: Ataraxos** ([arXiv 2511.07312](https://arxiv.org/abs/2511.07312),
published in Nature in 2026). [read]

- Superhuman Stratego: beat a top human 15–1–4.
- Three parts: a policy trained by regularised policy-gradient self-play; an
  autoregressive transformer that guesses the hidden pieces; and test-time
  search that samples worlds from that belief model, rolls each candidate move
  out with the policy playing both sides, then takes one mirror-descent step.
- Search added 123 Elo over the raw policy at about 1.26 H100-seconds per move.
- Training used 16 H100s for a week plus 4 for four days, under $8k, on a
  custom GPU simulator running millions of steps per second.

**Other relevant results**

- **Obscuro** ([arXiv 2506.01242](https://arxiv.org/abs/2506.01242)): superhuman
  Fog of War chess on consumer hardware with no training. It samples a few
  hundred consistent positions and searches them. It depends on a strong
  perfect-information evaluation. [read]
- **Test-time RL** (Kubíček, Lisý, Sandholm): extends Ataraxos-style search to
  several update steps. [reported by the research pass as arXiv 2608.30635]
- **VRPO** and a Generals.io agent reached state-of-the-art or superhuman play
  with policy-gradient self-play on four consumer-class GPUs in days.
  [reported by the research pass as arXiv 2605.19235 and 2606.23348]

## What this implies

- **Candidate method:** Jaxcalibur's recipe, adapted to OU. Train a
  transformer policy and value by PPO self-play, with heads predicting the
  opponent's next action and its sets. At test time, run regret-matching
  search over worlds sampled from those heads. This is the Ataraxos recipe at
  single-GPU scale. The belief model should be easier here than in Stratego,
  because the hidden information is static and factored and usage statistics
  give a prior. With teams drawn
  from the ladder distribution, there is no setup network to train.
- **Cheap baseline and sparring partner:** sample opponent teams from usage
  stats, then run simultaneous-move search with regret matching.
- **Hardware:** the available machine is one RX 9070 XT (16 GB) and one Core
  Ultra 7 265K (20 cores). That is far below Ataraxos's training budget, and a
  slower GPU than the one H100 Jaxcalibur trained on for a week.
  Simulator throughput is the main risk, which is why engine speed matters.
- **What the engine must expose for any of these:**
  - a pure step function, and explicit chance;
  - a per-player view of the state;
  - a world constructor: a full state built from one side's view plus sampled
    opponent sets;
  - a state hash, including a coarse one over bucketed fields for search
    statistics;
  - a damage query that makes no draws, since Jaxcalibur feeds damage
    calculations to its network.
  The Sample interpreter matters first; the Enumerate interpreter is optional
  for this kind of search.
- **Team building:** if the bot builds its own teams, as PokeRus does,
  `parseTeam`'s tested pool limits what it can try. It would also need a team
  legality check, outside the parity contract.
- **Evaluation:** ladder numbers after 2026-10-20 come from an opt-in pool that
  includes other bots. Compare against fixed opponents instead, such as Foul
  Play and Metamon checkpoints.

## Not verified

- Game-size statistics for Gen 9 OU: mean game length, information-set counts,
  how much hidden information is typically revealed. None were found.
- Showdown's own Gen 9 throughput.
- Whether PyTorch or JAX on ROCm works on the RX 9070 XT under Fedora.
- The exact wording of the Jaxcalibur and PokeRus forum posts. Smogon's forum
  blocks scripted fetches, so they were read through a summariser.
- Which thread holds the 2026-10-20 opt-in change. The Ladder policy section
  cites 3789528; the PokeRus research read it in
  [3789822](https://www.smogon.com/forums/threads/3789822/).
- The terms of use for public replay data.
- Whether a new generation will displace Gen 9 OU as the current metagame
  during this project.
