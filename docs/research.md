# Research notes

What is known about prior work, and what it implies for this project. Gathered
on 2026-11-25. These are notes, not decisions; decisions are in
[architecture.md](architecture.md) and [roadmap.md](roadmap.md).

How far each claim was checked:

- **[checked]** read at the primary source and re-checked for this document.
- **[read]** read at the primary source during the research pass, not
  re-checked.
- **[reported]** self-reported by an author on a forum, or seen only in a
  summary or abstract.

## Where competitive Pokémon AI stands

No agent has a verified top-10 result in Gen 9 OU. The organisers of the
NeurIPS 2025 PokéAgent Challenge describe the state of the art as "less than
superhuman". [read]

| System | Method | Hidden information | Strength |
|---|---|---|---|
| [Foul Play](https://pmariglia.github.io/posts/foul-play) | Root-parallel MCTS with decoupled UCT and a hand-written evaluation, on the Rust `poke-engine` | Samples full opponent sets from usage stats, forum teams and replays; narrows them by inference | Gen 9 OU: 80% GXE, peak 1879, top 100. Gen 3 OU: 71% GXE, peak 1815, rank 4. Won the PokéAgent Gen 9 OU track 50–14. [checked, as of May 2025] |
| [Metamon](https://arxiv.org/abs/2504.04395) | Offline RL with transformers, no search | Implicit, from the history | Best public model (Dec 2025): GXE 82% Gen 1, 63% Gen 3, 71% Gen 9. Gen 1 ensembles have held #1 on the human ladder. [read] |
| [PokéChamp](https://arxiv.org/abs/2503.04094) | LLM-driven minimax | LLM prediction | Projected Elo 1300–1500 in Gen 9 OU. [read] |
| Jett Wang, [MIT thesis](https://dspace.mit.edu/handle/1721.1/153888) | PPO actor-critic guiding MCTS | — | Gen 4 random battles, peak rank 8 at 1693. [reported, abstract only] |

So the strongest verified approach in OU is search with a hand-written
evaluation. What it lacks:

- A learned evaluation.
- Equilibrium mixing. Decoupled UCT has no convergence guarantee in
  simultaneous-move games; regret matching and Exp3 do
  ([Lisý et al. 2013](https://arxiv.org/abs/1310.8613)). [reported]
- Sound reasoning about hidden information. Sampling opponent sets cannot
  reason about concealing your own sets or about what the opponent believes.

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

- **Candidate method:** the Ataraxos recipe at reduced scale. The belief
  network should be easier here than in Stratego because the hidden information
  is static and factored, and usage statistics give a prior. With teams drawn
  from the ladder distribution, there is no setup network to train.
- **Cheap baseline and sparring partner:** sample opponent teams from usage
  stats, then run simultaneous-move search with regret matching.
- **Hardware:** the available machine is one RX 9070 XT (16 GB) and one Core
  Ultra 7 265K (20 cores). That is far below Ataraxos's training budget.
  Simulator throughput is the main risk, which is why engine speed matters.
- **What the engine must expose for any of these:** a pure step function,
  explicit chance, and a per-player view of the state.

## Not verified

- Game-size statistics for Gen 9 OU: mean game length, information-set counts,
  how much hidden information is typically revealed. None were found.
- Showdown's own Gen 9 throughput.
- Whether PyTorch on ROCm works on the RX 9070 XT under Fedora.
- The terms of use for public replay data.
- Whether a new generation will displace Gen 9 OU as the current metagame
  during this project.
