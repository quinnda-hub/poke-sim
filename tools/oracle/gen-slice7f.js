#!/usr/bin/env node
'use strict';
// Generates battles for "slice 7f" (the combined set) and records each on Showdown.
//
// Slice 7 built five mechanics one at a time: the protect family (7a), Substitute (7b), Encore and Taunt (7c), Sucker Punch and
// Thunderclap (7d) and the Choice items with `choicelock` (7e). Each phase's set holds only its own mechanic. This set records their
// interactions. The teams are slice 6's (gen-slice6.js `assign`), with every step-4 move and the Choice items sprinkled on the sets.
// About 30% of the battles are plain random play over those teams; the rest are scripted openings (a pair of choices per decision
// point; the random policy takes over when the script ends, and a script that Showdown rejects is dropped and the battle recorded
// without it):
//   punchprot   Sucker Punch or Thunderclap into Protect, Spiky Shield, Baneful Bunker or Endure (+4 moves first: the target has no
//               move action left), against a stall counter that climbs, and with the target's protect move chosen on a later turn
//   punchsub    the same moves into a Substitute (hit on the Substitute, a move that goes through), a pivot's Substitute and a foe
//               that switches
//   punchstate  the same moves into an encored, a taunted and a Choice-locked target, whose chosen move decides whether Try passes
//   protsub     a Substitute user that then protects, a protect user that then makes a Substitute, with contact and multi-hit attackers
//   protchoice  Spiky Shield and Baneful Bunker against a Choice-locked contact attacker
//   protencore  Encore on a Pokemon that has protected (the stall counter climbs and the move fails or succeeds), then repeated turns
//   prottaunt   Taunt on a Pokemon with protect moves (Struggle when only status moves are left), Taunt before a queued Substitute
//   subbypass   Encore and Taunt on a Substitute user (bypasssub), an encored Substitute user at low HP, a Substitute under Taunt
//   itemsub     Trick, Switcheroo, Knock Off, Thief and a Choice-locked U-turn into a Substitute
//   encchoice   Encore on a Choice-locked target (the encored move is the locked one), a Choice item handed to an encored or taunted
//               Pokemon by Trick, and a Choice holder encored in the turn it receives the item
//   duo         Encore and Taunt on one Pokemon: every move disabled, Struggle
//   endure      Endure against multi-hit moves that break a Substitute (and a lone Endure against a Substitute user)
//   scarf       a Choice Scarf holder (a slower base speed) against priority 1 (Sucker Punch, Thunderclap) and priority 4 (Protect)
//
// Usage:
//   node gen-slice7f.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice7f-<seed>.json, plus slice7f-{moves,species,abilities,items}.json (copies of the pool). A tally of the
// outcomes that distinguish each crossing is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the
// wrappers pass every argument and result through and call nothing the battle would not call).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { BattleActions } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'battle-actions'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');
const { assign: assign6 } = require('./gen-slice6.js');

const dex = Dex.mod('gen9');

function mulberry32(seed) {
	let a = seed >>> 0;
	return function () {
		a = (a + 0x6D2B79F5) >>> 0;
		let t = a;
		t = Math.imul(t ^ (t >>> 15), t | 1);
		t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
		return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
	};
}

const PROTECTS = ['protect', 'spikyshield', 'banefulbunker', 'endure'];
const PUNCHES = ['suckerpunch', 'thunderclap'];
const CHOICE = ['choiceband', 'choicespecs', 'choicescarf'];
const SWAPPERS = ['trick', 'switcheroo'];
const PIVOTS = ['uturn', 'voltswitch', 'flipturn'];
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];
const IDLE = ['recover', 'swordsdance', 'calmmind', 'roost'];
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const isChoice = (item) => CHOICE.includes(item);
const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2246822519 + 3266489917);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	for (const id of [...PROTECTS, ...PUNCHES, ...CHOICE, ...SWAPPERS, 'substitute', 'encore', 'taunt', 'knockoff', 'thief', 'uturn', ...IDLE]) {
		if (!has(id) && !pool.items.includes(id)) throw new Error(`the pool has no ${id}`);
	}
	let scenario = assign6(scenario0, pool, seed);
	const short = (sc) => sc.teams[0].length !== 6 || sc.teams[1].length !== 6;
	for (let k = 1; short(scenario) && k < 30; k++) {
		const s2 = seed + 1000003 * k;
		scenario = assign6(makeScenario(s2, 'gen9ou', pool), pool, s2);
	}
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const mv = (id) => dex.moves.get(id);
	const speciesName = (id) => dex.species.get(id).name;
	const real = pool.species.filter(id => id !== 'shedinja');
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves.splice(int(4), 1, id);
		s.moves = [...new Set(s.moves)].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const setItem = (s, id) => { s.item = itemName(id); };

	// the sprinkle: every step-4 kit, with a Choice item on many sets, so that unscripted play meets their crossings
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 36) put(s, pick(PROTECTS));
		if (int(100) < 28) put(s, pick(PUNCHES));
		if (int(100) < 24) put(s, 'substitute');
		if (int(100) < 26) put(s, 'encore');
		if (int(100) < 26) put(s, 'taunt');
		if (int(100) < 14) put(s, pick(SWAPPERS));
		if (int(100) < 10) put(s, 'knockoff');
		if (int(100) < 12) put(s, pick(PIVOTS));
		if (int(100) < 38) setItem(s, pick(CHOICE));
	}
	const kinds = [
		['random', 30], ['punchprot', 7], ['punchsub', 5], ['punchstate', 8], ['protsub', 5], ['protchoice', 5], ['protencore', 5],
		['prottaunt', 5], ['subbypass', 7], ['itemsub', 6], ['encchoice', 6], ['duo', 3], ['endure', 4], ['scarf', 4],
	];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	scenario.kind = kind;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const duoC = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const full = [`team ${[1, 2, 3, 4, 5, 6].join('')}`, `team ${[1, 2, 3, 4, 5, 6].join('')}`];
	const attacks = pool.moves.filter(id => {
		const m = mv(id);
		return m.category !== 'Status' && m.basePower >= 40 && m.basePower <= 65 && m.accuracy === true || (m.category !== 'Status' && m.basePower >= 40 && m.basePower <= 65 && m.accuracy === 100);
	}).filter(id => {
		const m = mv(id);
		return m.priority === 0 && !m.multihit && !m.selfSwitch && !m.forceSwitch && !m.selfdestruct && !m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge && !m.self && !m.recoil && !m.drain && m.flags.protect;
	});
	const contact = attacks.filter(id => mv(id).flags.contact);
	const multi = pool.moves.filter(id => mv(id).category !== 'Status' && mv(id).multihit && (mv(id).accuracy === true || mv(id).accuracy >= 90) && mv(id).priority === 0 && !mv(id).selfdestruct);
	const strong = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.basePower >= 80 && m.accuracy === 100 && !m.multihit && m.priority === 0 && !m.selfSwitch && !m.forceSwitch && !m.self && !m.selfdestruct && !m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge; });
	const types = (s) => dex.species.get(s.species).types;
	const canHit = (id, s) => dex.getImmunity(mv(id).type, types(s));
	const pickFor = (ids, s) => { const ok = ids.filter(id => canHit(id, s)); return pick(ok.length ? ok : ids); };
	const steady = (s, spe) => {
		s.level = 100; s.ability = 'No Ability'; s.nature = 'Hardy';
		s.evs = { hp: 252, atk: 0, def: 0, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
		if (s.item === 'Assault Vest' || s.item === 'Heavy-Duty Boots') s.item = '';
	};
	// A faster or slower than B (by more than a margin, scarf aside)
	const speeds = (aFaster) => {
		steady(A, aFaster ? 252 : 0); steady(B, 0);
		for (let k = 0; k < 400; k++) {
			const a = pick(real);
			const sa = speStat(a, 100, aFaster ? 252 : 0);
			const bs = real.filter(b => (aFaster ? sa > speStat(b, 100, 0) + 10 : speStat(b, 100, 0) > sa + 10));
			if (bs.length) { A.species = speciesName(a); B.species = speciesName(pick(bs)); return; }
		}
		throw new Error('no speed pair');
	};
	// the first ids in order (slot 1, 2, ...), padded with idle status moves
	const moves = (s, ...ids) => { s.moves = [...new Set([...ids, ...IDLE])].slice(0, 4); };
	const tm = (m) => (int(100) < 20 ? `${m} terastallize` : m);
	const aFaster = int(2) === 0;
	const prot = pick(PROTECTS);
	const punch = pick(PUNCHES);
	const idleAt = 3; // after moves(s, x, y) the idle moves are slots 3 and 4
	const setPlain = () => { for (const s of [A, B]) { setItem(s, ''); } };

	// Every script keeps the Pokemon that Encore, Taunt or a Choice item may constrain on one move (the same slot every turn, or
	// all-status moves, whose slot 1 is Struggle under a Taunt), so that the script stays legal whichever way the speeds fall.
	if (kind === 'punchprot') {
		// B protects (priority 4, so it moves first whatever the speed): the punch finds no move action. Turn 2: B protects again
		// (the stall draw), or attacks.
		speeds(aFaster); setPlain();
		if (int(3) === 0) setItem(B, pick(['rockyhelmet', 'leftovers', 'lifeorb']));
		moves(A, punch, pickFor(attacks, B)); moves(B, prot, pickFor(contact.length ? contact : attacks, A), pickFor(attacks, A));
		scenario.choices = [full, duoC(tm('move 1'), 'move 1'), duoC('move 1', int(2) ? 'move 1' : 'move 2'), duoC('move 1', 'move 1'), duoC('move 2', 'move 1')];
	} else if (kind === 'punchsub') {
		// B makes a Substitute: the punch then hits the Substitute (B attacks, so Try passes), or B uses a status move and it fails
		speeds(int(3) === 0); setPlain();
		moves(A, punch, pickFor(attacks, B)); moves(B, 'substitute', pickFor(attacks, A), pick(IDLE));
		const second = int(3) === 0 ? 'move 3' : 'move 2';
		scenario.choices = [full, duoC('move 2', 'move 1'), duoC('move 1', second), duoC('move 1', 'move 2'),
			int(2) ? duoC('move 1', 'move 2') : duoC('move 1', 'switch 2')];
	} else if (kind === 'punchstate') {
		const v = int(4);
		speeds(int(2) === 0); setPlain();
		if (v === 0) {
			// B is encored into an attack or a status move (it repeats its move 1); the punch works only against an encored attack
			const encAttack = int(2) === 0;
			moves(A, punch, 'encore', pick(IDLE), pickFor(attacks, B));
			moves(B, encAttack ? pickFor(attacks, A) : pick(IDLE), pickFor(attacks, A));
			scenario.choices = [full, duoC('move 3', 'move 1'), duoC('move 2', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('move 4', 'move 1')];
		} else if (v === 1) {
			// B is taunted: an attack goes through Try; B's own status move is not offered (Struggle if every move is status)
			moves(A, punch, 'taunt', pickFor(attacks, B), pick(IDLE));
			if (int(3) === 0) B.moves = [pick(['recover', 'roost']), 'toxic', 'swordsdance', 'calmmind'];
			else moves(B, pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 3', 'move 1'), duoC('move 2', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1')];
		} else if (v === 2) {
			// B is Choice-locked into an attack or into a status move (the lock takes any move)
			setItem(B, pick(CHOICE));
			moves(A, punch, pickFor(attacks, B));
			moves(B, int(2) === 0 ? pick(IDLE) : pickFor(attacks, A), pickFor(attacks, A));
			scenario.choices = [full, duoC('move 2', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('move 2', 'move 1')];
		} else {
			// A, a puncher, is encored by B after its punch has been used (priority 1 moves first, so the override cannot take the punch's
			// place): A is encored into the punch itself and repeats it, whether it fails or works
			speeds(false);
			moves(A, punch, pickFor(attacks, B)); moves(B, 'encore', pickFor(attacks, A));
			scenario.choices = [full, duoC('move 1', 'move 2'), duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2')];
		}
	} else if (kind === 'protsub') {
		speeds(aFaster); setPlain();
		const atk = int(3) === 0 && multi.length ? pickFor(multi, A) : pickFor(contact.length ? contact : attacks, A);
		moves(A, 'substitute', prot, pickFor(attacks, B)); moves(B, atk, pickFor(attacks, A));
		const order = int(2) === 0;
		scenario.choices = [full, duoC(order ? 'move 1' : 'move 2', 'move 1'), duoC(order ? 'move 2' : 'move 1', 'move 1'), duoC('move 2', 'move 1'), duoC('move 2', 'move 2'), duoC('move 1', 'move 1')];
	} else if (kind === 'protchoice') {
		speeds(aFaster); setPlain();
		setItem(B, pick(CHOICE));
		const shield = pick(['spikyshield', 'banefulbunker']);
		moves(A, shield, pickFor(attacks, B), 'toxic'); moves(B, pickFor(contact.length ? contact : attacks, A), pickFor(attacks, A));
		scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC(int(2) ? 'move 1' : 'move 2', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1')];
	} else if (kind === 'protencore') {
		// B (faster) encores A, which protected last turn and now chose an attack: the override makes it a protect move again
		speeds(false); setPlain();
		moves(A, pick(PROTECTS), pickFor(attacks, B)); moves(B, 'encore', pickFor(attacks, A), pick(IDLE));
		scenario.choices = [full, duoC('move 1', 'move 3'), duoC('move 2', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2')];
	} else if (kind === 'prottaunt') {
		speeds(!aFaster); setPlain();
		if (int(2) === 0) {
			// A (protect user, status moves only) is taunted by the faster B: Struggle, which Protect blocks as any move; the Taunt wears off
			A.moves = [prot, 'recover', 'toxic', 'swordsdance'];
			moves(B, 'taunt', pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2')];
		} else {
			// A queues a Substitute (a status move), and the faster B taunts: it is blocked in BeforeMove; then A attacks (move 3)
			moves(A, 'substitute', prot, pickFor(attacks, B)); moves(B, 'taunt', pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 3', 'move 2'), duoC('move 3', 'move 2')];
		}
	} else if (kind === 'subbypass') {
		const v = int(3);
		speeds(int(2) === 0); setPlain();
		if (v === 0) {
			// Encore and Taunt reach a Substitute holder; A repeats its Substitute (all A's moves are status: Struggle once taunted)
			moves(A, 'substitute', 'toxic'); moves(B, 'encore', 'taunt', pickFor(attacks, A));
			scenario.choices = [full, duoC('move 1', 'move 3'), duoC('move 1', 'move 1'), duoC('move 1', 'move 3'), duoC('move 1', 'move 2'), duoC('move 1', 'move 3'), duoC('move 1', 'move 3')];
		} else if (v === 1) {
			// an encored Substitute user at low HP: A is hit hard, then encored into Substitute, which fails or costs a quarter
			moves(A, 'substitute', 'toxic'); moves(B, 'encore', pickFor(strong, A), pickFor(strong, A));
			scenario.choices = [full, duoC('move 1', 'move 2'), duoC('move 1', 'move 2'), duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 3'), duoC('move 1', 'move 3')];
		} else {
			// a Substitute under Taunt fails when the Taunt comes first; the Substitute made before the Taunt stays
			moves(A, 'substitute', 'toxic'); moves(B, 'taunt', pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2')];
		}
	} else if (kind === 'itemsub') {
		const v = int(4);
		speeds(int(2) === 0); setPlain();
		moves(B, 'substitute', pickFor(attacks, A), pick(IDLE)); setItem(B, pick(['', 'leftovers', 'lifeorb', ...CHOICE]));
		const mine = v === 0 ? pick(SWAPPERS) : v === 1 ? 'knockoff' : v === 2 ? 'thief' : pick(PIVOTS);
		if (v === 3) {
			setItem(A, pick(CHOICE));
			if (!canHit(mine, B)) B.species = speciesName(pick(real.filter(id => !dex.species.get(id).types.includes('Ground'))));
		} else setItem(A, v === 2 ? '' : pick(['leftovers', 'lifeorb', ...CHOICE]));
		moves(A, mine, pickFor(attacks, B), pick(IDLE));
		// A repeats its move 1 (a Choice lock on it is no trouble); B repeats its Substitute (a lock on it is none either)
		scenario.choices = [full, duoC('move 1', 'move 1'), v === 3 ? duoC('switch 2', null) : duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1')];
	} else if (kind === 'encchoice') {
		const v = int(2);
		speeds(int(2) === 0); setPlain();
		const inner = int(2) === 0 ? 'encore' : 'taunt';
		if (v === 0) {
			// a Choice-locked target is encored into the move it is locked into (the lock and the encore agree)
			setItem(A, pick(CHOICE));
			moves(A, pickFor(attacks, B), pickFor(attacks, B)); moves(B, inner, pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 1', 'move 3'), duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 1')];
		} else if (v === 1) {
			// A (no item) encores or taunts B (Choice-locked on its move 1), takes the Choice item by Trick and hands it back: the
			// encored or taunted B receives a Choice item
			setItem(B, pick(CHOICE)); setItem(A, '');
			moves(A, inner, pick(SWAPPERS), pickFor(attacks, B)); moves(B, pickFor(attacks, A), pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 3', 'move 1'), duoC('move 1', 'move 1'), duoC('move 2', 'move 1'), duoC('move 2', 'move 1'), duoC('move 3', 'move 1')];
		}
	} else if (kind === 'duo') {
		// Encore and Taunt on one Pokemon: encored into a status move, then taunted: every move disabled (A repeats move 1)
		speeds(!aFaster); setPlain();
		A.moves = [pick(IDLE), 'toxic', 'substitute', prot];
		moves(B, 'encore', 'taunt', pickFor(attacks, A));
		const order = int(2) === 0;
		scenario.choices = [full, duoC('move 1', 'move 3'), duoC('move 1', order ? 'move 1' : 'move 2'), duoC('move 1', order ? 'move 2' : 'move 1'), duoC('move 1', 'move 3'), duoC('move 1', 'move 3'), duoC('move 1', 'move 3')];
	} else if (kind === 'endure') {
		speeds(aFaster); setPlain();
		const m = multi.length ? pickFor(multi, A) : pickFor(attacks, A);
		moves(A, 'substitute', 'endure'); moves(B, m, pickFor(attacks, A));
		scenario.choices = int(3) === 0 ? [full, duoC('move 1', 'move 2'), duoC('move 2', 'move 1')]
			: [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 1'), duoC('move 1', 'move 1'), duoC('move 2', 'move 1')];
	} else if (kind === 'scarf') {
		// the Scarf holder has the lower base speed, so a Scarf makes it the faster one: against priority 1 and priority 4 (A repeats move 1)
		speeds(false); setPlain();
		setItem(A, 'choicescarf');
		const v = int(3);
		if (v === 0) { moves(A, pickFor(attacks, B)); moves(B, punch, pickFor(attacks, A)); }
		else if (v === 1) { moves(A, punch); moves(B, pickFor(attacks, A), prot); }
		else { moves(A, prot); moves(B, punch, pickFor(attacks, A)); }
		scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 1', v === 1 ? 'move 2' : 'move 1'), duoC('move 1', v === 1 ? 'move 2' : 'move 1'), duoC('move 1', 'move 1')];
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// The tally. Every wrapper passes its arguments and result through.

function newTally() {
	return {
		punchTry: { works: 0, noAction: 0, statusMove: 0 },
		punchTarget: { protecting: 0, endured: 0, substitute: 0, encored: 0, taunted: 0, choiceLocked: 0, choiceLockedStatus: 0 },
		punchWorksOnSub: 0,
		punchUserScarf: 0,
		protectBlocks: { fromChoiceLocked: 0, spikyOnLocked: 0, baneOnLocked: 0, encoreOrTaunt: 0 },
		protectBehindSub: 0,
		stallRoll: { whileEncored: 0, failedWhileEncored: 0, climbedUnderEncore: 0 },
		override: { toStatus: 0, toAttack: 0, protectOverStatus: 0 },
		tauntBlocked: { substitute: 0, other: 0 },
		subFailed: { already: 0, lowHp: 0, whileEncored: 0 },
		bypass: { encoreOnSub: 0, tauntOnSub: 0 },
		itemIntoSub: { trick: 0, knockoff: 0, thief: 0, uturnLocked: 0 },
		encoreOnLocked: 0,
		trickChoiceToEncored: 0,
		trickChoiceToTaunted: 0,
		struggleEncoreTaunt: 0,
		endureMulti: { subBroken: 0, saved: 0 },
		scarfWithPriority: { priority1: 0, priority4: 0 },
	};
}
let activeTally = null;
const t = () => activeTally;
const protectsVol = (p) => !!(p.volatiles.protect || p.volatiles.spikyshield || p.volatiles.banefulbunker);

function install() {
	// the punches' onTry, with the target's state
	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, source, sourceEffect, ...rest) {
		const c = t();
		if (c && eventid === 'Try' && effect && PUNCHES.includes(effect.id) && source && target) {
			const foe = source;
			const action = this.queue.willMove(foe);
			if (!action) c.punchTry.noAction++;
			else if (action.move.category === 'Status') c.punchTry.statusMove++;
			else c.punchTry.works++;
			if (protectsVol(foe)) c.punchTarget.protecting++;
			if (foe.volatiles.endure) c.punchTarget.endured++;
			if (foe.volatiles.substitute) c.punchTarget.substitute++;
			if (foe.volatiles.encore) c.punchTarget.encored++;
			if (foe.volatiles.taunt) c.punchTarget.taunted++;
			if (foe.volatiles.choicelock) {
				c.punchTarget.choiceLocked++;
				if (action && action.move.category === 'Status') c.punchTarget.choiceLockedStatus++;
			}
			if (target.item === 'choicescarf') c.punchUserScarf++;
			if (action && action.move.category !== 'Status' && foe.volatiles.substitute) c.punchWorksOnSub++;
		}
		return singleEvent.call(this, eventid, effect, state, target, source, sourceEffect, ...rest);
	};
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, ...rest) {
		const c = t();
		if (c && eventid === 'StallMove' && target && target.volatiles && target.volatiles.encore && target.volatiles.stall) {
			const counter = target.volatiles.stall.counter;
			const r = runEvent.call(this, eventid, target, ...rest);
			c.stallRoll.whileEncored++;
			if (!r) c.stallRoll.failedWhileEncored++;
			if (counter >= 9) c.stallRoll.climbedUnderEncore++;
			return r;
		}
		if (c && eventid === 'OverrideAction' && target) {
			const r = runEvent.call(this, eventid, target, ...rest);
			if (typeof r === 'string') {
				const chosen = rest[1];
				const to = this.dex.moves.get(r);
				if (to.category === 'Status') c.override.toStatus++; else c.override.toAttack++;
				if (chosen && PROTECTS.includes(to.id) && chosen.category !== 'Status') c.override.protectOverStatus++;
			}
			return r;
		}
		if (c && eventid === 'Damage' && target && target.volatiles && target.volatiles.endure && this.activeMove && this.activeMove.multihit) {
			const hp = target.hp;
			const r = runEvent.call(this, eventid, target, ...rest);
			if (rest[2] >= hp) c.endureMulti.saved++;
			return r;
		}
		return runEvent.call(this, eventid, target, ...rest);
	};
	const add = Battle.prototype.add;
	Battle.prototype.add = function (...args) {
		const c = t();
		if (c) {
			if (args[0] === '-fail' && args[2] === 'move: Substitute' && args[1] && args[1].volatiles) {
				if (args[3] === '[weak]') c.subFailed.lowHp++; else c.subFailed.already++;
				if (args[1].volatiles.encore) c.subFailed.whileEncored++;
			}
			if (args[0] === 'cant' && args[2] === 'move: Taunt') {
				const id = dex.moves.get(args[3]).id;
				if (id === 'substitute') c.tauntBlocked.substitute++;
				else c.tauntBlocked.other++;
			}
		}
		return add.call(this, ...args);
	};
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, ...rest) {
		const c = t();
		const id = typeof status === 'string' ? status : status.id;
		const had = c && (id === 'encore' || id === 'taunt') && this.volatiles.substitute;
		const r = addVolatile.call(this, status, ...rest);
		if (c && r && had) c.bypass[id === 'encore' ? 'encoreOnSub' : 'tauntOnSub']++;
		if (c && r && id === 'encore' && this.volatiles.choicelock) c.encoreOnLocked++;
		if (c && r && id === 'substitute' && protectsVol(this)) c.subBehindProtect++;
		if (c && r && PROTECTS.includes(id) && this.volatiles.substitute) c.protectBehindSub++;
		return r;
	};
	const removeVolatile = Pokemon.prototype.removeVolatile;
	Pokemon.prototype.removeVolatile = function (status, ...rest) {
		const c = t();
		const id = typeof status === 'string' ? status : status.id;
		if (c && id === 'substitute' && this.volatiles.substitute && this.volatiles.endure && this.battle.activeMove && this.battle.activeMove.multihit) c.endureMulti.subBroken++;
		return removeVolatile.call(this, status, ...rest);
	};
	// the block itself
	const checkMoveBypassesProtect = Battle.prototype.checkMoveBypassesProtect;
	Battle.prototype.checkMoveBypassesProtect = function (move, attacker, defender, ...rest) {
		const r = checkMoveBypassesProtect.call(this, move, attacker, defender, ...rest);
		const c = t();
		const id = this.effect && this.effect.id;
		if (c && !r && PROTECTS.concat([]).includes(id)) {
			if (attacker.volatiles.choicelock) {
				c.protectBlocks.fromChoiceLocked++;
				if (id === 'spikyshield' && move.flags.contact) c.protectBlocks.spikyOnLocked++;
				if (id === 'banefulbunker' && move.flags.contact) c.protectBlocks.baneOnLocked++;
			}
			if (move.id === 'encore' || move.id === 'taunt') c.protectBlocks.encoreOrTaunt++;
		}
		return r;
	};
	const useMove = BattleActions.prototype.useMove;
	BattleActions.prototype.useMove = function (move, pokemon, ...rest) {
		const c = t();
		const id = typeof move === 'string' ? move : move && move.id;
		if (c && id === 'struggle' && pokemon && pokemon.volatiles.encore && pokemon.volatiles.taunt) c.struggleEncoreTaunt++;
		return useMove.call(this, move, pokemon, ...rest);
	};
	const runMove = BattleActions.prototype.runMove;
	BattleActions.prototype.runMove = function (moveOrMoveName, pokemon, targetLoc, options) {
		const c = t();
		const id = typeof moveOrMoveName === 'string' ? moveOrMoveName : moveOrMoveName && moveOrMoveName.id;
		if (c && pokemon.item === 'choicescarf') {
			const p = dex.moves.get(id).priority;
			if (p === 1) c.scarfWithPriority.priority1++;
			if (p === 4) c.scarfWithPriority.priority4++;
		}
		if (c && SWAPPERS.includes(id)) {
			const foe = pokemon.side.foe.active[0];
			const foeItem = foe && foe.item;
			const r = runMove.call(this, moveOrMoveName, pokemon, targetLoc, options);
			if (foe && foe.volatiles.substitute && foe.item === foeItem) c.itemIntoSub.trick++;
			const theirs = foeItem;
			if (foe && isChoice(pokemon.item) === false && isChoice(foe.item) && foe.item !== theirs) {
				if (foe.volatiles.encore) c.trickChoiceToEncored++;
				if (foe.volatiles.taunt) c.trickChoiceToTaunted++;
			}
			return r;
		}
		if (c && foeSub(pokemon) && (id === 'knockoff' || id === 'thief')) c.itemIntoSub[id]++;
		if (c && PIVOTS.includes(id) && foeSub(pokemon) && pokemon.volatiles.choicelock) c.itemIntoSub.uturnLocked++;
		return runMove.call(this, moveOrMoveName, pokemon, targetLoc, options);
	};
}
const foeSub = (p) => { const foe = p.side.foe.active[0]; return !!(foe && foe.volatiles.substitute); };

// counters that cannot be hit by construction or are rare by nature
// - subBehindProtect, override over a queued punch and a Choice-locked override (the `-fail` of choicelock's onBeforeMove) are not counted:
//   a protect move (priority 4) and a Substitute (0) cannot be used by one Pokemon in a turn; a punch (priority 1) always runs before
//   the Encore (0) that could replace it, so Encore takes it only on the turn after, by the lastMove; and an encored move is the
//   last move, which a Choice lock was added for in the same step, so the lock and the encore always agree.
const STRUCTURAL_ZERO = [];

function recordScenario(scenario) {
	const before = JSON.stringify(activeTally);
	try {
		return { trace: record(scenario), scripted: !!scenario.choices };
	} catch (e) {
		if (!scenario.choices) throw e;
		activeTally = JSON.parse(before);
		const plain = Object.assign({}, scenario);
		delete plain.choices;
		return { trace: record(plain), scripted: false, dropped: e.message };
	}
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice7f.js <outDir> <count> <poolFile> [firstSeed=1]');
		process.exit(2);
	}
	const count = parseInt(countArg, 10), first = parseInt(firstArg || '1', 10);
	const pool = JSON.parse(fs.readFileSync(poolFile, 'utf8'));
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		if (!Array.isArray(pool[k])) throw new Error(`bad pool file ${poolFile}: no ${k}`);
	}
	for (const id of pool.moves) if (!dex.moves.get(id).exists) throw new Error(`unknown move ${id}`);
	for (const id of pool.species) if (!dex.species.get(id).exists) throw new Error(`unknown species ${id}`);
	for (const id of pool.abilities) if (id !== 'noability' && !dex.abilities.get(id).exists) throw new Error(`unknown ability ${id}`);
	for (const id of pool.items) if (id !== '' && !dex.items.get(id).exists) throw new Error(`unknown item ${id}`);
	fs.mkdirSync(outDir, { recursive: true });
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		fs.writeFileSync(path.join(outDir, `slice7f-${k}.json`), JSON.stringify(pool[k]));
	}
	activeTally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scriptedCount = 0, dropped = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scriptedCount++;
		if (result.dropped) { dropped++; if (process.env.SLICE7F_VERBOSE) console.error(`seed ${seed}: script dropped (${scenario.kind}, turn ${(scenario.choices||[]).length}): ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice7f-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice7f seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error('tally (from Showdown\'s own events):');
	const zeros = [];
	const walk = (prefix, v) => {
		if (typeof v === 'number') { if (v === 0 && !STRUCTURAL_ZERO.includes(prefix)) zeros.push(prefix); return; }
		for (const [k, x] of Object.entries(v)) walk(prefix ? `${prefix}.${k}` : k, x);
	};
	for (const [k, v] of Object.entries(activeTally)) {
		console.error(`  ${k}: ${JSON.stringify(v)}`);
		walk(k, v);
	}
	if (zeros.length) console.error(`ZERO COUNTERS (not structural): ${zeros.join(', ')}`);
	else console.error('no counter is zero except the structural ones');
}

module.exports = { assign, install, newTally, setTally: (x) => { activeTally = x; } };
