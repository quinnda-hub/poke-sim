#!/usr/bin/env node
'use strict';
// Generates battles for "slice 8a" (stat stages and switch-in abilities) and records each on Showdown.
//
// Slice 8a lets in fourteen abilities that act on stat stages or at a switch-in: Intimidate (`onStart`: the foe's Attack -1, a Substitute makes
// the foe immune), Dauntless Shield (`onStart`: Defense +1, once per battle for each Pokemon), the abilities of `battle.boost`'s events
// (Defiant `onAfterEachBoost`, Contrary `onChangeBoost`, Clear Body, Big Pecks, Keen Eye, Inner Focus, Guard Dog and Mirror Armor `onTryBoost`),
// Guard Dog's `onDragOut` and Inner Focus's flinch immunity, Keen Eye's `ignoreEvasion`, the hit reactions (Weak Armor, Justified, Gooey:
// `onDamagingHit`) and Speed Boost (`onResidual`, which reads `activeTurns`).
//
// The teams are slice 6's (gen-slice6.js `assign`: slice 1's teams under the hazards, weather, terrain, Paradox and pivots of slices 3 to 6),
// with one of the fourteen abilities on 40% of the sets and the moves that change stages (self-drops, boosts, drops on the foe, Sticky Web) and the
// forcing and pivoting moves on many. About 28% of the battles are plain random play over those teams; the rest are scripted openings (a pair of
// choices per decision point; the random policy takes over when the script ends, and a script that Showdown rejects is dropped and the battle
// recorded without it):
//   intimtry     Intimidate (at the battle start, and on a switch back in) into a holder of Clear Body, Big Pecks, Inner Focus, Guard Dog, Mirror
//                Armor, Defiant, Contrary or Keen Eye (or nothing): blocked, turned into +1, reflected, or taken
//   mirror       a Mirror Armor holder against drops from a Clear Body, Defiant, Contrary, Big Pecks or Keen Eye source (the reflected drop meets the
//                source's own ability), or from an Intimidate source
//   doubleintim  Intimidate on both leads at the battle start (a speed tie when the leads match), then both switch to a second Intimidate holder
//   introsub     Intimidate against a Substitute (the switch-in is immune)
//   midturn      Intimidate on a switch-in in the middle of a turn: after a U-turn, and after the foe's lead faints
//   contrary     a Contrary holder with self-drops (Close Combat, Superpower, Draco Meteor, Leaf Storm, Overheat) and boosting moves, against drops
//                and an Intimidate foe
//   defiant      a Defiant holder against Sticky Web (on a switch back in), drops from the foe and an Intimidate foe, but not its own self-drops
//   shield       Dauntless Shield leaving and coming back (no second boost), or dragged out and in by a forcing move
//   weakarmor    a Weak Armor holder hit by multi-hit physical moves until its Defense and Speed reach -6 and +6
//   justified    a Justified holder hit by Dark moves (and behind a Substitute)
//   gooey        a Gooey holder hit by contact moves from a Clear Body, Contrary, Defiant, Mirror Armor or Big Pecks attacker
//   speedboost   Speed Boost on the first turn (no boost), later, and after a switch out and back in
//   guarddog     Guard Dog against Roar, Whirlwind, Dragon Tail and Circle Throw, an Intimidate foe, and a Red Card in front of a Guard Dog attacker
//   keeneye      Keen Eye against accuracy drops and evasion boosts, and attacking a foe with evasion boosts (the accuracy check ignores evasion)
//   innerfocus   Inner Focus against flinching moves (and Intimidate)
//
// Usage:
//   node gen-slice8a.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice8a-<seed>.json, plus slice8a-{moves,species,abilities,items}.json (copies of the pool). A tally of the branches each
// ability took is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers pass every argument and
// result through and call nothing the battle would not call).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
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

const ABILITIES = ['intimidate', 'dauntlessshield', 'defiant', 'contrary', 'clearbody', 'guarddog', 'innerfocus', 'bigpecks', 'mirrorarmor',
	'keeneye', 'weakarmor', 'justified', 'gooey', 'speedboost'];
const HOLDERS = ['clearbody', 'bigpecks', 'innerfocus', 'guarddog', 'mirrorarmor', 'defiant', 'contrary', 'keeneye'];
const SELF_DROPS = ['closecombat', 'superpower', 'dracometeor', 'leafstorm', 'overheat'];
const BOOSTERS = ['swordsdance', 'dragondance', 'calmmind', 'nastyplot', 'bulkup', 'shellsmash', 'agility', 'workup'];
const DROPS = ['icywind', 'snarl', 'acidspray', 'screech', 'growl', 'tailwhip', 'leer', 'sandattack', 'charm', 'faketears', 'metalsound', 'mudslap',
	'moonblast', 'featherdance'];
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];
const FLINCHERS = ['airslash', 'ironhead', 'rockslide', 'headbutt', 'zenheadbutt', 'waterfall', 'extrasensory'];
const MULTI_PHYSICAL = ['bulletseed', 'iciclespear', 'rockblast', 'tailslap', 'pinmissile', 'furyswipes', 'armthrust', 'doublekick', 'bonerush'];
const DARK = ['bite', 'crunch', 'darkpulse', 'knockoff', 'suckerpunch', 'snarl', 'nightslash', 'foulplay', 'darkestlariat'];
const PIVOTS = ['uturn', 'flipturn', 'voltswitch'];
const IDLE = ['recover', 'swordsdance', 'calmmind', 'roost'];
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 3266489917 + 668265263);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	for (const id of ABILITIES) if (!pool.abilities.includes(id)) throw new Error(`the pool has no ability ${id}`);
	for (const id of [...SELF_DROPS.slice(0, 3), 'swordsdance', 'icywind', 'snarl', 'stickyweb', 'roar', 'whirlwind', 'dragontail', 'circlethrow', 'uturn',
		'substitute', 'doubleteam', 'sandattack', 'recover', 'bulletseed', 'airslash', 'ironhead', 'knockoff', 'crunch']) {
		if (!has(id)) throw new Error(`the pool has no ${id}`);
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
	const setAb = (s, id) => { s.ability = id === '' || id === 'noability' ? 'No Ability' : dex.abilities.get(id).name; };

	// the sprinkle: one of the fourteen abilities on 40% of the sets, and the moves that meet them on many
	const kit = [...SELF_DROPS, ...BOOSTERS, ...DROPS, ...FORCERS, ...PIVOTS, 'stickyweb', 'substitute', 'doubleteam', 'knockoff', 'crunch'].filter(has);
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 40) setAb(s, pick(ABILITIES));
		if (int(100) < 30) put(s, pick(kit));
		if (int(100) < 12) put(s, pick(FORCERS));
		if (int(100) < 10) put(s, 'stickyweb');
		if (int(100) < 12) setItem(s, pick(['redcard', 'ejectbutton', 'rockyhelmet', 'leftovers'].filter(id => pool.items.includes(id))));
	}
	const kinds = [
		['random', 28], ['intimtry', 8], ['mirror', 5], ['doubleintim', 4], ['introsub', 5], ['midturn', 6], ['contrary', 7], ['defiant', 6],
		['shield', 4], ['weakarmor', 5], ['justified', 4], ['gooey', 4], ['speedboost', 4], ['guarddog', 4], ['keeneye', 3], ['innerfocus', 3],
	];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	scenario.kind = kind;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const A2 = scenario.teams[aIdx][1], B2 = scenario.teams[bIdx][1];
	const duoC = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const full = [`team ${[1, 2, 3, 4, 5, 6].join('')}`, `team ${[1, 2, 3, 4, 5, 6].join('')}`];
	const attacks = pool.moves.filter(id => {
		const m = mv(id);
		return m.category !== 'Status' && m.basePower >= 40 && m.basePower <= 65 && (m.accuracy === true || m.accuracy === 100);
	}).filter(id => {
		const m = mv(id);
		return m.priority === 0 && !m.multihit && !m.selfSwitch && !m.forceSwitch && !m.selfdestruct && !m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge && !m.self && !m.recoil && !m.drain && m.flags.protect;
	});
	const contact = attacks.filter(id => mv(id).flags.contact);
	const strong = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.basePower >= 80 && m.accuracy === 100 && !m.multihit && m.priority === 0 && !m.selfSwitch && !m.forceSwitch && !m.self && !m.selfdestruct && !m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge; });
	const lowAcc = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.accuracy !== true && m.accuracy >= 70 && m.accuracy <= 85 && !m.multihit && m.priority === 0 && !m.selfSwitch && !m.forceSwitch && !m.selfdestruct && !m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge; });
	const types = (s) => dex.species.get(s.species).types;
	const canHit = (id, s) => dex.getImmunity(mv(id).type, types(s));
	const pickFor = (ids, s) => { const ok = ids.filter(id => canHit(id, s)); return pick(ok.length ? ok : ids); };
	const present = (ids) => ids.filter(has);
	const steady = (s, spe) => {
		s.level = 100; s.ability = 'No Ability'; s.nature = 'Hardy';
		s.evs = { hp: 252, atk: 0, def: 0, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
		if (s.item === 'Assault Vest' || s.item === 'Heavy-Duty Boots') s.item = '';
		if (s.species === 'Shedinja') s.species = speciesName(pick(real));
	};
	// A faster or slower than B (by more than a margin)
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
	const tm = (m) => (int(100) < 15 ? `${m} terastallize` : m);
	const setPlain = () => { for (const s of [A, B, A2, B2]) { setItem(s, ''); } };
	const aFaster = int(2) === 0;
	const cycle = (n, f) => Array.from({ length: n }, (_, i) => f(i));
	const benchAbility = (side, id) => { for (let i = 1; i < 6; i++) if (int(100) < 55) setAb(scenario.teams[side][i], id); };
	const holder = () => pick(HOLDERS.concat(['']));
	const steadyBench = (...ss) => { for (const s of ss) { s.level = 100; s.evs = { hp: 252, atk: 0, def: 0, spa: 0, spd: 0, spe: 0 }; if (s.species === 'Shedinja') s.species = speciesName(pick(real)); } };

	if (kind === 'intimtry') {
		// A (Intimidate on the lead and the first bench slot) switches out and back; B holds one of the abilities that meet Intimidate
		speeds(aFaster); setPlain(); steadyBench(A2);
		setAb(A, 'intimidate'); setAb(A2, 'intimidate'); setAb(B, holder());
		moves(A, pickFor(attacks, B), pickFor(attacks, B)); moves(B, pickFor(attacks, A), pick(IDLE));
		scenario.choices = [full, duoC(tm('move 1'), 'move 1'), duoC('switch 2', 'move 1'), duoC('switch 2', 'move 1'), duoC('move 1', 'move 1'), duoC('switch 2', 'move 1'), duoC('switch 2', 'move 1')];
	} else if (kind === 'mirror') {
		// B holds Mirror Armor and idles; A drops its stats, and Mirror Armor reflects each drop onto A (where its ability answers)
		speeds(aFaster); setPlain();
		const src = pick(['clearbody', 'defiant', 'contrary', 'bigpecks', 'keeneye', 'intimidate', 'mirrorarmor', '']);
		setAb(A, src); setAb(B, 'mirrorarmor'); setAb(B2, 'mirrorarmor');
		const ds = [...new Set(cycle(8, () => pick(present(DROPS))))].slice(0, 4);
		while (ds.length < 4) ds.push(pick(present(DROPS)));
		A.moves = [...new Set(ds)];
		while (A.moves.length < 4) A.moves.push(pick(present(DROPS)));
		A.moves = [...new Set(A.moves)];
		if (A.moves.length < 4) A.moves.push(...IDLE.slice(0, 4 - A.moves.length));
		moves(B, 'recover', pickFor(attacks, A));
		scenario.choices = [full, ...cycle(9, (i) => duoC(`move ${(i % 4) + 1}`, i === 5 ? 'move 2' : 'move 1'))];
	} else if (kind === 'doubleintim') {
		steady(A, 0); steady(B, 0); steadyBench(A2, B2);
		if (int(2) === 0) { B.species = A.species; B.nature = A.nature; B.evs = { ...A.evs }; B.ivs = { ...A.ivs }; }
		else { const o = pick(real); B.species = speciesName(o); }
		setPlain(); setAb(A, 'intimidate'); setAb(B, 'intimidate'); setAb(A2, 'intimidate'); setAb(B2, 'intimidate');
		moves(A, pickFor(attacks, B), pick(IDLE)); moves(B, pickFor(attacks, A), pick(IDLE));
		scenario.choices = [full, duoC('move 1', 'move 1'), duoC('switch 2', 'switch 2'), duoC('move 1', 'move 1'), duoC('switch 2', 'move 1'), duoC('move 1', 'move 1')];
	} else if (kind === 'introsub') {
		// B makes a Substitute; A's bench member with Intimidate switches in against it (immune), and again after the Substitute breaks or not
		speeds(aFaster); setPlain(); steadyBench(A2);
		setAb(A, int(3) === 0 ? 'clearbody' : ''); setAb(A2, 'intimidate'); setAb(B, int(3) === 0 ? 'defiant' : '');
		moves(A, pickFor(attacks, B), pick(IDLE)); moves(B, 'substitute', pickFor(attacks, A), pick(IDLE));
		scenario.choices = [full, duoC('move 1', 'move 1'), duoC('switch 2', int(2) ? 'move 2' : 'move 1'), duoC('move 1', 'move 3'), duoC('switch 2', 'move 3'), duoC('switch 2', 'move 3')];
	} else if (kind === 'midturn') {
		setPlain();
		if (int(2) === 0) {
			// A pivots (U-turn, Flip Turn or Volt Switch) to a bench member with Intimidate
			speeds(aFaster); steadyBench(A2, B2);
			const pivot = pick(PIVOTS);
			setAb(A, ''); setAb(A2, 'intimidate'); setAb(B, holder());
			moves(A, pivot, pickFor(attacks, B)); moves(B, pickFor(attacks, A), pick(IDLE));
			if (!canHit(pivot, B)) B.species = speciesName(pick(real.filter(id => canHit(pivot, { species: speciesName(id) }))));
			scenario.choices = [full, duoC(tm('move 1'), 'move 1'), duoC('switch 2', null), duoC('move 2', 'move 1'), duoC('move 1', 'move 1')];
		} else {
			// B's lead (level 1) is knocked out by A; its replacement, with Intimidate, comes in at the end of the turn
			speeds(aFaster); steadyBench(A2, B2);
			B.level = 1; setAb(A, holder()); setAb(B, ''); setAb(B2, 'intimidate');
			moves(A, pickFor(strong, B), pickFor(attacks, B)); moves(B, pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC(null, 'switch 2'), duoC('move 2', 'move 1'), duoC('move 2', 'move 1')];
		}
	} else if (kind === 'contrary') {
		// A (Contrary) uses self-drops and boosting moves in turn; B drops its stats or intimidates
		speeds(aFaster); setPlain();
		setAb(A, 'contrary'); setAb(B, int(2) === 0 ? 'intimidate' : '');
		const sd = pick(present(SELF_DROPS)), b1 = pick(present(BOOSTERS)), b2 = pick(present(BOOSTERS));
		A.moves = [...new Set([sd, b1, b2, pickFor(attacks, B)])];
		while (A.moves.length < 4) A.moves.push(pick(IDLE.filter(m => !A.moves.includes(m))));
		moves(B, pickFor(attacks, A), pick(present(DROPS)), 'recover');
		const seq = [1, 1, 2, 3, 1, 4, 1, 2];
		scenario.choices = [full, ...seq.map((m, i) => duoC(`move ${m}`, i % 3 === 2 ? 'move 2' : 'move 1'))];
	} else if (kind === 'defiant') {
		// A (Defiant) against Sticky Web, drops from the foe, an Intimidate foe, and its own self-drop (no trigger)
		speeds(aFaster); setPlain(); steadyBench(A2);
		setAb(A, 'defiant'); setAb(A2, int(3) === 0 ? '' : 'defiant'); setAb(B, int(2) === 0 ? 'intimidate' : '');
		const sd = pick(present(SELF_DROPS));
		A.moves = [...new Set([sd, pickFor(attacks, B), 'recover', 'swordsdance'])];
		B.moves = [...new Set(['stickyweb', pick(['icywind', 'snarl']), pick(['moonblast', 'acidspray', 'growl']), pickFor(attacks, A)])];
		while (B.moves.length < 4) B.moves.push(pick(IDLE.filter(m => !B.moves.includes(m))));
		scenario.choices = [full, duoC('move 1', 'move 1'), duoC('switch 2', 'move 2'), duoC('switch 2', 'move 3'), duoC('move 1', 'move 2'), duoC('move 2', 'move 3'), duoC('move 1', 'move 2'), duoC('move 2', 'move 4')];
	} else if (kind === 'shield') {
		speeds(aFaster); setPlain(); steadyBench(A2);
		setAb(A, 'dauntlessshield'); setAb(A2, int(2) === 0 ? 'dauntlessshield' : '');
		if (int(2) === 0) {
			// out and back: no second boost
			setAb(B, int(3) === 0 ? 'intimidate' : '');
			moves(A, pickFor(attacks, B), pick(IDLE)); moves(B, pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('switch 2', 'move 1'), duoC('switch 2', 'move 1'), duoC('move 1', 'move 1'), duoC('switch 2', 'move 1'), duoC('switch 2', 'move 1')];
		} else {
			// dragged out and in by a forcing move, the bench members holding the ability too
			benchAbility(aIdx, 'dauntlessshield');
			moves(A, pickFor(attacks, B), pick(IDLE)); B.moves = [...new Set([pick(FORCERS), pick(FORCERS), pickFor(attacks, A), 'recover'])];
			if (B.moves.length < 4) B.moves.push(...IDLE.filter(m => !B.moves.includes(m)).slice(0, 4 - B.moves.length));
			scenario.choices = [full, ...cycle(6, (i) => duoC('move 1', `move ${(i % 2) + 1}`))];
		}
	} else if (kind === 'weakarmor') {
		// B hits a bulky Weak Armor holder with a multi-hit physical move turn after turn: Defense to -6, Speed to +6
		speeds(aFaster); setPlain();
		setAb(A, 'weakarmor'); setAb(B, int(3) === 0 ? 'contrary' : '');
		const mh = pickFor(present(MULTI_PHYSICAL), A);
		moves(A, 'recover', pickFor(attacks, B)); moves(B, mh, pickFor(attacks, A));
		B.level = 40;
		scenario.choices = [full, ...cycle(7, (i) => duoC(i === 4 ? 'move 2' : 'move 1', 'move 1'))];
	} else if (kind === 'justified') {
		speeds(aFaster); setPlain();
		setAb(A, 'justified'); setAb(B, '');
		const dk = pickFor(present(DARK), A);
		const sub = int(3) === 0;
		moves(A, sub ? 'substitute' : 'recover', pickFor(attacks, B)); moves(B, dk, pickFor(present(DARK), A), pickFor(attacks, A));
		B.level = 50;
		scenario.choices = [full, ...cycle(7, (i) => duoC(sub && i > 2 ? 'move 2' : 'move 1', i % 3 === 2 ? 'move 2' : 'move 1'))];
	} else if (kind === 'gooey') {
		speeds(aFaster); setPlain();
		setAb(A, 'gooey'); setAb(B, pick(['clearbody', 'contrary', 'defiant', 'mirrorarmor', 'bigpecks', 'speedboost', '']));
		moves(A, 'recover', pickFor(attacks, B)); moves(B, pickFor(contact.length ? contact : attacks, A), pickFor(contact.length ? contact : attacks, A));
		B.level = 40;
		scenario.choices = [full, ...cycle(6, (i) => duoC(i === 3 ? 'move 2' : 'move 1', i % 2 ? 'move 2' : 'move 1'))];
	} else if (kind === 'speedboost') {
		speeds(aFaster); setPlain(); steadyBench(A2);
		setAb(A, 'speedboost'); setAb(A2, 'speedboost'); setAb(B, '');
		const pivot = int(3) === 0;
		moves(A, pivot ? pick(PIVOTS) : pickFor(attacks, B), pickFor(attacks, B)); moves(B, pickFor(attacks, A), pick(IDLE));
		B.level = 60;
		if (pivot && !canHit(A.moves[0], B)) B.species = speciesName(pick(real.filter(id => canHit(A.moves[0], { species: speciesName(id) }))));
		scenario.choices = pivot
			? [full, duoC('move 1', 'move 1'), duoC('switch 2', null), duoC('move 2', 'move 1'), duoC('move 2', 'move 1'), duoC('move 2', 'move 1')]
			: [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('switch 2', 'move 1'), duoC('move 1', 'move 1'), duoC('switch 2', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1')];
	} else if (kind === 'guarddog') {
		speeds(aFaster); setPlain(); steadyBench(A2);
		setAb(A, 'guarddog'); setAb(A2, int(2) === 0 ? 'guarddog' : '');
		if (int(3) === 0) {
			// a Red Card holder on B: A (Guard Dog) attacks it, and the card is used up without forcing A out
			setItem(B, 'redcard'); setAb(B, '');
			moves(A, pickFor(attacks, B), pickFor(attacks, B)); moves(B, pickFor(attacks, A), pick(IDLE));
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 1')];
		} else {
			setAb(B, int(3) === 0 ? 'intimidate' : '');
			moves(A, pickFor(attacks, B), pick(IDLE));
			B.moves = [...new Set([pick(FORCERS), pick(FORCERS), pick(FORCERS), pickFor(attacks, A)])];
			if (B.moves.length < 4) B.moves.push(...IDLE.filter(m => !B.moves.includes(m)).slice(0, 4 - B.moves.length));
			scenario.choices = [full, ...cycle(6, (i) => duoC(i === 3 ? 'switch 2' : 'move 1', `move ${(i % Math.min(B.moves.length, 3)) + 1}`))];
		}
	} else if (kind === 'keeneye') {
		speeds(aFaster); setPlain();
		setAb(A, 'keeneye'); setAb(B, int(3) === 0 ? 'keeneye' : '');
		moves(A, pickFor(lowAcc.length ? lowAcc : attacks, B), pickFor(lowAcc.length ? lowAcc : attacks, B), pickFor(attacks, B));
		B.moves = [...new Set(['doubleteam', 'sandattack', pickFor(lowAcc.length ? lowAcc : attacks, A), pick(IDLE)])];
		while (B.moves.length < 4) B.moves.push(pick(IDLE.filter(m => !B.moves.includes(m))));
		scenario.choices = [full, ...cycle(8, (i) => duoC(`move ${(i % 3) + 1}`, i < 3 ? 'move 1' : i < 5 ? 'move 2' : `move ${(i % 2) + 3}`))];
	} else if (kind === 'innerfocus') {
		speeds(!aFaster); setPlain();
		setAb(A, 'innerfocus'); setAb(B, int(2) === 0 ? 'intimidate' : '');
		moves(A, 'recover', pickFor(attacks, B)); B.moves = [...new Set([pick(FLINCHERS), pick(FLINCHERS), pick(FLINCHERS), pickFor(attacks, A)])];
		if (B.moves.length < 4) B.moves.push(...IDLE.filter(m => !B.moves.includes(m)).slice(0, 4 - B.moves.length));
		scenario.choices = [full, ...cycle(6, (i) => duoC(i % 2 ? 'move 2' : 'move 1', `move ${(i % Math.min(B.moves.length, 3)) + 1}`))];
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// The tally. Every wrapper passes its arguments and result through.

function newTally() {
	const byAbility = (names) => Object.fromEntries(names.map(n => [n, 0]));
	return {
		// Intimidate's boost call, by when it happened and by the target's ability
		intimidateWhen: { battleStart: 0, afterPivot: 0, afterFaint: 0, laterSwitch: 0 },
		intimidateInto: byAbility(['none', 'clearbody', 'bigpecks', 'innerfocus', 'guarddog', 'mirrorarmor', 'defiant', 'contrary', 'keeneye', 'other']),
		intimidateSubstitute: 0,
		doubleIntimidateStart: 0,
		// what TryBoost did: dropped a stat from the table, by the holder
		tryBoostBlocked: byAbility(['clearbody', 'bigpecks', 'innerfocus', 'guarddog', 'mirrorarmor', 'keeneye']),
		tryBoostPassed: byAbility(['clearbody', 'bigpecks', 'innerfocus', 'guarddog', 'mirrorarmor', 'keeneye']),
		guardDogBoost: 0,
		mirrorReflectedOnto: byAbility(['none', 'clearbody', 'defiant', 'contrary', 'bigpecks', 'keeneye', 'intimidate', 'mirrorarmor', 'other']),
		mirrorAtFloor: 0,
		// Contrary: the changes it flipped, by cause
		contrary: { selfDrop: 0, selfBoost: 0, foeDrop: 0, intimidate: 0, other: 0, atFloor: 0 },
		defiantBoost: { sticky: 0, intimidate: 0, foeMove: 0, mirror: 0, gooey: 0, other: 0 },
		defiantNoTrigger: { selfDrop: 0 },
		dauntless: { first: 0, repeat: 0, benchMember: 0 },
		weakArmor: { hits: 0, atDefFloor: 0, atSpeCap: 0, multiHit: 0 },
		justified: { hits: 0, multiHit: 0 },
		gooey: { hits: 0, intoClearBody: 0, intoContrary: 0, intoDefiant: 0, intoMirrorArmor: 0, intoOther: 0 },
		speedBoost: { noBoostFirstTurn: 0, boosted: 0, boostedAfterSwitchIn: 0 },
		dragOut: { guardDogRoar: 0, guardDogWhirlwind: 0, guardDogDragonTail: 0, guardDogCircleThrow: 0, guardDogRedCard: 0, guardDogDragIn: 0 },
		innerFocusFlinchBlocked: 0,
		innerFocusIntimidateBlocked: 0,
		keenEye: { ignoredEvasionBoost: 0, accuracyDropBlocked: 0 },
	};
}
let activeTally = null;
const t = () => activeTally;

const ABS = ['none', 'clearbody', 'bigpecks', 'innerfocus', 'guarddog', 'mirrorarmor', 'defiant', 'contrary', 'keeneye'];
const slot = (ability) => ABS.includes(ability) ? ability : (ability === 'noability' || ability === '' ? 'none' : 'other');

function install() {
	let lastSwitchRequest = null; // 'faint' | 'pivot' | null, until the next move request
	const makeRequest = Battle.prototype.makeRequest;
	Battle.prototype.makeRequest = function (type, ...rest) {
		if (type === 'switch') lastSwitchRequest = this.sides.some(s => s.active[0] && s.active[0].fainted) ? 'faint' : 'pivot';
		else if (type === 'move') lastSwitchRequest = null;
		return makeRequest.call(this, type, ...rest);
	};
	const boost = Battle.prototype.boost;
	Battle.prototype.boost = function (b, target, source, effect, isSecondary, isSelf) {
		const c = t();
		let eff = effect, tgt = target, src = source;
		if (this.event) { tgt = tgt || this.event.target; src = src || this.event.source; eff = eff || this.effect; }
		if (c && tgt && tgt.hp && tgt.isActive && eff) {
			const cause = this.event && this.event.effect;
			if (eff.id === 'intimidate') {
				if (this.turn === 0) c.intimidateWhen.battleStart++;
				else if (lastSwitchRequest === 'pivot') c.intimidateWhen.afterPivot++;
				else if (lastSwitchRequest === 'faint') c.intimidateWhen.afterFaint++;
				else c.intimidateWhen.laterSwitch++;
				c.intimidateInto[slot(tgt.ability || 'none')]++;
				if (this.turn === 0 && src && src.side.foe.active[0] && src.side.foe.active[0].ability === 'intimidate') c.doubleIntimidateStart++;
			}
			if (eff.id === 'guarddog') c.guardDogBoost++;
			if (eff.id === 'mirrorarmor' && tgt.ability !== 'mirrorarmor') {
				c.mirrorReflectedOnto[tgt.ability in c.mirrorReflectedOnto ? tgt.ability : tgt.ability === 'noability' ? 'none' : 'other']++;
				if (Object.keys(b).some(k => tgt.boosts[k] === -6)) c.mirrorAtFloor++;
			}
			if (eff.id === 'defiant') {
				const ce = cause && cause.id;
				if (ce === 'stickyweb') c.defiantBoost.sticky++;
				else if (ce === 'intimidate') c.defiantBoost.intimidate++;
				else if (ce === 'mirrorarmor') c.defiantBoost.mirror++;
				else if (ce === 'gooey') c.defiantBoost.gooey++;
				else if (cause && cause.effectType === 'Move') c.defiantBoost.foeMove++;
				else c.defiantBoost.other++;
			}
			if (eff.id === 'weakarmor') {
				c.weakArmor.hits++;
				if (tgt.boosts.def === -6) c.weakArmor.atDefFloor++;
				if (tgt.boosts.spe >= 5) c.weakArmor.atSpeCap++;
				if (this.activeMove && this.activeMove.multihit) c.weakArmor.multiHit++;
			}
			if (eff.id === 'justified') {
				c.justified.hits++;
				if (this.activeMove && this.activeMove.multihit) c.justified.multiHit++;
			}
			if (eff.id === 'gooey') {
				c.gooey.hits++;
				const k = { clearbody: 'intoClearBody', contrary: 'intoContrary', defiant: 'intoDefiant', mirrorarmor: 'intoMirrorArmor' }[tgt.ability];
				c.gooey[k || 'intoOther']++;
			}
			if (tgt.ability === 'contrary' && eff.effectType !== 'Ability') {
				const neg = Object.values(b).some(x => x < 0), pos = Object.values(b).some(x => x > 0);
				if (eff.effectType === 'Move' && src === tgt) { if (neg) c.contrary.selfDrop++; else if (pos) c.contrary.selfBoost++; }
				else if (eff.effectType === 'Move' && neg) c.contrary.foeDrop++;
				else c.contrary.other++;
				if (Object.entries(b).some(([k, v]) => v > 0 && tgt.boosts[k] === -6 || v < 0 && tgt.boosts[k] === 6)) c.contrary.atFloor++;
			}
			if (tgt.ability === 'contrary' && eff.id === 'intimidate') c.contrary.intimidate++;
			if (tgt.ability === 'defiant' && src === tgt && Object.values(b).some(x => x < 0) && eff.effectType === 'Move') c.defiantNoTrigger.selfDrop++;
		}
		return boost.call(this, b, target, source, effect, isSecondary, isSelf);
	};
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, relayVar, ...rest) {
		const c = t();
		if (c && eventid === 'TryBoost' && target && target.ability in c.tryBoostBlocked && relayVar && typeof relayVar === 'object') {
			const before = Object.keys(relayVar).length;
			const had = JSON.stringify(relayVar);
			const r = runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
			if (r && Object.keys(r).length < before) {
				c.tryBoostBlocked[target.ability]++;
				if (target.ability === 'innerfocus' && sourceEffect && sourceEffect.id === 'intimidate') c.innerFocusIntimidateBlocked++;
				if (target.ability === 'keeneye') c.keenEye.accuracyDropBlocked++;
			} else if (had !== '{}') c.tryBoostPassed[target.ability]++;
			return r;
		}
		if (c && eventid === 'DragOut' && target && target.ability === 'guarddog') {
			const r = runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
			if (!r) {
				const id = sourceEffect && sourceEffect.id;
				if (source && source.lastItem === 'redcard') c.dragOut.guardDogRedCard++;
				else if (id === 'roar') c.dragOut.guardDogRoar++;
				else if (id === 'whirlwind') c.dragOut.guardDogWhirlwind++;
				else if (id === 'dragontail') c.dragOut.guardDogDragonTail++;
				else if (id === 'circlethrow') c.dragOut.guardDogCircleThrow++;
				else if (!sourceEffect && !source) c.dragOut.guardDogDragIn++;
			}
			return r;
		}
		if (c && eventid === 'TryAddVolatile' && target && target.ability === 'innerfocus' && relayVar && relayVar.id === 'flinch') {
			const r = runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
			if (!r) c.innerFocusFlinchBlocked++;
			return r;
		}
		return runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
	};
	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, source, sourceEffect, ...rest) {
		const c = t();
		if (c && effect && target && target.side) {
			if (effect.id === 'dauntlessshield' && (eventid === 'SwitchIn' || eventid === 'Start')) {
				if (target.shieldBoost) c.dauntless.repeat++;
				else {
					c.dauntless.first++;
					if (this.turn > 0) c.dauntless.benchMember++;
				}
			}
			if (effect.id === 'speedboost' && eventid === 'Residual') {
				if (target.activeTurns) { c.speedBoost.boosted++; if (target.activeTurns === 1 && this.turn > 1) c.speedBoost.boostedAfterSwitchIn++; }
				else c.speedBoost.noBoostFirstTurn++;
			}
			if (effect.id === 'intimidate' && eventid === 'SwitchIn') {
				const foe = target.side.foe.active[0];
				if (foe && foe.hp && foe.volatiles.substitute) c.intimidateSubstitute++;
			}
		}
		return singleEvent.call(this, eventid, effect, state, target, source, sourceEffect, ...rest);
	};
	const hitStepAccuracy = BattleActions.prototype.hitStepAccuracy;
	BattleActions.prototype.hitStepAccuracy = function (targets, pokemon, move) {
		const c = t();
		if (c && pokemon.ability === 'keeneye' && move.ignoreEvasion) {
			for (const target of targets) if (target.boosts.evasion > 0 && move.accuracy !== true) c.keenEye.ignoredEvasionBoost++;
		}
		return hitStepAccuracy.call(this, targets, pokemon, move);
	};
}

// counters that cannot be hit by construction or are rare by nature
// - justified.multiHit: no Dark multi-hit move is supported (Beat Up, the only one, is not in the pool).
// - mirrorAtFloor, contrary.atFloor, weakArmor.atSpeCap are reached when stages climb to the cap in long scripted exchanges; they are counted and
//   reported, and not required.
// - defiantBoost.other, intimidateInto.other and the like are residues of the random play.
const STRUCTURAL_ZERO = ['justified.multiHit', 'defiantBoost.other', 'dragOut.guardDogRedCard', 'dragOut.guardDogDragIn', 'tryBoostPassed.keeneye',
	'mirrorReflectedOnto.mirrorarmor', 'mirrorReflectedOnto.other', 'mirrorReflectedOnto.none', 'contrary.other'];

// The random policy can pick a switch that the request lists and Showdown then rejects ("The Pokemon in slot N can only switch in once": a
// mid-turn request for a Pokemon that has just been switched in, whose slot is the one the same side chose for the turn; Showdown's `clearChoice`
// takes the queued switch for a choice already made). That is the recorder's, not the engine's: the battle is recorded again with the policy's
// next seed.
function recordWithPolicy(scenario) {
	for (let k = 0; ; k++) {
		const attempt = k === 0 ? scenario : Object.assign({}, scenario, { policySeed: (scenario.policySeed || 1) + 7919 * k });
		try { return record(attempt); } catch (e) {
			if (k >= 30 || !/can only switch in once/.test(e.message)) throw e;
			if (activeTally) activeTally = JSON.parse(JSON.stringify(activeTally)); // (the tally keeps what the failed attempt saw: see below)
		}
	}
}

// A script that Showdown rejects (a faint asks for a switch where the script has a move) is cut back to the longest prefix that it accepts;
// the random policy plays on from there. Only a script with no accepted prefix is dropped altogether.
function recordScenario(scenario) {
	const before = JSON.stringify(activeTally);
	const choices = scenario.choices;
	let lastError = null;
	for (let n = choices ? choices.length : 0; ; n--) {
		const attempt = Object.assign({}, scenario);
		if (choices) { if (n >= 1) attempt.choices = choices.slice(0, n); else delete attempt.choices; }
		try {
			const trace = recordWithPolicy(attempt);
			return { trace, scripted: !!attempt.choices, cut: choices && attempt.choices && attempt.choices.length < choices.length, dropped: lastError && !attempt.choices ? lastError : null };
		} catch (e) {
			if (!attempt.choices) throw e;
			lastError = e.message;
			activeTally = JSON.parse(before);
		}
	}
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice8a.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice8a-${k}.json`), JSON.stringify(pool[k]));
	}
	activeTally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scriptedCount = 0, dropped = 0, cutCount = 0;
	const droppedBy = {};
	const abilitySeen = new Set();
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		for (const side of scenario.teams) for (const s of side) abilitySeen.add(s.ability);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scriptedCount++;
		if (result.cut) cutCount++;
		if (result.dropped) {
			dropped++;
			droppedBy[scenario.kind] = (droppedBy[scenario.kind] || 0) + 1;
			if (process.env.SLICE8A_VERBOSE) console.error(`seed ${seed}: script dropped (${scenario.kind}, turn ${(scenario.choices || []).length}): ${result.dropped}`);
		}
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice8a-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice8a seeds, ${written} traces written (${scriptedCount} scripted, ${cutCount} of them cut short, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error(`scripts dropped by kind: ${JSON.stringify(droppedBy)}`);
	const missing = ABILITIES.filter(id => !abilitySeen.has(dex.abilities.get(id).name));
	console.error(missing.length ? `ABILITIES NOT IN ANY TEAM: ${missing.join(', ')}` : 'every slice 8a ability is in some team');
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
