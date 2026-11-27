#!/usr/bin/env node
'use strict';
// Generates battles for "slice 8e" (the move pipeline: Pressure, Prankster, Libero, Protean, Liquid Voice, Infiltrator) and records
// each on Showdown.
//
// The teams are slice 6's (gen-slice6.js `assign`, so every earlier mechanic is still in the pool). Every set may then get one of the six
// abilities and some of the moves that meet them, from a stream of its own. Every scenario is recorded with `tera: true`. About 30% of the
// battles are the plain random policy over those teams; the rest are scripted openings (a pair of choices per decision point; the random
// policy takes over when the script ends, and a script that Showdown rejects is dropped and the battle recorded without it):
//   ptarget    Pressure against each kind of move target: one foe, all foes, the user, the foe's side (Spikes and its kin have
//              `mustpressure`), the user's side and the whole field
//   plow       Pressure on moves that run low on PP: the foe's Pressure doubles every use, a slot reaches 0 with the extra PP wasted, and
//              the last move is Struggle
//   pchoice    the same under a Choice lock, and under an Encore that the Pressure holder puts on the user
//   prank      Prankster status moves into Dark types (immune), non-Dark types, a Terastallized Dark type, and a Pokemon under Psychic
//              Terrain; moves on the user, its side or the field are never blocked
//   prio       Prankster priority ties: two Pranksters with status moves, a Prankster against a priority 1 attack and against a faster
//              Pokemon with a slower status move
//   libero     Libero and Protean: types changed once per switch-in, a repeated use, switching out and back, Terastallizing before and after
//   lvoice     Liquid Voice with sound moves: against every type, with STAB for a Water-type user, into a Substitute and with Tera
//   infil      Infiltrator into a Substitute, Reflect, Light Screen and Aurora Veil, with status moves and with Defog
//
// Usage:
//   node gen-slice8e.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice8e-<seed>.json, plus slice8e-{moves,species,abilities,items}.json (copies of the pool). A tally of the outcomes
// that distinguish each branch is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers
// pass every argument and result through and call nothing the battle would not call).

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

const MINE = ['pressure', 'prankster', 'libero', 'protean', 'liquidvoice', 'infiltrator'];
// the usage weights of the six (data/usage/gen9ou.json), rounded: Pressure first
const WEIGHTED = [...Array(32).fill('pressure'), ...Array(2).fill('prankster'), ...Array(7).fill('libero'), ...Array(2).fill('protean'),
	...Array(2).fill('liquidvoice'), ...Array(8).fill('infiltrator')];
const IDLE = ['recover', 'roost', 'calmmind', 'swordsdance'];
const SELF_MOVES = ['swordsdance', 'nastyplot', 'calmmind', 'agility', 'recover', 'roost'];
const SIDE_MOVES = ['spikes', 'stealthrock', 'toxicspikes'];
const OWN_SIDE_MOVES = ['reflect', 'lightscreen', 'tailwind'];
const FIELD_MOVES = ['haze', 'trickroom', 'raindance', 'sunnyday', 'sandstorm', 'snowscape'];
const SPREAD_MOVES = ['earthquake', 'surf', 'heatwave', 'bulldoze', 'discharge'];
const HARASS = ['thunderwave', 'willowisp', 'toxic', 'glare', 'stunspore', 'poisonpowder', 'screech'];
const TARGETED_STATUS = ['thunderwave', 'willowisp', 'toxic', 'taunt', 'encore', 'glare', 'stunspore', 'poisonpowder', 'screech'];
const PRIORITY_ATTACKS = ['quickattack', 'aquajet', 'iceshard', 'machpunch', 'shadowsneak', 'bulletpunch', 'suckerpunch', 'extremespeed'];
const CHOICE = ['choiceband', 'choicespecs', 'choicescarf'];
const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2246822519 + 3266489917);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	for (const id of MINE) if (!pool.abilities.includes(id)) throw new Error(`the pool has no ${id}`);
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
	const typesOf = (id) => dex.species.get(id).types;
	const put = (s, id) => {
		if (!has(id) || s.moves.includes(id)) return;
		s.moves.splice(int(4), 1, id);
		s.moves = [...new Set(s.moves)].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const setItem = (s, id) => { s.item = itemName(id); };
	const setAbility = (s, id) => { s.ability = abilityName(id); };
	const attacksAll = pool.moves.filter(id => mv(id).category !== 'Status');
	const sound = pool.moves.filter(id => mv(id).flags.sound && mv(id).category !== 'Status');

	// the sprinkle: the six abilities on many sets, with the moves that meet them, so that unscripted play crosses them as well
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 34) {
			const ab = pick(WEIGHTED);
			setAbility(s, ab);
			if (ab === 'prankster') { put(s, pick(TARGETED_STATUS)); if (int(2)) put(s, pick(SELF_MOVES)); }
			if (ab === 'liquidvoice' && sound.length) { put(s, pick(sound)); if (int(2)) put(s, pick(sound)); }
			if (ab === 'infiltrator' && int(2)) put(s, pick(['defog', 'knockoff', 'icebeam', 'earthquake']));
		}
		if (int(100) < 22) put(s, pick(SIDE_MOVES));
		if (int(100) < 14) put(s, pick(OWN_SIDE_MOVES.concat(['auroraveil'])));
		if (int(100) < 14) put(s, 'substitute');
		if (int(100) < 10) put(s, pick(FIELD_MOVES));
		if (int(100) < 8) put(s, pick(TARGETED_STATUS));
		if (int(100) < 10) setItem(s, pick(CHOICE));
	}
	const kinds = [
		['random', 30], ['ptarget', 9], ['plow', 6], ['pchoice', 7], ['prank', 12], ['prio', 7], ['libero', 12], ['lvoice', 8], ['infil', 9],
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
	// a steady Pokemon: no item, the ability given, EVs in HP and Speed only, so that damage and speed are predictable
	const steady = (s, ab, spe) => {
		s.level = 100; s.nature = 'Hardy'; s.item = ''; setAbility(s, ab);
		s.evs = { hp: 252, atk: 0, def: 0, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
	};
	// bulky species: the sum of HP, Defense and Special Defense is large (the foe lasts through a scripted opening)
	const bulk = (id) => { const b = dex.species.get(id).baseStats; return b.hp + b.def + b.spd; };
	const bulky = real.filter(id => bulk(id) >= 270 && !dex.species.get(id).types.includes('Dark'));
	const darks = real.filter(id => typesOf(id).includes('Dark') && bulk(id) >= 200);
	const notDark = real.filter(id => !typesOf(id).includes('Dark') && bulk(id) >= 200);
	const weak = attacksAll.filter(id => {
		const m = mv(id);
		return m.basePower >= 40 && m.basePower <= 75 && m.priority === 0 && !m.multihit && !m.selfSwitch && !m.forceSwitch && !m.selfdestruct &&
			!m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge && !m.self && !m.recoil && !m.drain && m.target === 'normal' &&
			(m.accuracy === true || m.accuracy >= 90) && !m.secondaries && !m.volatileStatus;
	});
	const hit = (ids, s) => { const ok = ids.filter(id => dex.getImmunity(mv(id).type, dex.species.get(s.species).types)); return pick(ok.length ? ok : ids); };
	const tm = (m) => (int(100) < 20 ? `${m} terastallize` : m);
	const has2 = (xs) => xs.filter(has);

	// the user is not a frail or one-HP species (Shedinja): a scripted opening must not end in a forced switch
	if (kind !== 'prio') A.species = speciesName(pick(real.filter(id => bulk(id) >= 200)));

	if (kind === 'ptarget') {
		// A (no ability, or Pressure too) uses a move of each target kind into a Pressure foe; the PP in every request shows the cost.
		// The foe recovers (a move on itself: no Pressure of A's side applies to it unless A has the ability as well).
		B.species = speciesName(pick(bulky)); steady(B, 'pressure', 0);
		steady(A, int(4) === 0 ? 'pressure' : 'noability', 0);
		const kindsPick = [
			pick(has2(SELF_MOVES)), pick(has2(SIDE_MOVES)), pick(has2(FIELD_MOVES.concat(['defog']))), pick(has2(OWN_SIDE_MOVES)),
			pick(has2(SPREAD_MOVES)), hit(weak, B),
		];
		const chosen = [hit(weak, B), ...[...new Set(kindsPick)].sort(() => rnd() - 0.5).slice(0, 3)];
		A.moves = chosen.slice(0, 4);
		while (A.moves.length < 4) A.moves.push(pick(['swordsdance', 'recover', 'nastyplot']));
		A.moves = [...new Set(A.moves)];
		while (A.moves.length < 4) { const m = pick(pool.moves); if (!A.moves.includes(m)) A.moves.push(m); }
		B.moves = ['swordsdance', 'calmmind', 'recover', 'roost'];
		const turns = [full];
		const slots = [1, 2, 3, 4].sort(() => rnd() - 0.5);
		for (const k of [...slots, ...slots.slice(0, 2 + int(3))]) turns.push(duoC(`move ${k}`, 'move 1'));
		scenario.choices = turns;
	} else if (kind === 'plow') {
		// level 1 attackers with the lowest PP in the pool, against a bulky Pressure foe: every use costs 2 until the slot is 0, then the
		// next slot, then Struggle (with `Recover` below). The script plays every slot to the end and two Struggles.
		const low = weak.filter(id => mv(id).pp <= 8 && mv(id).accuracy === true || (mv(id).pp <= 8 && mv(id).accuracy === 100) ).concat(weak.filter(id => mv(id).pp <= 10));
		const ids = [...new Set(low)].sort(() => rnd() - 0.5).slice(0, 4);
		if (ids.length < 4) throw new Error('too few low-PP moves');
		B.species = speciesName(pick(bulky)); steady(B, 'pressure', 0); B.moves = ['swordsdance', 'calmmind', 'nastyplot', 'irondefense'];
		steady(A, 'noability', 0); A.level = 1; A.moves = ids;
		const turns = [full];
		ids.forEach((id, i) => {
			const pp = Math.floor(mv(id).pp * 8 / 5);
			for (let u = 0; u < Math.ceil(pp / 2); u++) turns.push(duoC(`move ${i + 1}`, `move ${turns.length % 4 + 1}`));
		});
		turns.push(duoC('move 1', 'move 1'), duoC('move 1', 'move 2'));
		scenario.choices = turns;
	} else if (kind === 'pchoice') {
		const ids = [...new Set(weak.filter(id => mv(id).pp <= 10))].sort(() => rnd() - 0.5).slice(0, 4);
		B.species = speciesName(pick(bulky)); steady(B, 'pressure', 0);
		steady(A, 'noability', 252); A.level = 1; A.moves = ids;
		const v = int(2) === 0 ? 0 : 1 + int(2);
		if (v === 0) {
			// Choice lock: A repeats its move 1 until the PP is 0 (the lock leaves no other move: Struggle)
			setItem(A, pick(CHOICE)); B.moves = ['swordsdance', 'calmmind', 'recover', 'roost'];
			const pp = Math.floor(mv(ids[0]).pp * 8 / 5);
			scenario.choices = [full, ...Array.from({ length: Math.ceil(pp / 2) + 2 }, () => duoC('move 1', 'move 1'))];
		} else {
			// Encore by the Pressure holder (slower, so A has moved): A repeats its move 1 under it; the PP runs out as the Encore ends
			steady(B, 'pressure', 0); B.moves = ['encore', 'swordsdance', 'calmmind', 'nastyplot']; steady(A, 'noability', 252);
			A.level = 1; A.moves = ids;
			const pp = Math.floor(mv(ids[0]).pp * 8 / 5);
			const n = Math.ceil(pp / 2);
			// turn 1: A moves, then the slower B encores it; A repeats its move 1 until the PP is 0 (the Encore ends with it, or is over
			// already), then plays move 2 (v 1: one more turn after that)
			scenario.choices = [full, duoC('move 1', 'move 1'), ...Array.from({ length: n - 1 }, (_, i) => duoC('move 1', `move ${i % 3 + 2}`)),
				duoC('move 2', 'move 2'), duoC('move 2', 'move 3'), ...(v === 1 ? [duoC('move 3', 'move 4')] : [])];
		}
	} else if (kind === 'prank') {
		const v = int(5);
		const dark = v !== 2 && v !== 3 && int(10) < 8;
		B.species = speciesName(pick(dark ? darks : notDark)); steady(B, v === 3 ? 'psychicsurge' : 'noability', 0);
		if (v === 1) B.teraType = 'Dark';
		else delete B.teraType;
		steady(A, 'prankster', 0);
		const picks = [...new Set([pick(has2(TARGETED_STATUS)), pick(has2(TARGETED_STATUS)), pick(has2(SELF_MOVES)), pick(has2(SIDE_MOVES.concat(OWN_SIDE_MOVES, FIELD_MOVES)))])];
		A.moves = picks;
		for (const m of ['thunderwave', 'willowisp', 'recover', 'substitute']) if (A.moves.length < 4 && !A.moves.includes(m) && has(m)) A.moves.push(m);
		B.moves = [hit(weak, A), 'recover', 'roost', 'calmmind'];
		const turns = [full];
		turns.push(duoC(tm('move 1'), v === 1 ? 'move 1 terastallize' : 'move 1'));
		for (let i = 1; i < 5 + int(3); i++) turns.push(duoC(`move ${(i % A.moves.length) + 1}`, 'move 1'));
		scenario.choices = turns;
	} else if (kind === 'prio') {
		const v = int(3);
		const sp = pick(real.filter(id => bulk(id) >= 200));
		A.species = speciesName(sp); B.species = speciesName(v === 2 ? pick(real.filter(id => bulk(id) >= 200 && !typesOf(id).includes('Dark'))) : sp);
		steady(A, 'prankster', 0);
		if (v === 0) {
			// two Pranksters with status moves: +1 each, equal speed: the tie draws
			steady(B, 'prankster', 0);
			A.moves = [pick(has2(SELF_MOVES)), pick(has2(TARGETED_STATUS)), 'recover', 'substitute'];
			B.moves = [pick(has2(SELF_MOVES)), pick(has2(HARASS)), 'recover', 'swordsdance'];
			if (B.species !== A.species) B.species = A.species;
			scenario.choices = [full, ...Array.from({ length: 4 }, (_, i) => duoC(`move ${(i % 2) + 1}`, `move ${((i + int(2)) % 2) + 1}`))];
		} else if (v === 1) {
			// a Prankster status move against a priority 1 attack: the same priority, so the speed decides (equal: a draw)
			steady(B, 'noability', 0);
			A.moves = [pick(has2(SELF_MOVES)), pick(has2(TARGETED_STATUS)), 'recover', 'substitute'];
			B.moves = [pick(has2(PRIORITY_ATTACKS.filter(id => mv(id).priority === 1))), 'recover', 'roost', 'calmmind'];
			scenario.choices = [full, ...Array.from({ length: 4 }, (_, i) => duoC(`move ${(i % 2) + 1}`, 'move 1'))];
		} else {
			// a Prankster against a faster foe with a status move (the foe's priority 0 < 1) and against a slower one with a priority 2 move
			steady(B, 'noability', 252);
			A.moves = [pick(has2(SELF_MOVES)), pick(has2(TARGETED_STATUS)), 'recover', 'substitute'];
			B.moves = [pick(has2(HARASS.concat(SELF_MOVES))), 'extremespeed', 'recover', 'roost'];
			if (!has('extremespeed')) B.moves[1] = 'recover';
			scenario.choices = [full, ...Array.from({ length: 4 }, (_, i) => duoC(`move ${(i % 2) + 1}`, `move ${(i % 2) + 1}`))];
		}
	} else if (kind === 'libero') {
		const ab = pick(['libero', 'protean']);
		B.species = speciesName(pick(int(4) === 0 ? real.filter(id => typesOf(id).includes('Ghost') && bulk(id) >= 200) : bulky));
		steady(B, 'noability', 0); B.moves = [hit(weak, A), 'recover', 'roost', 'calmmind']; delete B.teraType;
		steady(A, ab, 0);
		// four moves of four different types, a status move among them half of the time
		const byType = {};
		for (const id of weak) (byType[mv(id).type] = byType[mv(id).type] || []).push(id);
		const typeNames = Object.keys(byType).sort(() => rnd() - 0.5).slice(0, 4);
		A.moves = typeNames.map(t => pick(byType[t]));
		if (int(2) === 0) A.moves[3] = pick(has2(['toxic', 'willowisp', 'thunderwave', 'swordsdance', 'spikes']));
		A.moves = [...new Set(A.moves)];
		while (A.moves.length < 4) { const m = pick(weak); if (!A.moves.includes(m)) A.moves.push(m); }
		const v = int(4);
		const b = (i) => duoC(`move ${i}`, 'move 2');
		if (v === 0) scenario.choices = [full, b(1), b(2), b(1), b(3), b(4), b(2)]; // repeated use, and a second type that the flag blocks
		else if (v === 1) scenario.choices = [full, b(1), duoC('switch 2', 'move 2'), duoC('switch 2', 'move 2'), b(2), b(3), b(1)]; // out and back
		else if (v === 2) scenario.choices = [full, duoC('move 1 terastallize', 'move 2'), b(2), b(3), b(4)]; // Tera before any change
		else scenario.choices = [full, b(1), duoC('move 2 terastallize', 'move 2'), b(3), b(4), b(1)]; // Tera after a change
	} else if (kind === 'lvoice') {
		const v = int(4);
		const snd = sound.length ? sound : weak;
		B.species = speciesName(pick(v === 0 ? real.filter(id => bulk(id) >= 200) : bulky));
		steady(B, 'noability', 0); B.moves = [hit(weak, A), 'recover', 'roost', 'calmmind']; delete B.teraType;
		steady(A, 'liquidvoice', 0);
		if (v === 1 && !dex.species.get(A.species).types.includes('Water')) {
			A.species = speciesName(pick(real.filter(id => typesOf(id).includes('Water') && bulk(id) >= 150)));
		}
		const ss = [...new Set([pick(snd), pick(snd), pick(snd)])];
		A.moves = [...ss, hit(weak, B)];
		A.moves = [...new Set(A.moves)];
		while (A.moves.length < 4) { const m = pick(weak); if (!A.moves.includes(m)) A.moves.push(m); }
		if (v === 2) { B.moves = ['substitute', 'recover', 'roost', hit(weak, A)]; steady(B, 'noability', 0); }
		if (v === 3) A.teraType = 'Water';
		const lead = v === 2 ? duoC('move 1', 'move 1') : duoC(v === 3 ? 'move 1 terastallize' : 'move 1', 'move 2');
		scenario.choices = [full, lead, ...Array.from({ length: 4 }, (_, i) => duoC(`move ${(i + 1) % 4 + 1}`, v === 2 ? 'move 2' : 'move 2'))];
	} else if (kind === 'infil') {
		const v = int(4);
		B.species = speciesName(pick(bulky)); steady(B, v === 2 ? 'snowwarning' : 'noability', 0); delete B.teraType;
		steady(A, 'infiltrator', 252);
		const phys = hit(weak.filter(id => mv(id).category === 'Physical'), B), spec = hit(weak.filter(id => mv(id).category === 'Special'), B);
		if (v === 0) {
			// a Substitute, then attacks and status moves into it (Infiltrator goes through; the foe's own Substitute blocks without it)
			A.moves = [phys, spec, pick(has2(['toxic', 'willowisp', 'thunderwave', 'taunt'])), 'defog'];
			B.moves = ['substitute', 'recover', 'roost', 'calmmind'];
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 2'), duoC('move 3', 'move 2'), duoC('move 4', 'move 2'), duoC('move 1', 'move 1'), duoC('move 2', 'move 2')];
		} else if (v === 1) {
			// Reflect and Light Screen: the physical and the special attack are not halved
			A.moves = [phys, spec, 'recover', 'swordsdance'];
			B.moves = ['reflect', 'lightscreen', 'recover', 'roost'];
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 2'), duoC('move 1', 'move 3'), duoC('move 2', 'move 3'), duoC('move 1', 'move 3')];
		} else if (v === 2) {
			// Aurora Veil under the snow that the foe's Snow Warning set
			A.moves = [phys, spec, 'recover', 'swordsdance'];
			B.moves = ['auroraveil', 'recover', 'roost', 'calmmind'];
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 2'), duoC('move 1', 'move 2'), duoC('move 2', 'move 3'), duoC('move 1', 'move 3')];
		} else {
			// Defog into a Substitute (the evasion drop goes through), and the foe's own Substitute with a Reflect behind it
			A.moves = ['defog', phys, spec, 'toxic'];
			B.moves = ['substitute', 'reflect', 'recover', 'roost'];
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 2', 'move 3'), duoC('move 3', 'move 3'), duoC('move 4', 'move 3')];
		}
	}
	// no duplicate or missing move in a set (a scripted slot that moved is only a drop of the script)
	for (const st of [A, B]) {
		st.moves = [...new Set(st.moves.filter(Boolean))];
		while (st.moves.length < 4) { const m = pick(pool.moves); if (!st.moves.includes(m)) st.moves.push(m); }
		st.moves = st.moves.slice(0, 4);
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// The tally. Every wrapper passes its arguments and result through.

function newTally() {
	return {
		pressure: {
			byTarget: { normal: 0, allAdjacentFoes: 0, allAdjacent: 0, all: 0, foeSide: 0, mustpressureFoeSide: 0 },
			noDropToAlly: { self: 0, allySide: 0, all: 0 }, extraPPWasted: 0, extraPPClamped: 0, struggleAfterPressure: 0, underChoiceLock: 0, underEncore: 0,
			onFaintedFoe: 0,
		},
		prankster: {
			intoDark: 0, intoDarkTera: 0, intoNonDark: 0, selfSideOrField: 0, blockedByPsychicTerrain: 0, tieWithPrankster: 0, tieWithPriorityAttack: 0,
			priorityRaised: 0,
		},
		libero: { changed: 0, protean: 0, blockedOnce: 0, sameType: 0, terastallized: 0, afterSwitchBack: 0, statusMove: 0, intoImmune: 0 },
		liquidVoice: { soundToWater: 0, withStab: 0, againstResist: 0, againstWeak: 0, intoSubstitute: 0, withTera: 0, nonSoundUnchanged: 0 },
		infiltrator: {
			intoSubstitute: 0, intoSubstituteStatus: 0, defogIntoSubstitute: 0, physicalThroughReflect: 0, specialThroughLightScreen: 0,
			throughAuroraVeil: 0,
		},
	};
}
let activeTally = null;
const t = () => activeTally;
const hasType = (p, ty) => p.getTypes().includes(ty);

function install() {
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, ...rest) {
		const c = t();
		if (c && eventid === 'DeductPP' && target && source && sourceEffect) {
			const r = runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
			const move = sourceEffect;
			if (r === 1) {
				c.pressure.byTarget[move.target] = (c.pressure.byTarget[move.target] || 0) + 1;
				if (move.flags.mustpressure && move.target === 'foeSide') c.pressure.byTarget.mustpressureFoeSide++;
				if (source.volatiles.choicelock) c.pressure.underChoiceLock++;
				if (source.volatiles.encore) c.pressure.underEncore++;
				if (target.fainted) c.pressure.onFaintedFoe++;
			} else if (target.ability === 'pressure' && target.isAlly(source)) {
				c.pressure.noDropToAlly[move.target] = (c.pressure.noDropToAlly[move.target] || 0) + 1;
			}
			return r;
		}
		if (c && eventid === 'PrepareHit' && target && sourceEffect && (target.ability === 'libero' || target.ability === 'protean')) {
			const key = target.ability;
			const type = sourceEffect.type;
			const before = target.getTypes().join();
			const wasUsed = !!target.abilityState[key];
			const had = this.__liberoSeen && this.__liberoSeen.has(target);
			const r = runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
			if (type && type !== '???' && !sourceEffect.hasBounced) {
				if (wasUsed && before !== type) c.libero.blockedOnce++;
				else if (before === type) c.libero.sameType++;
				else if (target.terastallized) c.libero.terastallized++;
				else if (target.getTypes().join() === type) {
					c.libero.changed++;
					if (key === 'protean') c.libero.protean++;
					if (sourceEffect.category === 'Status') c.libero.statusMove++;
					if (source && !source.runImmunity(type) && sourceEffect.category !== 'Status') c.libero.intoImmune++;
					if (had) c.libero.afterSwitchBack++;
					if (!this.__liberoSeen) this.__liberoSeen = new Set();
					this.__liberoSeen.add(target);
				}
			}
			return r;
		}
		if (c && eventid === 'ModifyType' && target && target.ability === 'liquidvoice' && sourceEffect) {
			const before = sourceEffect.type;
			const r = runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
			const foe = target.side.foe.active[0];
			if (sourceEffect.flags.sound) {
				if (before !== 'Water' && sourceEffect.type === 'Water') c.liquidVoice.soundToWater++;
				if (hasType(target, 'Water')) c.liquidVoice.withStab++;
				if (target.terastallized === 'Water') c.liquidVoice.withTera++;
				if (foe && foe.volatiles.substitute) c.liquidVoice.intoSubstitute++;
				if (foe && sourceEffect.category !== 'Status' && sourceEffect.type === 'Water') {
					const eff = foe.runEffectiveness(sourceEffect);
					if (eff < 0) c.liquidVoice.againstResist++;
					if (eff > 0) c.liquidVoice.againstWeak++;
				}
			} else if (sourceEffect.category !== 'Status' && sourceEffect.type === before) c.liquidVoice.nonSoundUnchanged++;
			return r;
		}
		if (c && eventid === 'TryPrimaryHit' && target && target.volatiles.substitute && sourceEffect && source && source !== target) {
			const inf = !!sourceEffect.infiltrates;
			const r = runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
			if (inf && !sourceEffect.flags.bypasssub) {
				if (sourceEffect.category === 'Status') c.infiltrator.intoSubstituteStatus++;
				else c.infiltrator.intoSubstitute++;
			}
			return r;
		}
		if (c && eventid === 'ModifyDamage' && target && sourceEffect && sourceEffect.infiltrates && source) {
			const defSide = source.side;
			const category = this.getCategory(sourceEffect);
			const crit = source.getMoveHitData ? source.getMoveHitData(sourceEffect).crit : false;
			if (!crit) {
				if (category === 'Physical' && defSide.sideConditions.reflect) c.infiltrator.physicalThroughReflect++;
				if (category === 'Special' && defSide.sideConditions.lightscreen) c.infiltrator.specialThroughLightScreen++;
				if (defSide.sideConditions.auroraveil) c.infiltrator.throughAuroraVeil++;
			}
		}
		return runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
	};
	const boost = Battle.prototype.boost;
	Battle.prototype.boost = function (boosts, target, source, effect, ...rest) {
		const c = t();
		// defog's `this.boost({ evasion: -1 })` passes no target, source or effect: they default to the running event's
		const eff = effect || this.effect, src = source || (this.event && this.event.source), tgt = target || (this.event && this.event.target);
		if (c && eff && eff.id === 'defog' && src && src.ability === 'infiltrator' && tgt && tgt.volatiles && tgt.volatiles.substitute && boosts && boosts.evasion) {
			c.infiltrator.defogIntoSubstitute++;
		}
		return boost.call(this, boosts, target, source, effect, ...rest);
	};
	const deductPP = Pokemon.prototype.deductPP;
	Pokemon.prototype.deductPP = function (move, amount, ...rest) {
		const c = t();
		if (c && amount && amount > 1) {
			const data = this.getMoveData(this.battle.dex.moves.get(move));
			if (data) {
				if (!data.pp) c.pressure.extraPPWasted++;
				else if (data.pp < amount) c.pressure.extraPPClamped++;
			}
		}
		return deductPP.call(this, move, amount, ...rest);
	};
	const add = Battle.prototype.add;
	Battle.prototype.add = function (...args) {
		const c = t();
		if (c) {
			if (args[0] === '-activate' && args[2] === 'move: Psychic Terrain' && this.activeMove && this.activeMove.pranksterBoosted) c.prankster.blockedByPsychicTerrain++;
		}
		return add.call(this, ...args);
	};
	// the hint that Showdown logs where a Dark type is immune to a Prankster move (hitStepTryImmunity)
	const hint = Battle.prototype.hint;
	Battle.prototype.hint = function (hintText, ...rest) {
		const c = t();
		if (c && typeof hintText === 'string' && hintText.includes('Dark is immune to Prankster')) {
			const user = this.activePokemon;
			const foe = user && user.side.foe.active[0];
			c.prankster.intoDark++;
			if (foe && foe.terastallized === 'Dark') c.prankster.intoDarkTera++;
		}
		return hint.call(this, hintText, ...rest);
	};
	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, comparator) {
		const c = t();
		if (c && list && list.length > 1 && list[0] && list[0].choice !== undefined) {
			// two move actions that tie on order, priority and speed (the sort draws): one or both Prankster-boosted
			const moves = list.filter(a => a.choice === 'move' && a.move);
			if (moves.length === 2 && moves[0].order === moves[1].order && moves[0].priority === moves[1].priority && moves[0].speed === moves[1].speed) {
				if (moves[0].move.pranksterBoosted && moves[1].move.pranksterBoosted) c.prankster.tieWithPrankster++;
				else if (moves[0].move.pranksterBoosted || moves[1].move.pranksterBoosted) c.prankster.tieWithPriorityAttack++;
			}
		}
		return speedSort.call(this, list, comparator);
	};
	const runMove = BattleActions.prototype.runMove;
	BattleActions.prototype.runMove = function (moveOrMoveName, pokemon, targetLoc, options) {
		const c = t();
		const m = typeof moveOrMoveName === 'string' ? dex.moves.get(moveOrMoveName) : moveOrMoveName;
		if (c && m && m.id === 'struggle' && pokemon.side.foe.active[0] && pokemon.side.foe.active[0].ability === 'pressure') c.pressure.struggleAfterPressure++;
		if (c && m && m.pranksterBoosted && pokemon.ability === 'prankster') {
			c.prankster.priorityRaised++;
			const foe = pokemon.side.foe.active[0];
			if (['self', 'allySide', 'all', 'foeSide', 'allyTeam'].includes(m.target)) c.prankster.selfSideOrField++;
			else if (foe && !hasType(foe, 'Dark')) c.prankster.intoNonDark++;
		}
		return runMove.call(this, moveOrMoveName, pokemon, targetLoc, options);
	};
}

const STRUCTURAL_ZERO = [
	// a Pressure target that has fainted is only reached by a move on the whole field after the foe's recoil faint: rare, and its
	// effect (a fainted Pokemon's Pressure still counts) is checked by the unit of the same name below when it occurs
	'pressure.onFaintedFoe',
	// every maximum PP is a multiple of 8 (the base PP is a multiple of 5, and the maximum is base * 8 / 5), so the one extra PP of a Pressure
	// target always finds PP left after the move's own, and a second Pressure target does not exist in singles
	'pressure.extraPPWasted', 'pressure.extraPPClamped',
];

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
		console.error('usage: node gen-slice8e.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice8e-${k}.json`), JSON.stringify(pool[k]));
	}
	activeTally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scriptedCount = 0, dropped = 0;
	const kinds = {};
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scriptedCount++;
		if (result.dropped) {
			dropped++;
			kinds[scenario.kind] = (kinds[scenario.kind] || 0) + 1;
			if (process.env.SLICE8E_VERBOSE) console.error(`seed ${seed}: script dropped (${scenario.kind}, turn ${(scenario.choices || []).length}): ${result.dropped}`);
		}
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice8e-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice8e seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped ${JSON.stringify(kinds)}), ${truncated} truncated at the turn limit, ` +
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
