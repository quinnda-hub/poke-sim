#!/usr/bin/env node
'use strict';
// Generates battles for "slice 6" (mid-turn switching; phase 6a: moves that switch their user out, 6b: moves that force the target
// out) and records each on Showdown.
//
// Slice 6a lets in U-turn, Flip Turn, Volt Switch (damaging), Teleport (`onTry`), Parting Shot (`onHit`) and Chilly Reception (snow and
// a `priorityChargeMove` action): a move with `selfSwitch: true` sets the user's `switchFlag` to its id at the end of
// `runMoveEffects` (battle-actions.ts 1369-1372), which turns into a switch request in the middle of the turn, a switch that runs
// before the rest of the turn, and a `runSwitch` that fires the replacement's switch-in effects at once.
//
// The teams are slice 5's (gen-slice5.js `assign`: slice 1's teams under slice 3's hazards, weather, terrain and Paradox, with a Tera
// type on every set), then each Pokemon gets a pivot move from a generator of its own, so the earlier slices stay reproducible from
// their seeds. Every scenario is recorded with `tera: true` and a `teraChance` of 0.1-0.5 (record.js), so a side terastallizes at
// varied turns. About a third of the battles are the plain random policy over those teams; the rest are scripted openings (a pair of
// choices per decision point; the random policy takes over when the script ends, and a script that Showdown rejects is dropped and
// the battle recorded without it) that put each case in front of the engine:
//   hazards   a hazard setter (or two) against a pivot user, then the pivot: each of the 6 moves into each of the 4 hazards (the
//             seed picks the pair, so the 24 combinations all occur), and into Toxic Spikes with a grounded Poison type that absorbs them
//   rocky     a Shedinja (1 HP) pivots into a Rocky Helmet holder (contact moves) or holds Life Orb: the user faints after the flag is
//             set, so it never switches (the end-of-turn replacement is the only request)
//   ko        a pivot (damaging) that knocks out a Shedinja: a mid-turn request for the user's side, then the foe's replacement at the
//             end of the same turn
//   solo      a side with one Pokemon: the pivot moves with no switch target (Teleport fails, Parting Shot's drops stay and the move
//             succeeds without switching, U-turn only damages, Chilly Reception sets snow and nothing more)
//   both      both leads pivot in the same turn (two requests in one turn)
//   fast      a fast pivot user against a slow attacker: the foe's move then hits the replacement
//   tera      the pivot user terastallizes on the turn it pivots
//   ground    Volt Switch into a Ground type (immune: no switch)
//   psblock   Parting Shot repeated until the foe's Attack and Special Attack are at -6 (the drops fail: no switch), or Strength Sap to
//             -6 Attack first (one drop still happens: the switch does too)
//   chilly    Chilly Reception with snow absent, already up (Snow Warning), and on both mirrored sides (two `priorityChargeMove` actions
//             tie on speed)
//   effects   a pivot whose replacement has switch-in effects at once: a surge, a weather setter, a seed in its terrain, Protosynthesis
//             or Quark Drive in the field or with Booster Energy, Unburden after a seed
//
// Slice 6b lets in Roar and Whirlwind (Status, priority -6), Dragon Tail and Circle Throw (damaging, priority -6): a move with `forceSwitch:
// true` sets the target's `forceSwitchFlag` in `spreadMoveHit` step 6 (battle-actions.ts 1413-1428), and the action tail's phazing step
// (battle.ts 2866-2873) calls `dragIn`: `getRandomSwitchable` draws `random(n)` over the target side's benched Pokemon that can switch
// (even for n = 1; no candidate, no draw), and `switchIn(..., isDrag)` runs `runSwitch` at once, with no Pursuit Update.
// A seed's 6b scenario comes from a generator of its own (the 6b share of the seeds replaces the 6a kind the seed would have had; every
// other seed is exactly the 6a scenario, plus the 6b sprinkle of forcing moves and hazards into its sets), and it is one of:
//   dhazards  a hazard setter that also forces (the seed picks the pair: the 4 moves into the 4 hazards, 16 combinations), the hazard(s)
//             first, then the forcing move: the dragged-in Pokemon (one of several candidates) meets the hazards
//   dfaint    a Shedinja forces its copy in onto Stealth Rock: the dragged-in Pokemon faints on arrival (resolved inside `runSwitch`'s
//             `fieldEvent`) with the same speed as the user, so the tail's Update sort must not count it
//   dtspikes  Toxic Spikes against a lone benched grounded Poison type (n = 1): the drag absorbs them
//   dsingle   the target side has two Pokemon: one candidate, and the draw with n = 1
//   dnone     the target side has one Pokemon: Roar and Whirlwind fail, Dragon Tail and Circle Throw only damage
//   dko       Dragon Tail knocks out a Shedinja (no drag; the end-of-turn replacement is the only request)
//   dhelmet   a Shedinja forces into a Rocky Helmet: the user faints (a lone one ends the battle) and the drag still happens
//   dtwo      both leads force: the faster drags the slower out before it moves (its move is cancelled)
//   deffects  the dragged-in Pokemon has switch-in effects at once: a surge, a weather setter, a seed, Protosynthesis, Quark Drive,
//             Booster Energy, Unburden
//   dpivot    a drag in the same turn as a 6a pivot: the forcing move drags out the pivot's replacement
//   dtera     the forcing user terastallizes on the turn it forces, or the target does on the turn it is dragged
//
// Usage:
//   node gen-slice6.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice6-<seed>.json, plus slice6-moves.json, slice6-species.json, slice6-abilities.json and slice6-items.json
// (copies of the pool, which the parity test compares with what the engine accepts). A tally of the outcomes that distinguish each
// part of the mechanic is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers pass
// every argument and result through and call nothing the battle would not call; stored fields, not getTypes or hasType, which run events).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { BattleActions } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'battle-actions'));
const { BattleQueue } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'battle-queue'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');
const { assign: assign5 } = require('./gen-slice5.js');

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

const PIVOTS = ['uturn', 'flipturn', 'voltswitch', 'teleport', 'partingshot', 'chillyreception'];
const DAMAGING = ['uturn', 'flipturn', 'voltswitch'];
const CONTACT = ['uturn', 'flipturn'];
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];
const STATUS_FORCERS = ['roar', 'whirlwind'];
const DAMAGING_FORCERS = ['dragontail', 'circlethrow'];
const HAZARDS = ['stealthrock', 'spikes', 'toxicspikes', 'stickyweb'];
const TERRAIN_ABILITY = { electricterrain: 'electricsurge', grassyterrain: 'grassysurge', mistyterrain: 'mistysurge', psychicterrain: 'psychicsurge' };
const SEED_OF = { electricterrain: 'electricseed', grassyterrain: 'grassyseed', mistyterrain: 'mistyseed', psychicterrain: 'psychicseed' };
const SETTERS = ['drought', 'drizzle', 'sandstream', 'snowwarning', 'electricsurge', 'grassysurge', 'mistysurge', 'psychicsurge'];

const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 3141592653 + 2718281829);
	// the 6b scenarios draw from a stream of their own (`rng` points at it while they are built), so a seed that gets no 6b kind keeps
	// exactly its 6a scenario
	const rnd6b = mulberry32(seed * 2246822519 + 3266489917);
	let rng = rnd;
	const int = (n) => Math.floor(rng() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	for (const id of PIVOTS) if (!has(id)) throw new Error(`the pool has no ${id}`);
	// slice 5's teams (a Struggle-profile base has 1-3 Pokemon: the scripted kinds need six on both sides, for `team 123456`)
	const short = (sc) => sc.teams[0].length !== 6 || sc.teams[1].length !== 6;
	let scenario = assign5(scenario0, pool, seed);
	for (let k = 1; short(scenario) && k < 30; k++) {
		const s2 = seed + 1000003 * k;
		scenario = assign5(makeScenario(s2, 'gen9ou', pool), pool, s2);
	}
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const byType = (t) => pool.species.filter(id => dex.species.get(id).types.includes(t));
	const bySpeed = pool.species.filter(id => id !== 'shedinja').sort((x, y) => dex.species.get(x).baseStats.spe - dex.species.get(y).baseStats.spe);
	const third = Math.max(1, Math.floor(bySpeed.length / 3));
	const speciesName = (id) => dex.species.get(id).name;
	const poison = byType('Poison').filter(id => !dex.species.get(id).types.includes('Flying'));
	const ground = byType('Ground'), flyingOrNot = pool.species.filter(id => !dex.species.get(id).types.includes('Ground') && id !== 'shedinja');

	const put = (s, id, slot) => {
		if (s.moves.includes(id)) {
			if (slot === undefined) return;
			s.moves.splice(s.moves.indexOf(id), 1);
		}
		const at = slot === undefined ? int(4) : slot;
		s.moves.splice(at, 0, id);
		s.moves = [...new Set(s.moves)].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const idleMoves = (s, ...first) => {
		s.moves = [...new Set([...first, 'recover', 'swordsdance', 'calmmind', 'toxic'].filter(id => has(id)))].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const steady = (s) => {
		s.level = 100;
		if (s.item === 'Assault Vest') s.item = '';
		if (s.species === 'Shedinja') s.species = speciesName(pick(flyingOrNot));
	};
	const setAbility = (s, id) => { s.ability = abilityName(id); };
	const setItem = (s, id) => { s.item = itemName(id); };
	const lead = (side) => scenario.teams[side][0];
	const bench = (side, i) => scenario.teams[side][i];

	// every set gets a pivot with some probability (from this generator, so the base teams stay slice 5's)
	const sprinkle = (s) => {
		if (int(100) < 45) put(s, pick(PIVOTS));
		if (int(100) < 10) put(s, pick(PIVOTS));
		if (int(100) < 12 && pool.items.includes('rockyhelmet')) setItem(s, 'rockyhelmet');
	};
	for (const side of [0, 1]) for (const s of scenario.teams[side]) sprinkle(s);

	const kindRoll = int(100);
	const kinds = [
		['random', 33], ['hazards', 18], ['rocky', 5], ['ko', 7], ['solo', 5], ['both', 5], ['fast', 5], ['tera', 5], ['ground', 3],
		['psblock', 4], ['chilly', 4], ['effects', 6],
	];
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (kindRoll < acc) { kind = k; break; } }
	const TEAM = ['team 123456', 'team 123456'];
	const step = (...pair) => pair;

	// ---- 6b: the forcing moves
	let kind6b = null;
	if (FORCERS.every(has)) {
		rng = rnd6b;
		for (const side of [0, 1]) for (const s of scenario.teams[side]) {
			if (int(100) < 22) put(s, pick(FORCERS));
			if (int(100) < 7) put(s, pick(HAZARDS));
		}
		const kinds6b = [
			['dhazards', 10], ['dfaint', 3], ['dtspikes', 4], ['dsingle', 3], ['dnone', 3], ['dko', 3], ['dhelmet', 4], ['dtwo', 4], ['deffects', 6], ['dpivot', 4], ['dtera', 4],
		];
		const roll6b = int(100);
		let acc6b = 0;
		for (const [k, w] of kinds6b) { acc6b += w; if (roll6b < acc6b) { kind6b = k; break; } }
		if (!kind6b) rng = rnd;
	}
	if (kind6b) {
		const fsd = int(2); // the side that forces
		const ts = 1 - fsd; // the side that is forced out
		const F = lead(fsd), T = lead(ts);
		const duo = (f, t) => { const pair = [null, null]; pair[fsd] = f; pair[ts] = t; return pair; };
		const team = (n) => { const pair = ['team 123456', 'team 123456']; pair[ts] = n === 6 ? 'team 123456' : `team ${[1, 2, 3, 4, 5, 6].slice(0, n).join('')}`; return pair; };
		const shrink = (n) => { scenario.teams[ts] = scenario.teams[ts].slice(0, n); };
		const noBoots = () => { for (let i = 1; i < scenario.teams[ts].length; i++) if (bench(ts, i).item === 'Heavy-Duty Boots' && int(100) < 85) bench(ts, i).item = ''; };
		const forcer = pick(FORCERS);
		const hitsByType = (type) => pool.species.filter(id => id !== 'shedinja' && dex.getImmunity(type, dex.species.get(id).types));
		steady(F); steady(T);
		if (kind6b === 'dhazards') {
			const combo = (seed * 7) % 16;
			const move = FORCERS[combo % 4], hz = HAZARDS[Math.floor(combo / 4)];
			const second = int(100) < 30 ? pick(HAZARDS.filter(h => h !== hz)) : null;
			const hzs = second ? [hz, second] : [hz];
			idleMoves(F, ...hzs, move);
			idleMoves(T, 'recover');
			noBoots();
			const choices = [team(6)];
			for (let i = 0; i < hzs.length; i++) choices.push(duo(`move ${i + 1}`, 'move 1'));
			choices.push(duo(`move ${hzs.length + 1}`, 'move 1'));
			if (int(100) < 40) choices.push(duo(`move ${hzs.length + 1}`, 'move 1'));
			scenario.choices = choices;
		} else if (kind6b === 'dfaint') {
			// a Shedinja forces a copy of itself in (R: the same set, so the same speed) onto Stealth Rock: R faints on arrival, inside
			// runSwitch's fieldEvent, so the tail's Update event sorts the Pokemon in play (a tie draws) without it
			shrink(2);
			F.species = 'Shedinja'; F.level = 100; F.ability = 'No Ability'; F.item = '';
			idleMoves(F, 'stealthrock', forcer);
			const R = bench(ts, 1);
			Object.assign(R, JSON.parse(JSON.stringify(F)));
			idleMoves(R, 'recover');
			idleMoves(T, 'recover');
			scenario.choices = [team(2), duo('move 1', 'move 1'), duo('move 2', 'move 1')];
		} else if (kind6b === 'dtspikes') {
			shrink(2);
			const R = bench(ts, 1);
			if (poison.length) R.species = speciesName(pick(poison));
			R.item = ''; R.level = 100;
			idleMoves(F, 'toxicspikes', forcer);
			idleMoves(T, 'recover');
			scenario.choices = [team(2), duo('move 1', 'move 1'), duo('move 2', 'move 1')];
		} else if (kind6b === 'dsingle') {
			shrink(2);
			noBoots();
			idleMoves(F, forcer);
			idleMoves(T, 'recover');
			scenario.choices = [team(2), duo('move 1', 'move 1')];
		} else if (kind6b === 'dnone') {
			shrink(1);
			idleMoves(F, forcer);
			idleMoves(T, 'recover');
			scenario.choices = [team(1), duo('move 1', 'move 1'), duo('move 1', 'move 1')];
		} else if (kind6b === 'dko') {
			T.species = 'Shedinja'; T.level = 100; T.ability = 'No Ability'; T.item = '';
			idleMoves(F, 'dragontail');
			idleMoves(T, 'recover');
			const repl = duo(null, 'switch 2');
			scenario.choices = [team(6), duo('move 1', 'move 1'), repl];
		} else if (kind6b === 'dhelmet') {
			const move = pick(DAMAGING_FORCERS);
			F.species = 'Shedinja'; F.level = 100; F.ability = 'No Ability'; F.item = '';
			const types = hitsByType(dex.moves.get(move).type);
			if (types.length) T.species = speciesName(pick(types));
			setItem(T, 'rockyhelmet');
			idleMoves(F, move);
			idleMoves(T, 'recover');
			const lone = int(100) < 25;
			if (lone) scenario.teams[fsd] = scenario.teams[fsd].slice(0, 1);
			const first = [null, null]; first[fsd] = lone ? 'team 1' : 'team 123456'; first[ts] = 'team 123456';
			scenario.choices = [first, duo('move 1', 'move 1'), duo('switch 2', null)];
		} else if (kind6b === 'dtwo') {
			idleMoves(F, forcer);
			idleMoves(T, pick(FORCERS));
			if (int(100) < 40) { T.species = F.species; T.level = 100; }
			scenario.choices = [team(6), duo('move 1', 'move 1'), duo('move 1', 'move 1')];
		} else if (kind6b === 'deffects') {
			shrink(2);
			const R = bench(ts, 1);
			R.level = 100; R.item = ''; R.ability = 'No Ability'; F.item = ''; T.item = '';
			idleMoves(F, forcer);
			idleMoves(T, 'recover');
			const variant = int(7);
			const terrains = Object.keys(TERRAIN_ABILITY).filter(t => pool.abilities.includes(TERRAIN_ABILITY[t]) && pool.items.includes(SEED_OF[t]));
			if (variant <= 2 && terrains.length) {
				const t = pick(terrains);
				setAbility(F, TERRAIN_ABILITY[t]);
				setItem(R, SEED_OF[t]);
				if (variant === 2 && pool.abilities.includes('unburden')) setAbility(R, 'unburden');
			} else if (variant === 3) {
				setAbility(F, 'drought'); setAbility(R, 'protosynthesis');
			} else if (variant === 4) {
				setAbility(F, 'electricsurge'); setAbility(R, 'quarkdrive');
			} else if (variant === 5) {
				setAbility(R, pick(['protosynthesis', 'quarkdrive'])); setItem(R, 'boosterenergy');
			} else {
				setAbility(R, pick(SETTERS.filter(id => pool.abilities.includes(id))));
			}
			scenario.choices = [team(2), duo('move 1', 'move 1')];
		} else if (kind6b === 'dpivot') {
			// the other side pivots (priority 0, or Teleport at -6, tying with the forcing move): its replacement is then dragged out
			const pivot = pick(PIVOTS);
			idleMoves(F, forcer);
			idleMoves(T, pivot);
			noBoots();
			const sw = [null, null]; sw[ts] = 'switch 2';
			scenario.choices = [team(6), duo('move 1', 'move 1'), sw];
		} else {
			// dtera
			idleMoves(F, forcer);
			idleMoves(T, 'recover');
			noBoots();
			const v = int(3);
			scenario.choices = [team(6), duo(v === 1 ? 'move 1' : 'move 1 terastallize', v === 0 ? 'move 1' : 'move 1 terastallize')];
		}
		rng = rnd;
		kind = 'b6';
	}
	if (kind === 'b6') {
		// built above
	} else if (kind === 'hazards') {
		// the seed picks (move, hazard): 6 x 4 = 24 combinations
		const combo = (seed * 7) % 24;
		const move = PIVOTS[combo % 6], hz = HAZARDS[Math.floor(combo / 6)];
		const A = lead(0), B = lead(1);
		steady(A); steady(B);
		idleMoves(A); put(A, move, 0); put(A, 'recover', 1);
		const second = int(100) < 30 ? pick(HAZARDS.filter(h => h !== hz)) : null;
		idleMoves(B); put(B, hz, 0); if (second) put(B, second, 1); put(B, 'recover', 2);
		if (move === 'voltswitch') B.species = speciesName(pick(flyingOrNot));
		else if (int(100) < 5) B.species = speciesName(pick(flyingOrNot));
		// the replacement: a grounded Poison type when Toxic Spikes (it absorbs them), otherwise any, and not holding Boots most of the time
		const R = bench(0, 1);
		if ((hz === 'toxicspikes' || second === 'toxicspikes') && poison.length) R.species = speciesName(pick(poison));
		if (R.item === 'Heavy-Duty Boots' && int(100) < 85) R.item = '';
		if (hz === 'toxicspikes' || second === 'toxicspikes') { if (R.item === 'Heavy-Duty Boots') R.item = ''; R.level = 100; }
		const choices = [TEAM, step('move 2', 'move 1')];
		if (second) choices.push(step('move 2', 'move 2'));
		choices.push(step('move 1', 'move 3'), step('switch 2', null));
		scenario.choices = choices;
	} else if (kind === 'rocky') {
		const A = lead(0), B = lead(1);
		steady(A); steady(B);
		A.species = 'Shedinja'; A.level = 100;
		A.ability = 'No Ability';
		const move = pick(CONTACT);
		idleMoves(A); put(A, move, 0);
		idleMoves(B); put(B, 'recover', 0);
		if (int(100) < 60 && pool.items.includes('rockyhelmet')) { setItem(B, 'rockyhelmet'); A.item = ''; }
		else if (pool.items.includes('lifeorb')) { setItem(A, 'lifeorb'); put(A, pick(DAMAGING), 0); }
		scenario.choices = [TEAM, step('move 1', 'move 1'), step('switch 2', null)];
	} else if (kind === 'ko') {
		const A = lead(0), B = lead(1);
		steady(A); steady(B);
		B.species = 'Shedinja'; B.level = 100; B.ability = 'No Ability'; B.item = '';
		idleMoves(B); put(B, 'recover', 0);
		idleMoves(A); put(A, pick(DAMAGING), 0);
		scenario.choices = [TEAM, step('move 1', 'move 1'), step('switch 2', null), step(null, 'switch 2')];
	} else if (kind === 'solo') {
		const keep = int(100) < 30 ? 0 : 1;
		scenario.teams[0] = scenario.teams[0].slice(0, 1);
		if (keep === 0) scenario.teams[1] = scenario.teams[1].slice(0, 1);
		const A = lead(0);
		const ids = ['teleport', 'partingshot', 'chillyreception', 'uturn', 'flipturn', 'voltswitch'];
		for (let i = 0; i < 3; i++) put(A, ids[(int(6) + i) % 6], i);
		if (int(100) < 40) A.level = 100;
		if (keep === 0) { const B = lead(1); put(B, pick(PIVOTS)); }
		scenario.choices = [['team 1', keep === 0 ? 'team 1' : 'team 123456']];
	} else if (kind === 'both') {
		const A = lead(0), B = lead(1);
		steady(A); steady(B);
		idleMoves(A); idleMoves(B);
		put(A, pick(PIVOTS), 0); put(B, pick(PIVOTS), 0);
		if (int(100) < 40) { B.species = A.species; B.level = 100; }
		scenario.choices = [TEAM, step('move 1', 'move 1'), step('switch 2', 'switch 2'), step('switch 2', 'switch 2')];
	} else if (kind === 'fast') {
		const side = int(2);
		const A = lead(side), B = lead(1 - side);
		steady(A); steady(B);
		A.species = speciesName(pick(bySpeed.slice(-third)));
		B.species = speciesName(pick(bySpeed.slice(0, third)));
		const attacks = pool.moves.filter(id => { const m = dex.moves.get(id); return m.basePower >= 40 && m.accuracy === 100 && m.category !== 'Status' && !PIVOTS.includes(id); });
		idleMoves(A); put(A, pick(PIVOTS), 0);
		idleMoves(B); put(B, pick(attacks), 0);
		const pair = [null, null];
		pair[side] = 'move 1'; pair[1 - side] = 'move 1';
		const sw = [null, null]; sw[side] = 'switch 2';
		scenario.choices = [TEAM, pair, sw];
	} else if (kind === 'tera') {
		const side = int(2);
		const A = lead(side), B = lead(1 - side);
		steady(A); steady(B);
		idleMoves(A); put(A, pick(PIVOTS), 0);
		idleMoves(B); put(B, 'recover', 0);
		const pair = [null, null];
		pair[side] = 'move 1 terastallize'; pair[1 - side] = 'move 1';
		const sw = [null, null]; sw[side] = 'switch 2';
		scenario.choices = [TEAM, pair, sw];
	} else if (kind === 'ground') {
		const A = lead(0), B = lead(1);
		steady(A); steady(B);
		idleMoves(A); put(A, 'voltswitch', 0);
		idleMoves(B); put(B, 'recover', 0);
		if (ground.length) B.species = speciesName(pick(ground));
		scenario.choices = [TEAM, step('move 1', 'move 1'), step('move 1', 'move 1')];
	} else if (kind === 'psblock') {
		const A = lead(0), B = lead(1);
		steady(A); steady(B);
		idleMoves(B); put(B, 'recover', 0);
		B.item = ''; B.ability = 'No Ability';
		const choices = [TEAM];
		if (int(100) < 55 || !has('strengthsap')) {
			// both drops blocked at -6: Parting Shot from every member in turn, the 7th use fails to drop
			for (const s of scenario.teams[0]) { idleMoves(s); put(s, 'partingshot', 0); }
			for (let i = 0; i < 7; i++) choices.push(step('move 1', 'move 1'), step('switch 2', null));
		} else {
			// Strength Sap to -6 Attack, then Parting Shot: Special Attack still drops, so the user still switches
			idleMoves(A); put(A, 'strengthsap', 0); put(A, 'partingshot', 1);
			for (let i = 0; i < 6; i++) choices.push(step('move 1', 'move 1'));
			choices.push(step('move 2', 'move 1'), step('switch 2', null));
		}
		scenario.choices = choices;
	} else if (kind === 'chilly') {
		const A = lead(0), B = lead(1);
		steady(A); steady(B);
		const v = int(3);
		idleMoves(A); put(A, 'chillyreception', 0);
		if (v === 2) {
			// both sides, mirrored: the two priorityChargeMove actions tie on speed
			scenario.teams[1] = JSON.parse(JSON.stringify(scenario.teams[0]));
			scenario.choices = [TEAM, step('move 1', 'move 1'), step('switch 2', 'switch 2'), step('switch 2', 'switch 2')];
		} else {
			idleMoves(B); put(B, 'recover', 0);
			if (v === 1) { setAbility(A, 'snowwarning'); }
			scenario.choices = [TEAM, step('move 1', 'move 1'), step('switch 2', null)];
		}
	} else if (kind === 'effects') {
		const A = lead(0), B = lead(1), R = bench(0, 1);
		steady(A); steady(B); R.level = 100;
		idleMoves(A); put(A, pick(PIVOTS), 0);
		idleMoves(B); put(B, 'recover', 0);
		B.item = ''; A.item = ''; R.item = ''; R.ability = 'No Ability';
		const variant = int(7);
		const terrains = Object.keys(TERRAIN_ABILITY).filter(t => pool.abilities.includes(TERRAIN_ABILITY[t]) && pool.items.includes(SEED_OF[t]));
		if (variant <= 2 && terrains.length) {
			// a seed in the foe's terrain, sometimes with Unburden (its speed doubles when the seed goes)
			const t = pick(terrains);
			setAbility(B, TERRAIN_ABILITY[t]);
			setItem(R, SEED_OF[t]);
			if (variant === 2 && pool.abilities.includes('unburden')) setAbility(R, 'unburden');
		} else if (variant === 3) {
			setAbility(B, 'drought'); setAbility(R, 'protosynthesis');
		} else if (variant === 4) {
			setAbility(B, 'electricsurge'); setAbility(R, 'quarkdrive');
		} else if (variant === 5) {
			setAbility(R, pick(['protosynthesis', 'quarkdrive'])); setItem(R, 'boosterenergy');
		} else {
			setAbility(R, pick(SETTERS.filter(id => pool.abilities.includes(id))));
		}
		scenario.choices = [TEAM, step('move 1', 'move 1'), step('switch 2', null)];
	}
	// Coverage: the long tier needs every accepted species in some team of the newest set. The last bench member of the second side
	// (never part of a script: the scripts use the leads and slot 2) takes the species at the seed's place in the pool, so seeds 1 to
	// the pool's size cover it whole; a one-Pokemon team (the `solo` kind) takes it as its lead.
	const covered = scenario.teams[1];
	covered[covered.length === 6 ? 5 : 0].species = speciesName(pool.species[(seed - 1) % pool.species.length]);
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally: observation hooks on Showdown's classes. Nothing is changed: each wrapper calls the original with every argument and
// returns its result.

let activeTally = null;
function newTally() {
	const z = (keys) => Object.fromEntries(keys.map(k => [k, 0]));
	const cross = (as, bs) => Object.fromEntries(as.flatMap(a => bs.map(b => [`${a}.${b}`, 0])));
	return {
		// the flag, by move: set (the user has HP and the side can switch), and what stopped it
		selfSwitchFlagSet: z(PIVOTS),
		selfSwitchNoTarget: z(PIVOTS),
		selfSwitchIntoImmunity: z(['voltswitch']),
		voltSwitchIntoGround: 0,
		teleportFailedNoTarget: 0, teleportSwitched: 0,
		// a flagged user that then fainted: the faint cleared the flag
		flaggedUserFainted: z(['rockyhelmet', 'lifeorb', 'other']),
		// requests in the middle of the turn
		midTurnSwitchRequest: 0, midTurnRequestBothSides: 0, bothSidesPivotSameTurn: 0,
		pivotKnockedOutTarget: z(DAMAGING), pivotKoThenEndOfTurnReplacement: 0, pivotThenFaintReplacementSameTurn: 0,
		// the switch itself
		pivotSwitchIns: z(PIVOTS),
		cancelActionRemovedSomething: 0,
		moveHitsPivotIn: 0, moveHitsPivotInFasterPivoter: 0,
		teraAndPivotSameTurn: 0, pivotAfterTeraEarlierTurn: 0, teraPokemonPivotedOut: 0,
		// hazards that fired on the replacement of a pivot, by move
		pivotIntoHazard: cross(PIVOTS, HAZARDS),
		pivotAbsorbsToxicSpikes: z(PIVOTS),
		pivotInFaintedToHazard: 0, pivotInWithBoots: 0,
		// the replacement's other switch-in effects (they run in the same turn, before the rest of it)
		pivotInSetWeather: 0, pivotInSetTerrain: 0, pivotInSeedUsed: 0, pivotInBoosterUsed: 0,
		pivotInProtosynthesis: 0, pivotInQuarkDrive: 0, pivotInUnburden: 0, pivotInBoosted: 0,
		// Parting Shot's onHit
		partingShotDropped: 0, partingShotOneDropOnly: 0, partingShotBlocked: 0, partingShotNoTargetDropsStay: 0,
		// Chilly Reception: snow, the priority charge and its volatile
		chillySnowSetAndSwitched: 0, chillySnowAlreadyUpSwitched: 0, chillySnowSetNoTarget: 0, chillyFailedSnowUpNoTarget: 0,
		priorityChargeMoveRan: 0, priorityChargeBothSides: 0, priorityChargeSpeedTie: 0, chillyVolatileAdded: 0,
		// 6b: the flag (spreadMoveHit step 6), by move, and what stopped it
		forceFlagSet: z(FORCERS),
		forceNoTargetStatusFailed: z(STATUS_FORCERS), forceNoTargetDamaged: z(DAMAGING_FORCERS),
		forceTargetKnockedOut: z(DAMAGING_FORCERS),
		// the drag (the phazing step and dragIn)
		dragged: z(FORCERS), dragOneCandidate: 0, dragSeveralCandidates: 0, dragBenchHasFainted: 0,
		dragCancelledMove: 0, dragCancelledMinus6Move: 0,
		dragAfterUserFainted: 0, dragAfterBattleEnded: 0,
		dragSameTurnAsPivot: 0, dragOutPivotReplacement: 0,
		dragTeraForcerSameTurn: 0, dragTeraTarget: 0, dragTeraTargetSameTurn: 0,
		dragIntoHazard: cross(FORCERS, HAZARDS), dragAbsorbsToxicSpikes: z(FORCERS),
		dragInFaintedToHazard: 0, dragInWithBoots: 0, dragInFaintedSpeedTie: 0,
		dragInSetWeather: 0, dragInSetTerrain: 0, dragInSeedUsed: 0, dragInBoosterUsed: 0,
		dragInProtosynthesis: 0, dragInQuarkDrive: 0, dragInUnburden: 0, dragInBoosted: 0,
		// structurally zero
		// - dragOutDeclined: no supported effect listens to DragOut (Suction Cups, Guard Dog, Ingrain are unsupported), so no hit
		//   result is false and the drag is never refused
		// - dragFlaggedTargetNoHp: the flag is set for a target with HP and consumed at the end of the same action, and nothing in
		//   between (the user's DamagingHit and Update events) can hurt the target
		// - selfSwitchNoTarget.teleport: with no switch target Teleport's onTry fails it before runMoveEffects, which is where the flag
		//   (and this counter) is decided; teleportFailedNoTarget counts it
		// - selfSwitchIntoImmunity of U-turn and Flip Turn: no type is immune to Bug or Water, and no supported ability grants it
		// - flaggedUserFainted.other: between the flag (runMoveEffects) and the request only DamagingHit (Rocky Helmet; Rough Skin is
		//   unsupported) and AfterMoveSecondarySelf (Life Orb) can hurt the user, and no pivot has recoil
		// - midTurnRequestBothSides: a flag is turned into a request at the end of the action that set it, so one side is flagged
		//   at a time in singles
		// - cancelActionRemovedSomething: a user's own queued actions are consumed before its move ends (its move, its terastallize and
		//   its priorityChargeMove run earlier), and nothing queues one for a Pokemon that is already out; the call is mirrored
		//   (battle-actions.ts 124-127) and cannot be observed to remove anything in 6a
		// - partingShotBlockedByAbility: no supported ability blocks a drop (Clear Body, White Smoke, Mirror Armor, Hyper Cutter), so
		//   the only blocked drop is -6
		// - roughSkin: Rough Skin is not a supported ability, so Rocky Helmet is the only contact punisher
		// - copyvolatile, shedtail (Baton Pass, Shed Tail: banned in gen9ou), revivalblessing, a commanding Tatsugiri
		partingShotBlockedByAbility: 0, roughSkinFaintedUser: 0, batonPassOrShedTail: 0, revivalBlessing: 0, commandedUser: 0,
		dragOutDeclined: 0, dragFlaggedTargetNoHp: 0,
	};
}

const STRUCTURAL_ZERO = ['selfSwitchNoTarget.teleport', 'flaggedUserFainted.other', 'midTurnRequestBothSides', 'cancelActionRemovedSomething', 'partingShotBlockedByAbility', 'roughSkinFaintedUser', 'batonPassOrShedTail',
	'revivalBlessing', 'commandedUser', 'dragOutDeclined', 'dragFlaggedTargetNoHp'];

function install() {
	if (Battle.prototype.__slice6Instrumented) return;
	Battle.prototype.__slice6Instrumented = true;
	const t = () => activeTally;
	const teraTurn = new WeakMap(); // Pokemon -> the turn it terastallized
	const pivotIn = new WeakMap(); // Pokemon -> { move, turn, faster } for a replacement switched in by a pivot move
	const state = { pivotSwitch: false, pivotRunSwitch: false, drag: false, dragRunSwitch: false };
	const forcedBy = new WeakMap(); // Pokemon -> { move, source, turn } while its forceSwitchFlag is set
	const dragCtx = new WeakMap(); // Pokemon -> { move, turn } for the Pokemon a drag is bringing in

	const terastallize = BattleActions.prototype.terastallize;
	BattleActions.prototype.terastallize = function (pokemon) {
		const r = terastallize.call(this, pokemon);
		if (pokemon.terastallized) teraTurn.set(pokemon, this.battle.turn);
		return r;
	};

	// the flag: runMoveEffects of a selfSwitch move's primary hit
	const runMoveEffects = BattleActions.prototype.runMoveEffects;
	BattleActions.prototype.runMoveEffects = function (damage, targets, source, move, moveData, isSecondary, isSelf) {
		const c = t();
		const watched = c && move.selfSwitch && moveData === move && !isSecondary && !isSelf && PIVOTS.includes(move.id);
		if (!watched) return runMoveEffects.call(this, damage, targets, source, move, moveData, isSecondary, isSelf);
		const battle = this.battle;
		const canSwitch = !!battle.canSwitch(source.side);
		const target = targets[0] || null;
		const weather = battle.field.weather;
		const boostsBefore = target ? { atk: target.boosts.atk, spa: target.boosts.spa } : null;
		const r = runMoveEffects.call(this, damage, targets, source, move, moveData, isSecondary, isSelf);
		const flagged = source.switchFlag === move.id;
		if (flagged) {
			c.selfSwitchFlagSet[move.id]++;
			if (move.id === 'teleport') c.teleportSwitched++;
			if (target && target.hp === 0 && DAMAGING.includes(move.id)) {
				c.pivotKnockedOutTarget[move.id]++;
				battle.__pivotKo = { turn: battle.turn, side: target.side };
			}
			const tt = teraTurn.get(source);
			if (tt === battle.turn) c.teraAndPivotSameTurn++;
			else if (tt !== undefined) c.pivotAfterTeraEarlierTurn++;
			if (source.terastallized) c.teraPokemonPivotedOut++;
			if (battle.__flagged && battle.__flagged.turn === battle.turn && battle.__flagged.side !== source.side) c.bothSidesPivotSameTurn++;
			battle.__flagged = { turn: battle.turn, side: source.side };
			const foe = source.side.foe.active[0];
			battle.__pivotUserFaster = !!(foe && source.speed > foe.speed);
			battle.__pivotMove = { move: move.id, turn: battle.turn };
		} else if (!canSwitch) {
			c.selfSwitchNoTarget[move.id]++;
		}
		if (move.id === 'partingshot' && target) {
			if (flagged) {
				c.partingShotDropped++;
				if (boostsBefore.atk === -6 || boostsBefore.spa === -6) c.partingShotOneDropOnly++;
			} else if (canSwitch && source.hp > 0) c.partingShotBlocked++;
			else if (!canSwitch && !(boostsBefore.atk === -6 && boostsBefore.spa === -6)) c.partingShotNoTargetDropsStay++;
		}
		if (move.id === 'chillyreception') {
			if (flagged && weather !== 'snowscape') c.chillySnowSetAndSwitched++;
			else if (flagged) c.chillySnowAlreadyUpSwitched++;
			else if (!canSwitch && weather !== 'snowscape') c.chillySnowSetNoTarget++;
			else if (!canSwitch) c.chillyFailedSnowUpNoTarget++;
		}
		return r;
	};

	// Teleport's onTry
	const trySpreadMoveHit = BattleActions.prototype.trySpreadMoveHit;
	BattleActions.prototype.trySpreadMoveHit = function (targets, pokemon, move, ...rest) {
		const r = trySpreadMoveHit.call(this, targets, pokemon, move, ...rest);
		const c = t();
		if (c && move.id === 'teleport' && !r) c.teleportFailedNoTarget++;
		return r;
	};

	// a pivot into a type immunity (Volt Switch into a Ground type): the hit steps stop before runMoveEffects
	const hitStepTypeImmunity = BattleActions.prototype.hitStepTypeImmunity;
	BattleActions.prototype.hitStepTypeImmunity = function (targets, pokemon, move) {
		const r = hitStepTypeImmunity.call(this, targets, pokemon, move);
		const c = t();
		if (c && move.selfSwitch && DAMAGING.includes(move.id) && r && r[0] === false) {
			c.selfSwitchIntoImmunity[move.id]++;
			if (move.id === 'voltswitch' && targets[0].types.includes('Ground')) c.voltSwitchIntoGround++;
		}
		return r;
	};

	// a flagged user that faints
	const faint = Pokemon.prototype.faint;
	Pokemon.prototype.faint = function (source, effect) {
		const c = t();
		if (c && typeof this.switchFlag === 'string' && !this.fainted && !this.faintQueued) {
			const id = effect && effect.id;
			c.flaggedUserFainted[id === 'rockyhelmet' || id === 'lifeorb' ? id : 'other']++;
			if (id === 'roughskin') c.roughSkinFaintedUser++;
		}
		return faint.call(this, source, effect);
	};

	// the switch-out (battle-actions.ts 100-127) and the replacement's arrival
	const cancelAction = BattleQueue.prototype.cancelAction;
	BattleQueue.prototype.cancelAction = function (pokemon) {
		const queuedMoves = this.list.filter(a => a.pokemon === pokemon && a.choice === 'move');
		const r = cancelAction.call(this, pokemon);
		const c = t();
		if (c && state.pivotSwitch && r) c.cancelActionRemovedSomething++;
		if (c && state.drag && queuedMoves.length) {
			c.dragCancelledMove++;
			if (queuedMoves.some(a => a.priority === -6)) c.dragCancelledMinus6Move++;
		}
		return r;
	};
	const switchIn = BattleActions.prototype.switchIn;
	BattleActions.prototype.switchIn = function (pokemon, pos, sourceEffect, ...rest) {
		const c = t();
		const pivot = !!(c && sourceEffect && sourceEffect.selfSwitch === true && PIVOTS.includes(sourceEffect.id));
		const old = pokemon.side.active[pos];
		state.pivotSwitch = pivot;
		const drag = !!(c && rest[0] === true);
		if (drag) {
			state.drag = true;
			dragCtx.set(pokemon, state.dragCtx);
		}
		const r = switchIn.call(this, pokemon, pos, sourceEffect, ...rest);
		state.pivotSwitch = false;
		state.drag = false;
		if (pivot && r !== false) {
			c.pivotSwitchIns[sourceEffect.id]++;
			pivotIn.set(pokemon, { move: sourceEffect.id, turn: this.battle.turn, faster: !!this.battle.__pivotUserFaster });
			if (sourceEffect.selfSwitch === true && old && old.terastallized && old !== pokemon) { /* counted at the flag */ }
		}
		return r;
	};

	// the replacement's runSwitch: hazards, abilities, seeds, Booster Energy, Unburden
	const runSwitch = BattleActions.prototype.runSwitch;
	BattleActions.prototype.runSwitch = function (pokemon) {
		const c = t();
		const info = c && pivotIn.get(pokemon);
		if (!info || info.turn !== this.battle.turn) return runSwitch.call(this, pokemon);
		const conds = pokemon.side.sideConditions;
		const had = Object.fromEntries(HAZARDS.map(h => [h, h in conds]));
		const types = pokemon.types.slice();
		const weather = this.battle.field.weather, terrain = this.battle.field.terrain;
		const item = pokemon.item;
		state.pivotRunSwitch = true;
		const r = runSwitch.call(this, pokemon);
		state.pivotRunSwitch = false;
		for (const h of HAZARDS) if (had[h]) c.pivotIntoHazard[`${info.move}.${h}`]++;
		if (had.toxicspikes && types.includes('Poison') && !types.includes('Flying') && !('toxicspikes' in pokemon.side.sideConditions)) {
			c.pivotAbsorbsToxicSpikes[info.move]++;
		}
		if (pokemon.hp === 0 && (had.stealthrock || had.spikes || had.toxicspikes)) c.pivotInFaintedToHazard++;
		if (item === 'heavydutyboots' && Object.values(had).some(Boolean)) c.pivotInWithBoots++;
		if (this.battle.field.weather !== weather) c.pivotInSetWeather++;
		if (this.battle.field.terrain !== terrain) c.pivotInSetTerrain++;
		if (item && !pokemon.item && item.endsWith('seed')) c.pivotInSeedUsed++;
		return r;
	};
	const useItem = Pokemon.prototype.useItem;
	Pokemon.prototype.useItem = function (...args) {
		const c = t();
		const id = this.item;
		const r = useItem.call(this, ...args);
		if (c && state.pivotRunSwitch && id === 'boosterenergy' && r) c.pivotInBoosterUsed++;
		if (c && state.dragRunSwitch && id === 'boosterenergy' && r) c.dragInBoosterUsed++;
		return r;
	};
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, ...rest) {
		const r = addVolatile.call(this, status, ...rest);
		const c = t();
		const id = typeof status === 'string' ? status : status.id;
		if (c && state.pivotRunSwitch && r) {
			if (id === 'protosynthesis') c.pivotInProtosynthesis++;
			if (id === 'quarkdrive') c.pivotInQuarkDrive++;
			if (id === 'unburden') c.pivotInUnburden++;
		}
		if (c && state.dragRunSwitch && r) {
			if (id === 'protosynthesis') c.dragInProtosynthesis++;
			if (id === 'quarkdrive') c.dragInQuarkDrive++;
			if (id === 'unburden') c.dragInUnburden++;
		}
		if (c && id === 'chillyreception' && r) c.chillyVolatileAdded++;
		return r;
	};
	const boost = Battle.prototype.boost;
	Battle.prototype.boost = function (boosts, target, ...rest) {
		const r = boost.call(this, boosts, target, ...rest);
		const c = t();
		if (c && state.pivotRunSwitch && r) c.pivotInBoosted++;
		if (c && state.dragRunSwitch && r) c.dragInBoosted++;
		return r;
	};

	// a foe's move that hits a replacement switched in earlier in the same turn
	const getDamage = BattleActions.prototype.getDamage;
	BattleActions.prototype.getDamage = function (source, target, move, ...rest) {
		const c = t();
		const info = c && target && pivotIn.get(target);
		if (info && info.turn === this.battle.turn && move && move.category !== 'Status' && source.side !== target.side && target.isActive) {
			c.moveHitsPivotIn++;
			if (info.faster) c.moveHitsPivotInFasterPivoter++;
		}
		return getDamage.call(this, source, target, move, ...rest);
	};

	// the priority charge actions
	const runAction = Battle.prototype.runAction;
	Battle.prototype.runAction = function (action) {
		const c = t();
		if (c && action.choice === 'priorityChargeMove') c.priorityChargeMoveRan++;
		return runAction.call(this, action);
	};
	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, ...rest) {
		const c = t();
		if (c && list.length >= 2 && list[0] && list[0].choice) {
			const charges = list.filter(a => a.choice === 'priorityChargeMove');
			if (charges.length === 2) {
				c.priorityChargeBothSides++;
				if (charges[0].speed === charges[1].speed) c.priorityChargeSpeedTie++;
			}
		}
		return speedSort.call(this, list, ...rest);
	};

	// 6b: the flag (spreadMoveHit step 6) and what stopped it
	const forceSwitch = BattleActions.prototype.forceSwitch;
	BattleActions.prototype.forceSwitch = function (damage, targets, source, move) {
		const before = targets.map(x => !!(x && x.forceSwitchFlag));
		const r = forceSwitch.call(this, damage, targets, source, move);
		const c = t();
		if (c && FORCERS.includes(move.id)) {
			targets.forEach((x, i) => {
				if (!x) return;
				if (x.forceSwitchFlag && !before[i]) {
					c.forceFlagSet[move.id]++;
					forcedBy.set(x, { move: move.id, source, turn: this.battle.turn });
				} else if (x.hp === 0 && DAMAGING_FORCERS.includes(move.id)) c.forceTargetKnockedOut[move.id]++;
			});
		}
		return r;
	};
	// the move's runMoveEffects: no switch target (Roar and Whirlwind fail, Dragon Tail and Circle Throw only damage)
	const runMoveEffects2 = BattleActions.prototype.runMoveEffects;
	BattleActions.prototype.runMoveEffects = function (damage, targets, source, move, moveData, isSecondary, isSelf) {
		const c = t();
		const watched = c && move.forceSwitch && moveData === move && !isSecondary && !isSelf && FORCERS.includes(move.id) && targets[0];
		const canSwitch = watched ? !!this.battle.canSwitch(targets[0].side) : true;
		const r = runMoveEffects2.call(this, damage, targets, source, move, moveData, isSecondary, isSelf);
		if (watched && !canSwitch) {
			if (STATUS_FORCERS.includes(move.id) && r[0] === false) c.forceNoTargetStatusFailed[move.id]++;
			if (DAMAGING_FORCERS.includes(move.id) && typeof r[0] === 'number') c.forceNoTargetDamaged[move.id]++;
		}
		return r;
	};
	// the drag
	const dragIn = BattleActions.prototype.dragIn;
	BattleActions.prototype.dragIn = function (side, pos) {
		const c = t();
		if (!c) return dragIn.call(this, side, pos);
		const battle = this.battle, old = side.active[pos];
		const by = old && forcedBy.get(old);
		const candidates = battle.possibleSwitches(side);
		const info = { move: by && by.move, turn: battle.turn };
		const turn = battle.turn;
		const ended = battle.ended;
		const oldTera = old.terastallized;
		const oldPivotIn = pivotIn.get(old);
		state.dragCtx = info;
		const r = dragIn.call(this, side, pos);
		const dragged = side.active[pos] !== old;
		if (dragged && info.move) {
			c.dragged[info.move]++;
			if (candidates.length === 1) c.dragOneCandidate++; else c.dragSeveralCandidates++;
			if (side.pokemon.slice(side.active.length).some(p => p.fainted)) c.dragBenchHasFainted++;
			if (by.source.hp === 0) c.dragAfterUserFainted++;
			if (ended) c.dragAfterBattleEnded++;
			if (battle.__pivotMove && battle.__pivotMove.turn === turn) c.dragSameTurnAsPivot++;
			if (oldPivotIn && oldPivotIn.turn === turn) c.dragOutPivotReplacement++;
			if (teraTurn.get(by.source) === turn) c.dragTeraForcerSameTurn++;
			if (oldTera) { c.dragTeraTarget++; if (teraTurn.get(old) === turn) c.dragTeraTargetSameTurn++; }
		}
		return r;
	};
	// the dragged-in Pokemon's runSwitch: hazards, abilities, seeds, Booster Energy, Unburden
	const runSwitch2 = BattleActions.prototype.runSwitch;
	BattleActions.prototype.runSwitch = function (pokemon) {
		const c = t();
		const info = c && dragCtx.get(pokemon);
		if (!info || !info.move) return runSwitch2.call(this, pokemon);
		dragCtx.delete(pokemon);
		const conds = pokemon.side.sideConditions;
		const had = Object.fromEntries(HAZARDS.map(h => [h, h in conds]));
		const types = pokemon.types.slice();
		const weather = this.battle.field.weather, terrain = this.battle.field.terrain;
		const item = pokemon.item;
		state.dragRunSwitch = true;
		const r = runSwitch2.call(this, pokemon);
		state.dragRunSwitch = false;
		for (const h of HAZARDS) if (had[h]) c.dragIntoHazard[`${info.move}.${h}`]++;
		if (had.toxicspikes && types.includes('Poison') && !types.includes('Flying') && !('toxicspikes' in pokemon.side.sideConditions)) {
			c.dragAbsorbsToxicSpikes[info.move]++;
		}
		if (pokemon.hp === 0 && (had.stealthrock || had.spikes || had.toxicspikes)) {
			c.dragInFaintedToHazard++;
			const foe = pokemon.side.foe.active[0];
			if (foe && foe.hp > 0 && foe.speed === pokemon.speed) c.dragInFaintedSpeedTie++;
		}
		if (item === 'heavydutyboots' && Object.values(had).some(Boolean)) c.dragInWithBoots++;
		if (this.battle.field.weather !== weather) c.dragInSetWeather++;
		if (this.battle.field.terrain !== terrain) c.dragInSetTerrain++;
		if (item && !pokemon.item && item.endsWith('seed')) c.dragInSeedUsed++;
		return r;
	};

	// the requests
	const makeRequest = Battle.prototype.makeRequest;
	Battle.prototype.makeRequest = function (type) {
		const c = t();
		if (c && type === 'switch') {
			const flags = this.sides.map(s => s.active.map(p => p && p.switchFlag));
			const pivots = flags.map(f => f.some(x => typeof x === 'string'));
			if (pivots.some(Boolean)) {
				c.midTurnSwitchRequest++;
				if (pivots.every(Boolean)) c.midTurnRequestBothSides++;
				this.__pivotRequestTurn = this.turn;
			} else if (flags.some(f => f.some(x => x === true))) {
				if (this.__pivotRequestTurn === this.turn) c.pivotThenFaintReplacementSameTurn++;
				if (this.__pivotKo && this.__pivotKo.turn === this.turn) c.pivotKoThenEndOfTurnReplacement++;
			}
		}
		return makeRequest.call(this, type);
	};
}

// Records a scenario; a scripted opening that Showdown rejects is dropped (the battle is recorded with the random policy alone), with
// the tally of the failed attempt thrown away.
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
		console.error('usage: node gen-slice6.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice6-${k}.json`), JSON.stringify(pool[k]));
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
		if (result.dropped) { dropped++; if (process.env.SLICE6_VERBOSE) console.error(`seed ${seed}: script dropped: ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice6-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice6 seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error('tally (from Showdown\'s own events):');
	let zeros = [];
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

module.exports = { assign };
