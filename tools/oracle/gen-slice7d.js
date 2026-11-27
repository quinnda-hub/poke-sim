#!/usr/bin/env node
'use strict';
// Generates battles for "slice 7d" (Sucker Punch and Thunderclap) and records each on Showdown.
//
// Both moves have priority 1 and an `onTry` (data/moves.ts 18412, 19513) that reads `this.queue.willMove(target)`: the move fails (before
// the accuracy roll, so with no draw) unless the target still has a queued move action whose move is not a Status move (Me First aside),
// and the target has no `mustrecharge`. The teams are slice 6's (gen-slice6.js `assign`, so every earlier mechanic is still in the pool),
// then every set may get one of the two moves from a stream of its own. Every scenario is recorded with `tera: true`. About half of the
// battles are the plain random policy over those teams; the rest are scripted openings (a pair of choices per decision point; the random
// policy takes over when the script ends, and a script that Showdown rejects is dropped) that put each case in front of the engine:
//   status    the user against a target that picks a status move (Sucker Punch fails), or an attack (it works)
//   fast      the target is faster than the user and attacks: the target has moved, no queued move, the move fails
//   slow      the target is slower and attacks: the move works
//   switch    the target switches out (a switch action is not a move action: it fails)
//   pivot     the target is faster and uses U-turn or Volt Switch (its move is queued, the switch happens before the user acts, the move
//             then hits the replacement or fails)
//   mirror    both leads have one of the two moves (priority 1 against priority 1: the speed tie draws)
//   tera      the user, the target, or both terastallize on the turn
//   prio      the target uses a priority move of its own (it moves first: the user's move fails)
//   ko        the target faints before the user acts, to a faster priority move
//
// Usage:
//   node gen-slice7d.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice7d-<seed>.json, plus slice7d-moves.json, slice7d-species.json, slice7d-abilities.json and slice7d-items.json
// (copies of the pool, which the parity test compares with what the engine accepts). A tally of the outcomes of the two `onTry`s is
// printed to stderr; it is computed by wrapping Showdown's `singleEvent` (observation only: the wrapper reads `queue.willMove`, which has
// no side effect, and passes every argument and result through).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
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

const PUNCHES = ['suckerpunch', 'thunderclap'];
const PIVOTS = ['uturn', 'voltswitch', 'flipturn'];
const STATUS_MOVES = ['recover', 'toxic', 'swordsdance', 'calmmind', 'willowisp', 'thunderwave', 'roost', 'stealthrock', 'spikes', 'protect'];

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2654435761 + 40503);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	for (const id of PUNCHES) if (!has(id)) throw new Error(`the pool has no ${id}`);
	const scenario = assign6(scenario0, pool, seed);
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const speciesName = (id) => dex.species.get(id).name;
	const spe = (id) => dex.species.get(id).baseStats.spe;
	const eligible = pool.species.filter(id => id !== 'shedinja').sort((x, y) => spe(x) - spe(y));
	const third = Math.max(1, Math.floor(eligible.length / 3));
	const slowOnes = eligible.slice(0, third), fastOnes = eligible.slice(-third);
	const put = (s, id, slot) => {
		if (s.moves.includes(id)) s.moves.splice(s.moves.indexOf(id), 1);
		s.moves.splice(slot === undefined ? int(4) : slot, 0, id);
		s.moves = [...new Set(s.moves)].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const attacks = pool.moves.filter(id => { const m = dex.moves.get(id); return m.category !== 'Status' && m.basePower >= 40 && m.accuracy === 100 && !PUNCHES.includes(id) && !m.selfSwitch && !m.forceSwitch && m.priority <= 0 && !m.self && !m.mindBlownRecoil; });
	const statuses = STATUS_MOVES.filter(has);
	const priority = pool.moves.filter(id => { const m = dex.moves.get(id); return m.category !== 'Status' && m.priority > 0 && !PUNCHES.includes(id) && m.basePower >= 40; });
	const pivots = PIVOTS.filter(has);

	// every set may carry one of the moves (a stream of this generator, so the base teams stay slice 6's)
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 40) put(s, pick(PUNCHES));
		if (int(100) < 5) put(s, pick(PUNCHES));
	}
	const kinds = [['random', 40], ['status', 12], ['fast', 8], ['slow', 6], ['switch', 7], ['pivot', 7], ['mirror', 6], ['tera', 6], ['prio', 5], ['ko', 3]];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	if (kind === 'random') return scenario;
	const TEAM = ['team 123456', 'team 123456'];
	if (scenario.teams[0].length !== 6 || scenario.teams[1].length !== 6) return scenario;
	const A = scenario.teams[0][0], B = scenario.teams[1][0];
	for (const s of [A, B]) {
		s.level = 100;
		s.ability = 'No Ability';
		if (s.item === 'Assault Vest' || s.item === 'Heavy-Duty Boots') s.item = '';
		if (s.species === 'Shedinja') s.species = speciesName(pick(eligible));
	}
	const movesOf = (s, ...ids) => { s.moves = [...new Set([...ids, pick(statuses), 'recover'].filter(has))].slice(0, 4); };
	const punch = pick(PUNCHES);
	const teraMaybe = (mv) => (int(100) < 50 ? `${mv} terastallize` : mv);
	const faster = (s, fast) => { s.species = speciesName(pick(fast ? fastOnes : slowOnes)); };
	if (kind === 'status') {
		// the target picks a status move (the user's move fails), or an attack (it works); repeated, as the policy takes over after
		faster(A, int(2) === 0); faster(B, int(2) === 0);
		movesOf(A, punch); movesOf(B, pick(attacks), ...[pick(statuses)]);
		B.moves = [...new Set([pick(attacks), pick(statuses), pick(statuses), pick(attacks)])].slice(0, 4);
		scenario.choices = [TEAM, ['move 1', `move ${1 + int(B.moves.length)}`], ['move 1', `move ${1 + int(B.moves.length)}`]];
	} else if (kind === 'fast') {
		faster(A, false); faster(B, true);
		movesOf(A, punch); movesOf(B, pick(attacks));
		B.moves = [pick(attacks), ...B.moves.filter(m => !attacks.includes(m))].slice(0, 4);
		scenario.choices = [TEAM, ['move 1', 'move 1']];
	} else if (kind === 'slow') {
		faster(A, true); faster(B, false);
		movesOf(A, punch); B.moves = [pick(attacks), pick(attacks), pick(statuses), 'recover'].filter((x, i, xs) => xs.indexOf(x) === i).slice(0, 4);
		scenario.choices = [TEAM, ['move 1', 'move 1'], ['move 1', 'move 2']];
	} else if (kind === 'switch') {
		faster(A, int(2) === 0); faster(B, int(2) === 0);
		movesOf(A, punch); movesOf(B, pick(attacks));
		scenario.choices = [TEAM, ['move 1', 'switch 2'], ['move 1', 'move 1'], ['move 1', 'switch 3']];
	} else if (kind === 'pivot') {
		faster(A, false); faster(B, true);
		movesOf(A, punch); B.moves = [pick(pivots), pick(attacks), pick(statuses), 'recover'].slice(0, 4);
		scenario.choices = [TEAM, ['move 1', 'move 1'], ['move 1', 'move 1']];
	} else if (kind === 'mirror') {
		movesOf(A, punch); movesOf(B, int(2) ? punch : pick(PUNCHES));
		if (int(3) === 0) { B.species = A.species; }
		scenario.choices = [TEAM, ['move 1', 'move 1'], ['move 1', 'move 1']];
	} else if (kind === 'tera') {
		faster(A, int(2) === 0); faster(B, int(2) === 0);
		movesOf(A, punch); movesOf(B, pick(attacks));
		const v = int(3);
		scenario.choices = [TEAM, [v !== 1 ? 'move 1 terastallize' : 'move 1', v !== 0 ? 'move 1 terastallize' : 'move 1'], ['move 1', 'move 2']];
	} else if (kind === 'prio') {
		faster(A, true); faster(B, false);
		movesOf(A, punch);
		B.moves = [pick(priority.length ? priority : attacks), pick(statuses), 'recover'].filter((x, i, xs) => xs.indexOf(x) === i);
		scenario.choices = [TEAM, ['move 1', teraMaybe('move 1')], ['move 1', 'move 1']];
	} else if (kind === 'ko') {
		// the target is low-level, so a faster priority move or a Sucker Punch ahead of it takes its HP; the user's second action
		// finds a fainted target
		faster(A, true); faster(B, false);
		B.level = 5; A.level = 100;
		movesOf(A, punch); B.moves = [pick(attacks), 'recover'];
		scenario.choices = [TEAM, ['move 1', 'move 1']];
	}
	// no duplicate moves (a team Showdown or the engine rejects), padded from the pool
	for (const s of [A, B]) {
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 3) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	}
	return scenario;
}

// ---- the tally, from Showdown's own onTry calls
function newTally() {
	return {
		onTry: { works: 0, noMoveAction: 0, statusMove: 0 },
		targetSpeed: { faster: 0, notFaster: 0 },
		targetMoveClass: { damaging: 0, struggle: 0 },
		noActionFullHp: 0,
		byMove: { suckerpunch: 0, thunderclap: 0 },
	};
}
let activeTally = null;
function install() {
	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, source, sourceEffect, ...rest) {
		const c = activeTally;
		// moveTry: singleEvent('Try', move, null, pokemon, targets[0], move): the event's target is the user, its source the move's target
		if (c && eventid === 'Try' && effect && PUNCHES.includes(effect.id) && source && target) {
			const foe = source, user = target;
			const action = this.queue.willMove(foe);
			c.byMove[effect.id]++;
			if (!action) c.onTry.noMoveAction++;
			else if (action.move.category === 'Status') c.onTry.statusMove++;
			else {
				c.onTry.works++;
				if (action.move.id === 'struggle') c.targetMoveClass.struggle++; else c.targetMoveClass.damaging++;
			}
			if (foe.speed > user.speed) c.targetSpeed.faster++; else c.targetSpeed.notFaster++;
			if (!action && foe.hp === foe.maxhp) c.noActionFullHp++;
		}
		return singleEvent.call(this, eventid, effect, state, target, source, sourceEffect, ...rest);
	};
}
// counters that cannot be hit by construction or are rare by nature
const STRUCTURAL_ZERO = ['targetMoveClass.struggle'];

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
		console.error('usage: node gen-slice7d.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice7d-${k}.json`), JSON.stringify(pool[k]));
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
		if (result.dropped) { dropped++; if (process.env.SLICE7D_VERBOSE) console.error(`seed ${seed}: script dropped: ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice7d-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice7d seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
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

module.exports = { assign };
