#!/usr/bin/env node
'use strict';
// Generates battles for "slice 7c" (Encore and Taunt) and records each on Showdown.
//
// Encore (`volatileStatus: 'encore'`): `onStart` fails without `lastMove`, for a `failencore` move (Struggle, Encore itself), with no slot
// or no PP, and adds a turn when the target has no move queued; `onOverrideAction` swaps in the encored move at the head of `runMove`;
// `onResidual` (order 16) ends it at 0 PP; `onDisableMove` disables the other moves. Taunt: one more turn when `activeTurns` and no move is
// queued; `onBeforeMove` (priority 5) blocks Status moves, `onDisableMove` disables them (all disabled: Struggle).
//
// The teams are slice 5's (as gen-slice6.js), with Encore and Taunt sprinkled on the sets and every status move in play. About a third
// of the battles are plain random play; the rest are scripted openings (a pair of choices per decision point; the random policy takes
// over when the script ends, and a script Showdown rejects is dropped and the battle recorded without it):
//   fast    Encore before the target moves, over a chosen move: the override; then Taunt, a switch while encored, a move at 0 PP
//   slow    Encore after the target moved (+1 turn); Taunt after it moved (+1 turn); Encore on a status move, then Taunt: all disabled
//   none    Encore or Taunt on turn 1 (no lastMove; activeTurns 0), whichever side is faster
//   pp      the target drains its low-PP move; Encore with 1 or 2 PP left, or with 0 left (fails)
//   switch  Encore or Taunt, the target switches out and back (volatile and lastMove gone), then the same again
//   uturn   a U-turn user is encored: a lone one stays and is encored into U-turn, one with a bench is replaced (lastMove null)
//   status  a status move is encored, then Taunt on top: the move is blocked and the next turn is Struggle; Encore again fails
//   taunt   Taunt before or after a status move, on turn 1 and on later turns, on a fresh switch-in
//   mirror  both sides have Encore: Encore on Encore (failencore), Encore overridden by Encore
//   tera    the target or the user terastallizes on the turn of the move
//
// Usage: node gen-slice7c.js <outDir> <count> <poolFile> [firstSeed=1]
// Output: <outDir>/slice7c-<seed>.json and slice7c-{moves,species,abilities,items}.json (copies of the pool).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
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
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];

const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 1597334677 + 3812015801);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	for (const id of ['encore', 'taunt']) if (!has(id)) throw new Error(`the pool has no ${id}`);
	const short = (sc) => sc.teams[0].length !== 6 || sc.teams[1].length !== 6;
	let scenario = assign5(scenario0, pool, seed);
	for (let k = 1; short(scenario) && k < 30; k++) {
		const s2 = seed + 1000003 * k;
		scenario = assign5(makeScenario(s2, 'gen9ou', pool), pool, s2);
	}
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const mv = (id) => dex.moves.get(id);
	const speciesName = (id) => dex.species.get(id).name;
	const real = pool.species.filter(id => id !== 'shedinja');
	const inert = (id) => !PIVOTS.includes(id) && !FORCERS.includes(id) && id !== 'encore' && id !== 'taunt';
	const statuses = pool.moves.filter(id => mv(id).category === 'Status' && inert(id));
	// status moves that harm nobody (a level-5 target must survive a long script)
	const quiet = statuses.filter(id => ['self', 'allySide'].includes(mv(id).target) && !mv(id).weather && !mv(id).sideCondition);
	const attacks = pool.moves.filter(id => {
		const m = mv(id);
		return m.category !== 'Status' && m.basePower >= 40 && (m.accuracy === true || m.accuracy >= 90) && m.priority === 0 && inert(id) &&
			!m.flags.charge && !m.flags.recharge && !m.selfdestruct && !m.mindBlownRecoil && !m.multihit && !m.selfSwitch && !m.forceSwitch;
	});
	const lowPp = attacks.filter(id => mv(id).pp === 5 && !mv(id).noPPBoosts);
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves[int(4)] = id;
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const set = (s, ...ids) => {
		s.moves = [...new Set(ids)];
		while (s.moves.length < 4) { const m = pick(inertPool); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const inertPool = pool.moves.filter(inert);
	const steady = (s, level, spe) => {
		s.level = level; s.item = ''; s.ability = 'No Ability'; s.nature = 'Hardy';
		s.evs = { hp: 0, atk: 0, def: 0, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
	};
	// sprinkle (every set): Encore, Taunt, status moves, a pivot
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 38) put(s, 'encore');
		if (int(100) < 38) put(s, 'taunt');
		if (int(100) < 30 && statuses.length) put(s, pick(statuses));
		if (int(100) < 18) put(s, pick(PIVOTS.filter(has)));
	}
	const kinds = [['random', 30], ['fast', 8], ['slow', 8], ['none', 6], ['pp', 12], ['switch', 8], ['uturn', 5], ['status', 8], ['taunt', 8], ['mirror', 4], ['tera', 3]];
	let kind = 'random', acc = 0;
	const roll = int(100);
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	scenario.kind = undefined;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const duo = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const teams = (an, bn) => { const pair = [null, null]; pair[aIdx] = `team ${[1, 2, 3, 4, 5, 6].slice(0, an).join('')}`; pair[bIdx] = `team ${[1, 2, 3, 4, 5, 6].slice(0, bn).join('')}`; return pair; };
	const full = () => teams(6, 6);
	// A (Encore, Taunt, two quiet moves) against B, A faster or slower
	const pair = (aFaster, levelA, levelB) => {
		for (let k = 0; k < 400; k++) {
			const a = pick(real);
			const sa = speStat(a, levelA, aFaster ? 252 : 0);
			const bs = real.filter(b => (aFaster ? sa > speStat(b, levelB, 0) + 10 : speStat(b, levelB, 0) > sa + 10));
			if (bs.length) { A.species = speciesName(a); B.species = speciesName(pick(bs)); return; }
		}
		throw new Error('no speed pair');
	};
	const idle = () => [pick(quiet.length ? quiet : statuses), pick(quiet.length ? quiet : statuses)];
	const setupA = (aFaster, levelB) => { steady(A, 100, aFaster ? 252 : 0); steady(B, levelB, 0); pair(aFaster, 100, levelB); const [i1, i2] = idle(); set(A, 'encore', 'taunt', i1, i2); };
	const bMoves = (...first) => set(B, ...first);
	const teraMaybe = (m) => (int(100) < 30 ? `${m} terastallize` : m);
	const aFast = int(2) === 0;
	const [atkA, atkB] = [pick(attacks), pick(attacks)];
	const [stA, stB] = [pick(statuses), pick(statuses)];

	if (kind === 'fast') {
		setupA(true, 50); bMoves(atkA, atkB, stA, stB);
		const variant = int(3);
		scenario.choices = [full(), duo('move 3', 'move 1'), duo('move 1', 'move 2'), duo('move 2', variant === 0 ? 'switch 2' : 'move 1'),
			duo('move 3', variant === 0 ? 'move 1' : 'move 1')];
	} else if (kind === 'slow') {
		setupA(false, 50); bMoves(atkA, atkB, stA, stB);
		scenario.choices = [full(), duo('move 3', 'move 3'), duo('move 1', 'move 4'), duo('move 3', 'move 4'), duo('move 2', 'move 4'), duo('move 3', 'move 1')];
	} else if (kind === 'none') {
		setupA(aFast, 50); bMoves(atkA, atkB, stA, stB);
		scenario.choices = [full(), duo(pick(['move 1', 'move 2']), 'move 1'), duo(pick(['move 1', 'move 2']), 'move 1'), duo('move 3', 'move 1')];
	} else if (kind === 'pp') {
		if (!lowPp.length) return scenario;
		const variant = int(3); // 0: A fast, 1 PP left; 1: A slow, 2 PP left; 2: A slow, 1 PP left (the last PP is used first: Encore fails)
		const fast = variant === 0;
		setupA(fast, fast ? 5 : 20);
		bMoves(pick(lowPp), atkB, stA, stB);
		const left = variant === 1 ? 2 : 1;
		const maxpp = Math.floor(mv(B.moves[0]).pp * 8 / 5);
		const steps = [full()];
		for (let i = 0; i < maxpp - left; i++) steps.push(duo('move 3', 'move 1'));
		steps.push(duo('move 1', 'move 1'));
		if (variant === 1) steps.push(duo('move 3', 'move 1'));
		scenario.choices = steps;
	} else if (kind === 'switch') {
		setupA(true, 50); bMoves(atkA, atkB, stA, stB);
		const which = int(2) === 0 ? 'move 1' : 'move 2';
		scenario.choices = [full(), duo('move 3', 'move 1'), duo(which, 'move 2'), duo('move 3', 'switch 2'), duo(which, 'switch 2'), duo('move 3', 'move 1')];
	} else if (kind === 'uturn') {
		const solo = int(2) === 0;
		setupA(aFast, 50); bMoves('uturn', atkB, stA, stB);
		if (solo) {
			scenario.teams[bIdx] = scenario.teams[bIdx].slice(0, 1);
			scenario.choices = [teams(6, 1), duo('move 3', 'move 1'), duo('move 1', 'move 1'), duo('move 3', 'move 1')];
		} else {
			scenario.choices = [full(), duo('move 3', 'move 1'), duo(null, 'switch 2'), duo(pick(['move 1', 'move 2']), 'move 1')];
		}
	} else if (kind === 'status') {
		setupA(true, 50); bMoves(stA, stB, atkA, atkB);
		scenario.choices = [full(), duo('move 3', 'move 1'), duo('move 1', 'move 2'), duo('move 2', 'move 1'), duo('move 1', 'move 1')];
	} else if (kind === 'taunt') {
		const variant = int(4);
		setupA(variant === 0, 50); bMoves(stA, stB, atkA, atkB);
		if (variant === 0) scenario.choices = [full(), duo('move 3', 'move 3'), duo('move 2', 'move 3'), duo('move 3', 'move 1'), duo('move 3', 'move 1')];
		else if (variant === 1) scenario.choices = [full(), duo('move 3', 'move 3'), duo('move 2', 'move 4'), duo('move 3', 'move 3'), duo('move 3', 'move 3'), duo('move 3', 'move 3')];
		else if (variant === 2) scenario.choices = [full(), duo('move 2', 'move 3'), duo('move 3', 'move 4'), duo('move 3', 'move 1')];
		else scenario.choices = [full(), duo('move 3', 'switch 2'), duo('move 2', 'move 3'), duo('move 3', 'move 1')];
	} else if (kind === 'mirror') {
		steady(A, 100, 252); steady(B, 100, 0); pair(true, 100, 100);
		set(A, 'encore', 'taunt', atkA, atkB); set(B, 'encore', 'taunt', atkA, atkB);
		scenario.choices = int(2) === 0
			? [full(), duo('move 3', 'move 3'), duo('move 1', 'move 1'), duo('move 1', 'move 3'), duo('move 2', 'move 3')]
			: [full(), duo('move 1', 'move 1'), duo('move 1', 'move 3'), duo('move 3', 'move 3')];
	} else if (kind === 'tera') {
		setupA(aFast, 50); bMoves(atkA, atkB, stA, stB);
		scenario.choices = [full(), duo('move 3', teraMaybe('move 1')), duo(teraMaybe(pick(['move 1', 'move 2'])), 'move 2'), duo('move 3', 'move 1')];
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally, from Showdown's own methods (observation only)

let tally = null;
function newTally() {
	return {
		encore: { started: 0, durationExtra: 0, noLastMove: 0, failencoreMove: 0, noPP: 0, already: 0, otherFail: 0, lastMoveWasEncore: 0, lastMoveStruggle: 0 },
		override: { changedMove: 0, sameMove: 0, targetWasQueued: 0 },
		encoreEnd: { noPP: 0, duration: 0 },
		taunt: { started: 0, durationExtra: 0, turn1NoExtra: 0, alreadyTaunted: 0, blockedStatusMove: 0 },
		struggleAllDisabled: { encoreTaunt: 0, encore: 0, taunt: 0 },
		switchedOutWhile: { encored: 0, taunted: 0 },
	};
}
const STRUCTURAL_ZERO = ['encore.otherFail', 'override.targetWasQueued', 'struggleAllDisabled.encore'];

function install() {
	const t = () => tally;
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, source, sourceEffect, linkedStatus) {
		const c = t();
		const id = status && (status.id || status);
		if (!c || (id !== 'encore' && id !== 'taunt')) return addVolatile.call(this, status, source, sourceEffect, linkedStatus);
		const last = this.lastMove, had = !!this.volatiles[id], activeTurns = this.activeTurns;
		const queued = !!this.battle.queue.willMove(this);
		const slot = last && this.getMoveData(last.id);
		const r = addVolatile.call(this, status, source, sourceEffect, linkedStatus);
		if (id === 'encore') {
			if (r) {
				c.encore.started++;
				if (this.volatiles.encore.duration === 4) c.encore.durationExtra++;
				if (!queued && this.volatiles.encore.duration !== 4) c.encore.otherFail++;
			} else if (had) c.encore.already++;
			else if (!last) c.encore.noLastMove++;
			else if (last.id === 'struggle') c.encore.lastMoveStruggle++;
			else if (last.flags && last.flags.failencore) { c.encore.failencoreMove++; if (last.id === 'encore') c.encore.lastMoveWasEncore++; }
			else if (!slot || slot.pp <= 0) c.encore.noPP++;
			else c.encore.otherFail++;
		} else if (r) {
			c.taunt.started++;
			if (this.volatiles.taunt.duration === 4) c.taunt.durationExtra++;
			else if (activeTurns === 0) c.taunt.turn1NoExtra++;
		} else if (had) c.taunt.alreadyTaunted++;
		return r;
	};
	const removeVolatile = Pokemon.prototype.removeVolatile;
	Pokemon.prototype.removeVolatile = function (status) {
		const c = t();
		const id = status && (status.id || status);
		if (c && id === 'encore' && this.volatiles.encore && this.hp) {
			const slot = this.getMoveData(this.volatiles.encore.move);
			if (this.volatiles.encore.duration > 1 || !slot || slot.pp <= 0) {
				if (!slot || slot.pp <= 0) c.encoreEnd.noPP++; else c.encoreEnd.duration++;
			} else c.encoreEnd.duration++;
		}
		return removeVolatile.call(this, status);
	};
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, ...rest) {
		const r = runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
		const c = t();
		if (c && eventid === 'OverrideAction') {
			if (typeof r === 'string') c.override.changedMove++; else if (target.volatiles.encore) c.override.sameMove++;
		}
		return r;
	};
	const add = Battle.prototype.add;
	Battle.prototype.add = function (...args) {
		const c = t();
		if (c && args[0] === 'cant' && args[2] === 'move: Taunt') c.taunt.blockedStatusMove++;
		return add.call(this, ...args);
	};
	const getMoveRequestData = Pokemon.prototype.getMoveRequestData;
	Pokemon.prototype.getMoveRequestData = function () {
		const r = getMoveRequestData.call(this);
		const c = t();
		if (c && r.moves.length === 1 && r.moves[0].id === 'struggle') {
			if (this.volatiles.encore && this.volatiles.taunt) c.struggleAllDisabled.encoreTaunt++;
			else if (this.volatiles.encore) c.struggleAllDisabled.encore++;
			else if (this.volatiles.taunt) c.struggleAllDisabled.taunt++;
		}
		return r;
	};
	const clearVolatile = Pokemon.prototype.clearVolatile;
	Pokemon.prototype.clearVolatile = function (...a) {
		const c = t();
		if (c && this.isActive && this.hp) {
			if (this.volatiles.encore) c.switchedOutWhile.encored++;
			if (this.volatiles.taunt) c.switchedOutWhile.taunted++;
		}
		return clearVolatile.call(this, ...a);
	};
}

function recordScenario(scenario) {
	const before = JSON.stringify(tally);
	try {
		return { trace: record(scenario), scripted: !!scenario.choices };
	} catch (e) {
		if (!scenario.choices) throw e;
		tally = JSON.parse(before);
		const plain = Object.assign({}, scenario);
		delete plain.choices;
		return { trace: record(plain), scripted: false, dropped: e.message };
	}
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice7c.js <outDir> <count> <poolFile> [firstSeed=1]');
		process.exit(2);
	}
	const count = parseInt(countArg, 10), first = parseInt(firstArg || '1', 10);
	const pool = JSON.parse(fs.readFileSync(poolFile, 'utf8'));
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		if (!Array.isArray(pool[k])) throw new Error(`bad pool file ${poolFile}: no ${k}`);
	}
	fs.mkdirSync(outDir, { recursive: true });
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		fs.writeFileSync(path.join(outDir, `slice7c-${k}.json`), JSON.stringify(pool[k]));
	}
	tally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scripted = 0, dropped = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scripted++;
		if (result.dropped) { dropped++; if (process.env.SLICE7C_VERBOSE) console.error(`seed ${seed}: script dropped: ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice7c-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice7c seeds, ${written} traces written (${scripted} scripted, ${dropped} scripts dropped), ${truncated} truncated, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${turns} turns, ${draws} draws`);
	const zeros = [];
	const walk = (prefix, v) => {
		if (typeof v === 'number') { if (v === 0 && !STRUCTURAL_ZERO.includes(prefix)) zeros.push(prefix); return; }
		for (const [k, x] of Object.entries(v)) walk(prefix ? `${prefix}.${k}` : k, x);
	};
	for (const [k, v] of Object.entries(tally)) { console.error(`  ${k}: ${JSON.stringify(v)}`); walk(k, v); }
	console.error(zeros.length ? `ZERO COUNTERS: ${zeros.join(', ')}` : 'no counter is zero except the structural ones');
}

module.exports = { assign };
