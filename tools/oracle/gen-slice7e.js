#!/usr/bin/env node
'use strict';
// Generates battles for "slice 7e" (the Choice items and the `choicelock` volatile) and records each on Showdown.
//
// Slice 7e lets in Choice Band, Choice Specs and Choice Scarf. Their `onModifyMove` adds `choicelock` (data/items.ts 971-1040), whose
// `onStart` records the move in use; `onDisableMove` (run at endTurn only) disables every other slot, and `onBeforeMove` and
// `onDisableMove` drop the lock once the holder has no Choice item. The item's own `onStart` removes a lock (so Trick, Switcheroo and
// Thief hand a lock-free item to the receiver). Knock Off, Trick, Switcheroo and Thief were accepted in slice 4d without Choice items in
// the pool; this slice records them against Choice holders.
//
// The teams are slice 6's (gen-slice6.js `assign`, which gives every Pokemon a pivot, a forcing move, hazards and the 6c/6d items), with
// a share of the sets given a Choice item and the 4d item moves and pivots. The scripted openings of slice 6 are dropped: every seed is
// either a plain random battle over those teams (about a third) or one of these scripted openings (a pair of choices per decision
// point; the random policy takes over when the script ends, and a script that Showdown rejects is dropped and the battle recorded
// without it). Tera is offered in all of them (`tera: true`, a `teraChance` of 0.1-0.5).
//   lock      a Choice holder uses a move twice: the lock after the first use, the request that offers only that move
//   trick     a Choice holder uses Trick or Switcheroo against a holder of any item (a Choice item half the time): the user's own lock
//             (added in ModifyMove, before the swap) goes with the item, or is removed by the arriving Choice item's `onStart`
//   knock     Knock Off into a Choice holder that has just moved (its lock stays, to be dropped when the DisableMove event runs)
//   thief     Thief, with no item, into a Choice holder: the thief holds it, unlocked, and may use any move at once
//   scarf     mirrored Choice Scarf holders (equal speed: a speed tie draw), or a Scarf holder against a Trick user that takes it
//   pivot     a Choice holder with U-turn or Volt Switch pivots out (the lock goes with `clearVolatile`) and may come back unlocked
//   pp        a Choice holder locked into a low-PP move against a foe immune to it, until the PP runs out: Struggle, with its recoil
//   tera      a locked Choice holder terastallizes (the request still offers the Tera variant of its one move)
//   pair      both leads Trick or Switcheroo with a Choice item
//
// Usage:
//   node gen-slice7e.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice7e-<seed>.json, plus slice7e-moves.json, slice7e-species.json, slice7e-abilities.json and slice7e-items.json
// (copies of the pool, which the parity test compares with what the engine accepts). A tally of the outcomes that distinguish each part
// of the mechanic is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers pass every
// argument and result through and call nothing the battle would not call).

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

const CHOICE = ['choiceband', 'choicespecs', 'choicescarf'];
const SWAPPERS = ['trick', 'switcheroo'];
const PIVOTS = ['uturn', 'voltswitch'];
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const isChoice = (item) => CHOICE.includes(item);

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2654435761 + 40503);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	for (const id of [...CHOICE, 'leftovers', 'lifeorb']) if (!pool.items.includes(id)) throw new Error(`the pool has no item ${id}`);
	for (const id of [...SWAPPERS, ...PIVOTS, 'knockoff', 'thief', 'recover', 'swordsdance', 'toxic']) if (!has(id)) throw new Error(`the pool has no ${id}`);

	const scenario = assign6(scenario0, pool, seed);
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const speciesName = (id) => dex.species.get(id).name;

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
	const moves = (s, ...ids) => {
		s.moves = [...new Set([...ids, 'recover', 'toxic', 'swordsdance'])].slice(0, 4);
	};
	const setItem = (s, id) => { s.item = itemName(id); };
	const steady = (s) => {
		s.level = 100;
		s.ability = 'No Ability';
		if (s.species === 'Shedinja') s.species = speciesName(pick(pool.species.filter(id => id !== 'shedinja')));
	};

	// the sprinkle: Choice items and the item moves on the sets (Trick and Switcheroo users prefer a Choice item)
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 42) setItem(s, pick(CHOICE));
		if (int(100) < 16) put(s, pick(SWAPPERS));
		if (int(100) < 14) put(s, 'knockoff');
		if (int(100) < 10) put(s, 'thief');
		if (int(100) < 18) put(s, pick(PIVOTS));
		if (int(100) < 8 && s.item === '') setItem(s, pick(['leftovers', 'lifeorb']));
		if (s.item !== '' && isChoice(dex.items.get(s.item).id) === false && int(100) < 30 && SWAPPERS.some(m => s.moves.includes(m))) setItem(s, pick(CHOICE));
	}
	const kinds = [
		['random', 30], ['lock', 9], ['trick', 14], ['knock', 9], ['thief', 7], ['scarf', 6], ['pivot', 7], ['pp', 7], ['tera', 5], ['pair', 6],
	];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const full = `team ${scenario.teams[0].map((_, i) => i + 1).join('')}`;
	const fullB = `team ${scenario.teams[1].map((_, i) => i + 1).join('')}`;
	const teamPair = () => [full, fullB];
	const duo = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const types = (s) => dex.species.get(s.species).types;
	const canHit = (mv, s) => dex.getImmunity(dex.moves.get(mv).type, types(s));
	const plain = pool.moves.filter(id => {
		const m = dex.moves.get(id);
		return m.category !== 'Status' && m.basePower >= 40 && !m.selfdestruct && !m.self && !m.selfSwitch && !m.flags.charge && !m.flags.recharge && !m.multihit;
	});
	const strike = (s) => { const ok = plain.filter(id => canHit(id, s)); return pick(ok.length ? ok : plain); };
	steady(A); steady(B);
	const pairs = [teamPair()];
	if (kind === 'lock') {
		setItem(A, pick(CHOICE));
		moves(A, strike(B), strike(B));
		pairs.push(duo('move 1', 'move 1'), duo('move 1', 'move 1'));
	} else if (kind === 'trick') {
		setItem(A, pick(CHOICE));
		const bItem = pick(['', 'leftovers', 'lifeorb', ...CHOICE, ...CHOICE]);
		setItem(B, bItem);
		moves(A, pick(SWAPPERS), strike(B));
		moves(B, strike(A), strike(A));
		// the second turn: 'move 1' is legal whatever the swap did (A is locked into the swapper, or free)
		pairs.push(duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 1', 'move 1'));
	} else if (kind === 'knock') {
		setItem(A, pick(['', 'leftovers', 'lifeorb', ...CHOICE]));
		setItem(B, pick(CHOICE));
		moves(A, 'knockoff', strike(B));
		moves(B, strike(A), strike(A));
		// B (locked into move 1 once it has used it) is knocked out of its lock: the DisableMove event then drops it (the random policy that
		// follows may then pick any move; 'move 1' is legal whether or not the Knock Off landed)
		pairs.push(duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 1', 'move 1'));
	} else if (kind === 'thief') {
		setItem(A, '');
		setItem(B, pick(CHOICE));
		moves(A, 'thief', strike(B));
		moves(B, strike(A), strike(A));
		// the thief picks up an item that is lock-free: its second move is a free choice, which then locks it
		pairs.push(duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 2', 'move 1'));
	} else if (kind === 'scarf') {
		if (int(2) === 0) {
			// mirrored Scarf holders: equal speed, so the move order is a draw
			setItem(A, 'choicescarf');
			scenario.teams[bIdx][0] = JSON.parse(JSON.stringify(A));
			moves(A, strike(A), strike(A));
			scenario.teams[bIdx][0].moves = A.moves.slice();
			pairs.push(duo('move 1', 'move 1'), duo('move 1', 'move 1'));
		} else {
			// a Scarf holder against a Trick user that takes the Scarf (or gives one)
			setItem(B, 'choicescarf');
			setItem(A, pick(['', 'leftovers', ...CHOICE]));
			moves(A, pick(SWAPPERS), strike(B));
			moves(B, strike(A), strike(A));
			pairs.push(duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 1', 'move 1'));
		}
	} else if (kind === 'pivot') {
		setItem(A, pick(CHOICE));
		const pivot = pick(PIVOTS);
		if (!canHit(pivot, B)) B.species = speciesName(pick(pool.species.filter(id => id !== 'shedinja' && !dex.species.get(id).types.includes('Ground'))));
		moves(A, pivot, strike(B));
		moves(B, strike(A), strike(A));
		pairs.push(duo('move 1', 'move 1'), duo('switch 2', null));
		const back = int(2) === 0;
		if (back) pairs.push(duo('move 1', 'move 1'));
	} else if (kind === 'pp') {
		// a low-PP move against a foe that is immune to it
		const low = plain.filter(id => dex.moves.get(id).pp <= 8 && dex.moves.get(id).accuracy !== true);
		const targets = [];
		for (const id of low) for (const sp of pool.species) {
			if (sp !== 'shedinja' && !dex.getImmunity(dex.moves.get(id).type, dex.species.get(sp).types)) targets.push([id, sp]);
		}
		if (targets.length) {
			const [mv, sp] = pick(targets);
			setItem(A, pick(CHOICE));
			setItem(B, '');
			B.species = speciesName(sp);
			B.teraType = B.teraType || 'Normal';
			moves(A, mv, 'recover');
			A.moves = [mv, ...A.moves.filter(m => m !== mv)].slice(0, 4);
			moves(B, 'swordsdance', 'calmmind');
			B.moves = ['swordsdance', 'calmmind', 'recover', 'toxic'];
			const maxpp = Math.floor(dex.moves.get(mv).pp * 8 / 5);
			for (let i = 0; i < maxpp + 2; i++) pairs.push(duo('move 1', 'move 1'));
		}
	} else if (kind === 'tera') {
		setItem(A, pick(CHOICE));
		moves(A, strike(B), strike(B));
		moves(B, strike(A), strike(A));
		pairs.push(duo('move 1', 'move 1'), duo('move 1 terastallize', 'move 1'), duo('move 1', 'move 1'));
	} else if (kind === 'pair') {
		setItem(A, pick(CHOICE));
		setItem(B, pick(CHOICE));
		moves(A, pick(SWAPPERS), strike(B));
		moves(B, pick(SWAPPERS), strike(A));
		pairs.push(duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 2', 'move 2'));
	}
	scenario.choices = pairs;
	return scenario;
}

// ---------------------------------------------------------------------------
// The tally. Every wrapper passes its arguments and result through.

function newTally() {
	return {
		lockAdded: { choiceband: 0, choicespecs: 0, choicescarf: 0 },
		lockRemoved: { itemStart: 0, disableMoveNoChoiceItem: 0, beforeMoveNoChoiceItem: 0, other: 0 },
		lockedRequest: 0,
		lockedRequestNoPP: 0,
		staleLockAtDisableMove: 0,
		struggleWhileLocked: 0,
		struggleChoiceHolder: 0,
		knockOffChoice: 0,
		thiefChoice: 0,
		trick: { userGaveChoice: 0, userGotChoice: 0, choiceForChoice: 0 },
		scarfSpeedTie: 0,
		scarfGivenOrTaken: 0,
		teraLocked: 0,
		pivotLocked: 0,
	};
}
let activeTally = null;
const t = () => activeTally;

function install() {
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, ...rest) {
		const c = t();
		const r = addVolatile.call(this, status, ...rest);
		if (c && r && (status === 'choicelock' || status.id === 'choicelock')) {
			if (c.lockAdded[this.item] !== undefined) c.lockAdded[this.item]++;
		}
		return r;
	};
	const removeVolatile = Pokemon.prototype.removeVolatile;
	Pokemon.prototype.removeVolatile = function (status, ...rest) {
		const c = t();
		const had = c && this.volatiles['choicelock'] && (status === 'choicelock' || status.id === 'choicelock');
		const ev = this.battle.event && this.battle.event.id;
		const choice = isChoice(this.item);
		const r = removeVolatile.call(this, status, ...rest);
		if (had) {
			if (ev === 'Start') c.lockRemoved.itemStart++;
			else if (ev === 'DisableMove' && !choice) c.lockRemoved.disableMoveNoChoiceItem++;
			else if (ev === 'BeforeMove' && !choice) c.lockRemoved.beforeMoveNoChoiceItem++;
			else c.lockRemoved.other++;
		}
		return r;
	};
	// the endTurn DisableMove event: a lock with no Choice item is stale and goes now
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, ...rest) {
		const c = t();
		if (c && eventid === 'DisableMove' && target && target.volatiles && target.volatiles['choicelock'] && !isChoice(target.item)) c.staleLockAtDisableMove++;
		return runEvent.call(this, eventid, target, ...rest);
	};
	// Struggle, by a Choice holder
	const useMove = BattleActions.prototype.useMove;
	BattleActions.prototype.useMove = function (move, pokemon, ...rest) {
		const c = t();
		const id = typeof move === 'string' ? move : move && move.id;
		if (c && id === 'struggle' && pokemon && isChoice(pokemon.item)) {
			c.struggleChoiceHolder++;
			if (pokemon.volatiles['choicelock']) c.struggleWhileLocked++;
		}
		return useMove.call(this, move, pokemon, ...rest);
	};
	// Trick, Switcheroo, Thief and Knock Off against Choice items
	const takeItem = Pokemon.prototype.takeItem;
	Pokemon.prototype.takeItem = function (source) {
		const c = t();
		const mv = this.battle.activeMove;
		const before = this.item;
		const r = takeItem.call(this, source);
		if (c && r && isChoice(before) && mv) {
			if (mv.id === 'knockoff') c.knockOffChoice++;
			if (mv.id === 'thief') c.thiefChoice++;
			if (before === 'choicescarf' && ['knockoff', 'thief', 'trick', 'switcheroo'].includes(mv.id)) c.scarfGivenOrTaken++;
		}
		return r;
	};
	const useMoveBattle = BattleActions.prototype.runMove;
	BattleActions.prototype.runMove = function (moveOrMoveName, pokemon, targetLoc, options) {
		const c = t();
		const id = typeof moveOrMoveName === 'string' ? moveOrMoveName : moveOrMoveName && moveOrMoveName.id;
		if (c && SWAPPERS.includes(id)) {
			const foe = pokemon.side.foe.active[0];
			const mine = pokemon.item, theirs = foe && foe.item;
			const r = useMoveBattle.call(this, moveOrMoveName, pokemon, targetLoc, options);
			if (foe && mine !== theirs) {
				if (isChoice(mine) && foe.item === mine) c.trick.userGaveChoice++;
				if (isChoice(theirs) && pokemon.item === theirs) c.trick.userGotChoice++;
				if (isChoice(mine) && isChoice(theirs) && foe.item === mine) c.trick.choiceForChoice++;
			}
			return r;
		}
		return useMoveBattle.call(this, moveOrMoveName, pokemon, targetLoc, options);
	};
	const terastallize = BattleActions.prototype.terastallize;
	BattleActions.prototype.terastallize = function (pokemon) {
		const c = t();
		if (c && pokemon.volatiles['choicelock'] && isChoice(pokemon.item)) c.teraLocked++;
		return terastallize.call(this, pokemon);
	};
	const makeRequest = Battle.prototype.makeRequest;
	Battle.prototype.makeRequest = function (type) {
		const c = t();
		if (c && type === 'move') {
			const act = this.sides.map(s => s.active[0]);
			for (const p of act) {
				if (!p || !p.hp) continue;
				// requests are made after endTurn's DisableMove: a lock still there is live
				if (p.volatiles['choicelock'] && isChoice(p.item)) {
					c.lockedRequest++;
					const slot = p.moveSlots.find(m => m.id === p.volatiles['choicelock'].move);
					if (slot && slot.pp <= 0) c.lockedRequestNoPP++;
				}
			}
			if (act.every(p => p && p.item === 'choicescarf') && act[0].speed === act[1].speed) c.scarfSpeedTie++;
		}
		if (c && type === 'switch') {
			for (const s of this.sides) {
				const p = s.active[0];
				if (p && typeof p.switchFlag === 'string' && p.volatiles['choicelock']) c.pivotLocked++;
			}
		}
		return makeRequest.call(this, type);
	};
}

// removals through a path other than the three above do not occur (the lock also goes by clearVolatile, which calls no removeVolatile)
const STRUCTURAL_ZERO = ['lockRemoved.other'];

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
		console.error('usage: node gen-slice7e.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice7e-${k}.json`), JSON.stringify(pool[k]));
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
		if (result.dropped) { dropped++; if (process.env.SLICE7E_VERBOSE) console.error(`seed ${seed}: script dropped: ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice7e-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice7e seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
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
