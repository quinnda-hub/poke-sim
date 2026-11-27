#!/usr/bin/env node
'use strict';
// Generates battles for "slice 7a" (roadmap step 4, volatile conditions; phase 7a: the protect family) and records each on Showdown.
//
// Slice 7a lets in Protect, Spiky Shield, Baneful Bunker and Endure. Each is a priority +4 status move on the user whose `onPrepareHit` is
// `!!this.queue.willAct() && this.runEvent('StallMove', pokemon)` (the `stall` volatile's `onStallMove` draws `randomChance(1, counter)`,
// counter 3 on the first use and tripled by `onRestart` on each success, up to 729) and whose `onHit` adds `stall`; the move's own
// volatile (duration 1) is added first by `runMoveEffects`. Protect's, Spiky Shield's and Baneful Bunker's `onTryHit` (priority 3) blocks
// every move with the `protect` flag with NOT_FAIL (a contact move also costs its user 1/8 of its max HP, or poisons it), and Endure's
// `onDamage` (priority -10) leaves the holder 1 HP against a move's damage.
//
// The teams are slice 6's (gen-slice6.js `assign`: the pivot moves, forcing moves, Rocky Helmet, Eject Button, Red Card and Eject Pack of
// slices 6a-6d on slice 5's teams, with a Tera type on every set), then every set may get a protect move from a stream of its own, and
// each seed is one of these scenarios (the choices are a scripted opening: a pair per decision point, the random policy takes over when
// the script ends, and a script that Showdown rejects is dropped and the battle recorded without it):
//   random     the plain random policy over the sprinkled teams
//   block      a protect user (maybe holding Rocky Helmet, Red Card, Eject Button or Eject Pack) against one kind of attack for two turns:
//              a contact move, a multi-hit move, a pivot (U-turn, Flip Turn, Volt Switch), a forcing move (Roar, Whirlwind, Dragon Tail,
//              Circle Throw), Knock Off, Trick, Switcheroo, Thief, a status move with the protect flag, a move without it (a hazard)
//   contact    Spiky Shield or Baneful Bunker against contact moves; the attacker may be a Shedinja (it faints from the damage), a Poison
//              or Steel type (immune to the poison), already statused, or the user may Terastallize
//   endure     Endure against a move that would KO: a strong or multi-hit or fixed-damage move, a Shedinja user (0 damage) or attacker, a Life
//              Orb attacker, Rocky Helmet, Red Card or Eject Button on the Endure user
//   last       the user moves last, so the move fails with no draw: the foe switches, or both lead with a protect move and the slower one fails
//   climb      one protect move used every turn against an idle foe, so the counter climbs (3, 9, 27, ...) and fails and starts over; the
//              Showdown seed is searched for a run that reaches a high counter
//   mix        a protect user alternating its protect move with other moves over a dozen turns, against a foe that attacks
//   struggle   Ghost types whose only moves are a protect move and a blocked low-PP move: both sides run out of PP and Struggle, which has
//              the `protect` flag
//
// Usage:
//   node gen-slice7a.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice7a-<seed>.json, plus slice7a-moves.json, slice7a-species.json, slice7a-abilities.json and slice7a-items.json
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

const PROTECTS = ['protect', 'spikyshield', 'banefulbunker', 'endure'];
const PIVOTS = ['uturn', 'flipturn', 'voltswitch'];
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];
const ITEM_THIEVES = ['knockoff', 'trick', 'switcheroo', 'thief'];
const FILLER = ['recover', 'toxic', 'swordsdance', 'calmmind', 'willowisp', 'thunderwave'];
const STATUS_PROTECTED = ['toxic', 'willowisp', 'thunderwave', 'spore', 'glare', 'leechseed'];
const FIXED = ['seismictoss', 'nightshade', 'superfang', 'ruination', 'dragonrage', 'sonicboom'];

const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 1103515245 + 12345);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	const protects = PROTECTS.filter(has);
	if (protects.length !== PROTECTS.length) throw new Error('the pool lacks a protect move');

	// slice 6's teams and leads' scripts are replaced by this slice's
	let scenario = assign6(scenario0, pool, seed);
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const speciesName = (id) => dex.species.get(id).name;
	const lead = (side) => scenario.teams[side][0];
	const flags = (id) => dex.moves.get(id).flags;
	const accurate = (id) => { const a = dex.moves.get(id).accuracy; return a === true || a === 100; };
	const damaging = (id) => dex.moves.get(id).category !== 'Status';
	const typesOf = (set) => dex.species.get(set.species).types;
	const canHit = (id, set) => dex.getImmunity(dex.moves.get(id).type, typesOf(set));
	const pickFor = (ids, set) => { const ok = ids.filter(id => canHit(id, set)); return ok.length ? pick(ok) : pick(ids); };
	const CONTACT = pool.moves.filter(id => damaging(id) && flags(id).contact && accurate(id) && !PIVOTS.includes(id) && !FORCERS.includes(id));
	const MULTI = pool.moves.filter(id => damaging(id) && dex.moves.get(id).multihit && accurate(id));
	const STRONG = pool.moves.filter(id => { const m = dex.moves.get(id); return damaging(id) && m.basePower >= 80 && accurate(id) && !m.multihit && !PIVOTS.includes(id) && !FORCERS.includes(id); });
	const PLAIN = pool.moves.filter(id => damaging(id) && flags(id).protect && accurate(id) && !PIVOTS.includes(id) && !FORCERS.includes(id));
	const STATUSES = STATUS_PROTECTED.filter(has);
	const SPECIES = pool.species.filter(id => id !== 'shedinja');
	const POISONS = SPECIES.filter(id => dex.species.get(id).types.some(t => t === 'Poison' || t === 'Steel'));

	const filler = (s, ids) => {
		s.moves = [...new Set([...ids, ...FILLER.filter(has)])].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves.splice(int(4), 1, id);
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const setItem = (s, id) => { s.item = itemName(id); };
	const steady = (s) => { s.level = 100; s.ability = 'No Ability'; if (s.item === 'Assault Vest' || s.item === 'Heavy-Duty Boots') s.item = ''; if (s.species === 'Shedinja') s.species = speciesName(pick(SPECIES)); };

	// the sprinkle: a protect move on many sets (in the plain random battles most of all)
	const sprinkled = (s) => {
		if (int(100) < 50) put(s, pick(protects));
		if (int(100) < 8) put(s, pick(protects));
		if (int(100) < 5 && pool.items.includes('rockyhelmet')) setItem(s, 'rockyhelmet');
	};
	for (const side of [0, 1]) for (const s of scenario.teams[side]) sprinkled(s);

	const kinds = [['random', 22], ['block', 24], ['contact', 14], ['endure', 14], ['last', 8], ['climb', 3], ['mix', 12], ['struggle', 3]];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	scenario.kind = kind;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx; // A: the protect user, B: the other side
	const A = lead(aIdx), B = lead(bIdx);
	const duo = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const full = (n) => scenario.teams.map(team => `team ${[1, 2, 3, 4, 5, 6].slice(0, n === undefined ? team.length : n).join('')}`); // (a base team may be short)
	const teraStep = (mv) => (int(100) < 25 ? `${mv} terastallize` : mv);
	const prot = () => pick(protects);
	const itemFor = (ids) => pick(['', ...ids.filter(id => pool.items.includes(id))]);
	if (kind === 'struggle') {
		// Ghost types (Normal and Fighting moves cannot hit them) with a low-PP blocked move; A's other move is a protect move
		const ghosts = pool.species.filter(id => dex.species.get(id).types.includes('Ghost') && id !== 'shedinja');
		const blocked = pool.moves.filter(id => { const m = dex.moves.get(id); return (m.type === 'Normal' || m.type === 'Fighting') && m.pp <= 10 && damaging(id); });
		const size = 1 + int(2);
		scenario.maxTurns = 120;
		scenario.teams[aIdx] = Array.from({ length: size }, () => Object.assign({}, scenario.teams[aIdx][0], { species: speciesName(pick(ghosts)), moves: [prot(), pick(blocked)], level: 100, ability: 'No Ability', item: itemFor(['rockyhelmet', 'lifeorb']) }));
		scenario.teams[bIdx] = Array.from({ length: size }, () => Object.assign({}, scenario.teams[bIdx][0], { species: speciesName(pick(ghosts)), moves: [pick(blocked)], level: 100, ability: 'No Ability', item: itemFor(['rockyhelmet', 'lifeorb']) }));
		const turns = 40;
		scenario.choices = [full(size), ...Array.from({ length: turns }, () => duo(int(100) < 65 ? 'move 1' : 'move 2', 'move 1'))];
		return scenario;
	}
	steady(A); steady(B);
	if (kind === 'block') {
		A.item = itemFor(['rockyhelmet', 'redcard', 'ejectbutton', 'ejectpack', 'lifeorb', 'leftovers']);
		B.item = itemFor(['lifeorb', 'rockyhelmet', 'redcard', 'ejectbutton', 'ejectpack']);
		const v = int(100);
		let atk;
		if (v < 24) atk = pickFor(CONTACT, A);
		else if (v < 38) atk = pickFor(MULTI, A);
		else if (v < 54) atk = pick(PIVOTS);
		else if (v < 68) atk = pick(FORCERS);
		else if (v < 80) atk = pick(ITEM_THIEVES);
		else if (v < 92) atk = pick(STATUSES);
		else atk = pick(['stealthrock', 'spikes', 'toxicspikes', 'stickyweb'].filter(has));
		const second = pickFor(PLAIN, A);
		filler(A, [prot()]);
		filler(B, [atk, second]);
		// the protect move twice (the second use draws the stall roll), against the attack; then the random policy
		scenario.choices = [full(), duo(teraStep('move 1'), 'move 1'), duo('move 1', int(2) ? 'move 1' : 'move 2')];
	} else if (kind === 'contact') {
		A.item = itemFor(['rockyhelmet', 'lifeorb', 'leftovers']);
		const bane = int(2) === 0;
		A.moves = [bane ? 'banefulbunker' : 'spikyshield'];
		const v = int(100);
		if (v < 25) { B.species = 'Shedinja'; B.level = 100; B.item = ''; }
		else if (v < 45) { B.species = speciesName(pick(POISONS)); B.level = 100; }
		B.item = v < 25 ? '' : itemFor(['rockyhelmet', 'lifeorb', 'redcard', 'ejectbutton', 'ejectpack']);
		filler(A, A.moves);
		const status = int(100) < 25 ? pick(['toxic', 'willowisp']) : null;
		filler(B, [pickFor(CONTACT, A), pickFor(CONTACT, A), ...(status && has(status) ? [status] : [])]);
		const first = status ? duo('move 1', 'move 3') : duo(teraStep('move 1'), 'move 1');
		scenario.choices = [full(), first, duo('move 1', 'move 1'), duo('move 1', 'move 2')];
	} else if (kind === 'endure') {
		A.item = itemFor(['rockyhelmet', 'redcard', 'ejectbutton', 'ejectpack', 'leftovers', 'lifeorb']);
		B.item = itemFor(['lifeorb', 'rockyhelmet']);
		const v = int(100);
		if (v < 30) { A.species = 'Shedinja'; A.item = itemFor(['rockyhelmet', 'redcard', 'ejectbutton']); }
		else if (v < 40) { B.species = 'Shedinja'; }
		// A is weak enough to be KO'd by a strong move, so Endure matters
		A.level = v < 30 ? 100 : 20 + int(40);
		let atk;
		const w = int(100);
		if (w < 40) atk = pickFor(STRONG, A);
		else if (w < 60) atk = pickFor(MULTI, A);
		else if (w < 80) atk = pick(FIXED.filter(has).concat(STRONG));
		else atk = pickFor(CONTACT, A);
		A.moves = ['endure'];
		filler(A, A.moves);
		filler(B, [atk, pickFor(STRONG, A)]);
		scenario.choices = [full(), duo(teraStep('move 1'), 'move 1'), duo('move 1', 'move 1'), duo('move 2', 'move 1')];
	} else if (kind === 'last') {
		const variant = int(3);
		A.item = itemFor(['rockyhelmet', 'redcard', 'ejectbutton']);
		if (variant === 0) {
			// the foe switches: its switch precedes A's priority +4 move, which has nothing left to wait for and fails
			filler(A, [prot(), pickFor(PLAIN, B)]);
			filler(B, [pickFor(PLAIN, A)]);
			scenario.choices = [full(), duo('move 1', 'switch 2'), duo('move 1', 'move 1'), duo('move 1', 'switch 3')];
		} else {
			// both lead with a protect move: the slower (or the loser of a speed tie) fails with no draw; then it goes on
			filler(A, [prot(), pickFor(PLAIN, B)]);
			filler(B, [prot(), pickFor(PLAIN, A)]);
			if (variant === 2) { B.species = A.species; B.nature = A.nature; B.evs = A.evs; B.ivs = A.ivs; B.level = A.level; }
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 1')];
		}
	} else if (kind === 'climb') {
		// A uses its protect move every turn; B idles (recover). The Showdown seed is searched in `climbScenario`.
		// (a move has 8 to 16 PP once the PP Ups are counted, so a run is at most 16 turns: B idles over three moves)
		A.moves = [prot()]; filler(A, A.moves);
		B.moves = ['swordsdance', 'calmmind', 'recover']; filler(B, B.moves);
		A.item = ''; B.item = '';
		scenario.maxTurns = 40;
		scenario.choices = [full(), ...Array.from({ length: 16 }, (_, i) => duo('move 1', `move ${1 + (i % 3)}`))];
	} else if (kind === 'mix') {
		A.item = itemFor(['rockyhelmet', 'redcard', 'ejectbutton', 'ejectpack', 'leftovers']);
		B.item = itemFor(['lifeorb', 'rockyhelmet', 'redcard', 'ejectbutton', 'ejectpack']);
		filler(A, [prot(), pickFor(PLAIN, B)]);
		const atk = int(3) === 0 ? pick([...PIVOTS, ...FORCERS]) : pickFor(CONTACT.length ? CONTACT : PLAIN, A);
		filler(B, [atk, pickFor(PLAIN, A)]);
		scenario.choices = [full(), ...Array.from({ length: 12 }, () => duo(int(100) < 70 ? 'move 1' : 'move 2', int(100) < 70 ? 'move 1' : 'move 2'))];
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// The tally: outcomes that distinguish each part of the mechanic, counted by wrapping Showdown's own methods (observation only).

let activeTally = null;
let climbMax = 0; // the highest stall counter seen, for the climb search (counted even with no tally)
const t = () => activeTally;
const STRUCTURAL_ZERO = [];

function newTally() {
	return {
		prepareHit: { success: 0, failedLast: 0, failedRoll: 0, noStallRoll: 0 },
		stallRoll: { 3: 0, 9: 0, 27: 0, 81: 0, 243: 0, 729: 0 },
		stallRestartCounter: { 9: 0, 27: 0, 81: 0, 243: 0, 729: 0 },
		stallDeleted: 0,
		blockedBy: { protect: 0, spikyshield: 0, banefulbunker: 0 },
		blockedKind: { contact: 0, multihit: 0, status: 0, pivot: 0, forcer: 0, knockoff: 0, trick: 0, thief: 0, struggle: 0, other: 0 },
		blockedHolderItem: { rockyhelmet: 0, redcard: 0, ejectbutton: 0, ejectpack: 0 },
		passedProtect: { noFlag: 0 },
		spikyDamage: 0, spikyKilledAttacker: 0,
		baneSet: 0, baneImmune: 0, baneAlready: 0,
		endureSaved: 0, endureToZero: 0, endureMultiHit: 0, endureHolderItem: { rockyhelmet: 0, redcard: 0, ejectbutton: 0 },
	};
}

function install() {
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, ...rest) {
		const id = typeof status === 'string' ? status : status.id;
		const c = t();
		const had = id === 'stall' && !!this.volatiles.stall;
		const r = addVolatile.call(this, status, ...rest);
		if (id === 'stall' && this.volatiles.stall) {
			const counter = this.volatiles.stall.counter;
			if (counter > climbMax) climbMax = counter;
			if (c && had && c.stallRestartCounter[counter] !== undefined) c.stallRestartCounter[counter]++;
		}
		return r;
	};
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, ...rest) {
		const c = t();
		if (c && eventid === 'StallMove') {
			const stall = target.volatiles.stall;
			const r = runEvent.call(this, eventid, target, ...rest);
			if (stall) {
				if (c.stallRoll[stall.counter] !== undefined) c.stallRoll[stall.counter]++;
				if (!r) c.stallDeleted++;
			} else c.prepareHit.noStallRoll++;
			return r;
		}
		if (c && eventid === 'Damage' && target && target.volatiles && target.volatiles.endure) {
			const effect = rest[1];
			const hp = target.hp;
			const r = runEvent.call(this, eventid, target, ...rest);
			if (effect && effect.effectType === 'Move' && rest[2] >= hp) {
				c.endureSaved++;
				if (r === 0) c.endureToZero++;
				if (this.activeMove && this.activeMove.multihit) c.endureMultiHit++;
				for (const it of ['rockyhelmet', 'redcard', 'ejectbutton']) if (target.item === it) c.endureHolderItem[it]++;
			}
			return r;
		}
		return runEvent.call(this, eventid, target, ...rest);
	};
	// the stalling moves' PrepareHit: why a use failed
	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, ...rest) {
		const c = t();
		if (!(c && eventid === 'PrepareHit' && effect && effect.stallingMove)) return singleEvent.call(this, eventid, effect, ...rest);
		const willAct = !!this.queue.willAct();
		const target = rest[1]; // singleEvent(eventid, effect, state, target, ...)
		const hadStall = !!(target && target.volatiles.stall);
		const r = singleEvent.call(this, eventid, effect, ...rest);
		if (r) c.prepareHit.success++;
		else if (!willAct) c.prepareHit.failedLast++;
		else if (hadStall) c.prepareHit.failedRoll++;
		return r;
	};
	// the block itself: which volatile, against which kind of move, with which item on the holder
	const checkMoveBypassesProtect = Battle.prototype.checkMoveBypassesProtect;
	Battle.prototype.checkMoveBypassesProtect = function (move, attacker, defender, ...rest) {
		const r = checkMoveBypassesProtect.call(this, move, attacker, defender, ...rest);
		const c = t();
		if (c) {
			const id = this.effect && this.effect.id;
			if (r) c.passedProtect.noFlag++;
			else if (c.blockedBy[id] !== undefined) {
				c.blockedBy[id]++;
				const kind = move.id === 'struggle' ? 'struggle' : move.multihit ? 'multihit' : PIVOTS.includes(move.id) ? 'pivot' : FORCERS.includes(move.id) ? 'forcer'
					: move.id === 'knockoff' ? 'knockoff' : (move.id === 'trick' || move.id === 'switcheroo') ? 'trick' : move.id === 'thief' ? 'thief'
					: move.category === 'Status' ? 'status' : move.flags.contact ? 'contact' : 'other';
				c.blockedKind[kind]++;
				if (c.blockedHolderItem[defender.item] !== undefined) c.blockedHolderItem[defender.item]++;
			}
		}
		return r;
	};
	// Spiky Shield's damage and Baneful Bunker's poison
	const damage = Battle.prototype.damage;
	Battle.prototype.damage = function (amount, target, source, effect, ...rest) {
		const c = t();
		const eff = effect || (this.event && this.effect);
		const watch = c && eff && eff.id === 'spikyshield' && target && target.hp;
		const r = damage.call(this, amount, target, source, effect, ...rest);
		if (watch) { c.spikyDamage++; if (target && !target.hp) c.spikyKilledAttacker++; }
		return r;
	};
	const trySetStatus = Pokemon.prototype.trySetStatus;
	Pokemon.prototype.trySetStatus = function (status, source, sourceEffect, ...rest) {
		const c = t();
		const watch = c && sourceEffect && sourceEffect.id === 'banefulbunker';
		const had = this.status;
		const r = trySetStatus.call(this, status, source, sourceEffect, ...rest);
		if (!watch) return r;
		if (r) c.baneSet++;
		else if (had) c.baneAlready++;
		else c.baneImmune++;
		return r;
	};
}

// Records a scenario. A scripted opening that Showdown rejects (a pivot or a forced switch made a different request than the script
// expected, or a bench Pokemon's move is disabled) is cut to its longest prefix that Showdown accepts, and the random policy takes over
// after it: the tally of the failed attempts is not counted (the probes run with no tally).
function recordScenario(scenario) {
	const accepts = (k) => {
		const saved = activeTally;
		activeTally = null;
		try { record(Object.assign({}, scenario, { choices: scenario.choices.slice(0, k) })); return true; } catch (e) { return false; } finally { activeTally = saved; }
	};
	const before = JSON.stringify(activeTally);
	try {
		return { trace: record(scenario), scripted: !!scenario.choices, kept: scenario.choices ? scenario.choices.length : 0 };
	} catch (e) {
		if (!scenario.choices) throw e;
		activeTally = JSON.parse(before);
		let lo = 0, hi = scenario.choices.length - 1; // the longest accepted prefix is in [lo, hi]
		while (lo < hi) {
			const mid = Math.ceil((lo + hi) / 2);
			if (accepts(mid)) lo = mid; else hi = mid - 1;
		}
		const cut = Object.assign({}, scenario, { choices: scenario.choices.slice(0, lo) });
		return { trace: record(cut), scripted: lo > 0, dropped: lo === 0 ? e.message : null, cut: lo };
	}
}

// The climb scenarios: the same teams under different Showdown seeds, until a run reaches a high stall counter (the draws are
// random, so most runs fail at the second or third use); the best run is recorded with the tally on.
function climbScenario(scenario) {
	const saved = activeTally;
	activeTally = null;
	let best = scenario, bestMax = -1;
	const rnd = mulberry32(scenario.policySeed * 7919 + 3);
	for (let attempt = 0; attempt < 150 && bestMax < 243; attempt++) {
		const hex = () => Math.floor(rnd() * 65536).toString(16).padStart(4, '0');
		const s = Object.assign({}, scenario, { seed: `gen5,${hex()}${hex()}${hex()}${hex()}` });
		climbMax = 0;
		try { record(s); } catch (e) { continue; }
		if (climbMax > bestMax) { best = s; bestMax = climbMax; }
	}
	activeTally = saved;
	return best;
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice7a.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice7a-${k}.json`), JSON.stringify(pool[k]));
	}
	activeTally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scriptedCount = 0, dropped = 0, cutCount = 0;
	const kinds = {};
	for (let seed = first; seed < first + count; seed++) {
		let scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		kinds[scenario.kind] = (kinds[scenario.kind] || 0) + 1;
		if (scenario.kind === 'climb') scenario = climbScenario(scenario);
		delete scenario.kind;
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scriptedCount++;
		if (result.cut !== undefined) { cutCount++; if (process.env.SLICE7A_VERBOSE) console.error(`seed ${seed}: script cut to ${result.cut} of ${scenario.choices.length}: ${result.dropped || ''}`); }
		if (result.dropped) dropped++;
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice7a-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice7a seeds, ${written} traces written (${scriptedCount} scripted, ${cutCount} scripts cut, ${dropped} dropped altogether), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error(`kinds: ${JSON.stringify(kinds)}`);
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
