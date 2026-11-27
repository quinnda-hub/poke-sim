#!/usr/bin/env node
'use strict';
// Generates battles for "slice 7b" (volatile conditions, phase 7b: Substitute) and records each on Showdown.
//
// Substitute is the first escape hatch. Its volatile's `onTryPrimaryHit` (data/moves.ts 18348-18381) takes every hit of a damaging move
// on its holder: `getDamage` (crit and random draws as usual) capped at the substitute's HP, recoil and drain from that damage, the
// move's `onAfterSubDamage`, and a `HIT_SUBSTITUTE` result that makes `spreadMoveHit` drop the target from the damage, `runMoveEffects`
// (no `onHit`, status, boost or item steps), the force switch, `DamagingHit` and `onAfterHit`, while the self drops, the `selfSwitch` flag
// and the secondaries' draws still run. A status move on the holder finds `getDamage` undefined and fails, unless it has `bypasssub`.
//
// The teams are slice 6's (gen-slice6.js `assign`, itself slice 5's under hazards, weather, terrain and Paradox, and with 6's pivots,
// forcing moves, Regenerator, Natural Cure, Eject Button, Red Card and Eject Pack in the sets), then each Pokemon gets Substitute and
// its foes' moves from a stream of its own, so the earlier slices stay reproducible from their seeds. Every scenario is recorded with
// `tera: true` and a `teraChance`. About a quarter of the battles are the plain random policy over those teams; the rest are scripted
// openings (a pair of choices per decision point; the random policy takes over when the script ends, and a script that Showdown rejects
// is dropped and the battle recorded without it). A is the side that uses Substitute, B the one that attacks it:
//   basic      A is the faster and substitutes, B attacks each turn: damage into a substitute, its break, a new substitute
//   multi      B has a multi-hit move: it breaks the substitute in the middle of its hits, and the hits after it land on A
//   recoil     B has recoil and drain moves, and A hits B first: recoil and drain are computed from the damage to the substitute
//   status     B uses status moves on the substitute: they fail, unless they have `bypasssub` (the sound moves, Defog, Roar ...)
//   phaze      B (slower) uses Roar, Whirlwind, Dragon Tail or Circle Throw on the substitute (the first two bypass it and drag; the
//              others hit the substitute and drag nothing)
//   pivot      B uses U-turn, Flip Turn or Volt Switch on the substitute: the damage goes to it and B still switches
//   items      Knock Off or Thief against an item holder; Rocky Helmet; Red Card or Eject Button; Life Orb or Expert Belt on the attacker
//   low        A substitutes again and again (the cost leaves it at 75, 50, 25 percent: a Substitute at a quarter fails), or is Shedinja
//   secondary  B's moves with secondary effects and self drops (Close Combat, Draco Meteor, Rapid Spin ...) against the substitute
//   struggle   B's one attack runs out of PP: Struggle and its recoil against a substitute
//
// Usage:
//   node gen-slice7b.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice7b-<seed>.json, plus slice7b-moves.json, slice7b-species.json, slice7b-abilities.json and slice7b-items.json
// (copies of the pool, which the parity test compares with what the engine accepts). A tally of the outcomes that distinguish each
// part of the mechanic is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers pass
// every argument and result through and call nothing the battle would not call).

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

const PIVOTS = ['uturn', 'flipturn', 'voltswitch'];
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];
const ITEM_THIEVES = ['knockoff', 'thief'];
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 1103515245 + 12345);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	if (!has('substitute')) throw new Error('the pool has no substitute');

	// slice 6's scenario: teams, Tera settings (and its scripted opening, which this slice replaces)
	const scenario = assign6(scenario0, pool, seed);
	delete scenario.choices;

	const speciesName = (id) => dex.species.get(id).name;
	const speed = (id) => dex.species.get(id).baseStats.spe;
	const bySpeed = pool.species.filter(id => id !== 'shedinja').sort((x, y) => speed(x) - speed(y));
	const third = Math.max(1, Math.floor(bySpeed.length / 3));
	const fast = bySpeed.slice(bySpeed.length - third), slow = bySpeed.slice(0, third);
	const mv = (id) => dex.moves.get(id);
	const accurate = (id) => mv(id).accuracy === true || mv(id).accuracy === 100;
	const damaging = pool.moves.filter(id => mv(id).category !== 'Status' && mv(id).priority === 0);
	const plain = damaging.filter(id => mv(id).basePower >= 60 && accurate(id) && !mv(id).multihit && !mv(id).selfSwitch && !mv(id).forceSwitch &&
		!mv(id).recoil && !mv(id).drain && !mv(id).self && !mv(id).selfBoost && !mv(id).selfdestruct && !(mv(id).flags || {}).charge);
	const multi = damaging.filter(id => mv(id).multihit && accurate(id));
	const recoilMoves = damaging.filter(id => mv(id).recoil && accurate(id)), drainMoves = damaging.filter(id => mv(id).drain && accurate(id));
	const contact = plain.filter(id => (mv(id).flags || {}).contact);
	const withSecondary = damaging.filter(id => (mv(id).secondaries || []).length || mv(id).self || mv(id).selfBoost);
	const statusFoe = pool.moves.filter(id => mv(id).category === 'Status' && ['normal', 'any', 'allAdjacentFoes'].includes(mv(id).target) && mv(id).priority === 0);
	const bypass = statusFoe.filter(id => (mv(id).flags || {}).bypasssub), nonBypass = statusFoe.filter(id => !(mv(id).flags || {}).bypasssub);
	const lowPp = damaging.filter(id => mv(id).pp <= 8 && accurate(id) && !mv(id).selfdestruct);

	const types = (set) => dex.species.get(set.species).types;
	const canHit = (id, set) => dex.getImmunity(mv(id).type, types(set));
	const pickFor = (ids, set) => { const ok = ids.filter(id => canHit(id, set)); return ok.length ? pick(ok) : pick(ids); };
	const moves = (s, ...ids) => {
		const first = [...new Set(ids.filter(id => has(id)))];
		const pad = ['recover', 'toxic', 'swordsdance', 'calmmind'].filter(id => has(id) && !first.includes(id));
		s.moves = [...first, ...pad].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves.splice(int(4), 1, id);
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const steady = (s, level) => {
		s.level = level;
		if (s.item === 'Assault Vest') s.item = '';
		s.ability = 'No Ability';
		if (s.species === 'Shedinja') s.species = speciesName(pick(bySpeed));
	};
	const setItem = (s, id) => { s.item = itemName(id); };

	// the sprinkle: Substitute and its foes' moves on every set (a scripted battle overwrites the leads it needs)
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 40) put(s, 'substitute');
		if (int(100) < 18 && multi.length) put(s, pick(multi));
		if (int(100) < 16) put(s, pick([...recoilMoves, ...drainMoves]));
		if (int(100) < 12) put(s, pick(ITEM_THIEVES.filter(has)));
		if (int(100) < 10 && has('trick')) put(s, pick(['trick', 'switcheroo'].filter(has)));
		if (int(100) < 9) put(s, pick(withSecondary));
		if (int(100) < 7) put(s, pick(PIVOTS.concat(FORCERS).filter(has)));
		if (int(100) < 6) put(s, pick(nonBypass.concat(bypass)));
		if (int(100) < 8) setItem(s, pick(['rockyhelmet', 'lifeorb', 'expertbelt', 'redcard', 'ejectbutton'].filter(id => pool.items.includes(id))));
	}

	const kinds = [
		['random', 22], ['basic', 12], ['multi', 11], ['recoil', 8], ['status', 10], ['phaze', 8], ['pivot', 8], ['items', 8], ['low', 6],
		['secondary', 4], ['struggle', 3],
	];
	let kind = 'random';
	const roll = int(100);
	for (let acc = 0, i = 0; i < kinds.length; i++) { acc += kinds[i][1]; if (roll < acc) { kind = kinds[i][0]; break; } }

	// a team of fewer than six (slice 6's solo kind) takes no scripted opening: `team 123456` needs six on both sides
	if (scenario.teams[0].length !== 6 || scenario.teams[1].length !== 6) kind = 'random';
	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const duo = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const TEAM = ['team 123456', 'team 123456'];
	const fastA = () => { A.species = speciesName(pick(fast)); B.species = speciesName(pick(slow)); };
	const prepare = (aLevel, bLevel) => {
		steady(A, aLevel); steady(B, bLevel);
		A.item = ''; B.item = '';
		for (const s of [A, B]) if (s.item === 'Heavy-Duty Boots') s.item = '';
	};
	const subFirst = (extra) => { moves(A, 'substitute', ...extra); };
	const times = (n, f) => Array.from({ length: n }, (_, i) => f(i));

	if (kind === 'basic') {
		// A substitutes, B attacks with a strong accurate move every turn, A substitutes again whenever the last one has gone
		fastA(); prepare(100, 100);
		const atk = pickFor(plain, A);
		subFirst(['recover']); moves(B, atk, pickFor(plain, A));
		scenario.choices = [TEAM, ...times(3 + int(3), () => duo('move 1', int(2) ? 'move 1' : 'move 2'))];
	} else if (kind === 'multi') {
		// a multi-hit move against a substitute: the break in the middle of the hits, then the hits that reach A
		fastA(); prepare(100, 100 - int(3) * 10);
		const m1 = pickFor(multi, A);
		subFirst(['recover']); moves(B, m1, pickFor(multi, A), pickFor(plain, A));
		scenario.choices = [TEAM, ...times(4 + int(3), () => duo('move 1', int(4) ? 'move 1' : 'move 2'))];
	} else if (kind === 'recoil') {
		// A hits B first (so a drain has something to heal), then substitutes, then B's recoil and drain moves meet the substitute
		fastA(); prepare(100, 100);
		const strike = pickFor(plain, B);
		subFirst([strike]); moves(B, pickFor(recoilMoves, A), pickFor(drainMoves, A), pickFor(recoilMoves.concat(drainMoves), A));
		scenario.choices = [TEAM, duo('move 2', 'move 2'), duo('move 1', 'move 2'), duo('move 2', 'move 1'), duo('move 1', 'move 3'), duo('move 2', 'move 3')];
	} else if (kind === 'status') {
		// B's status moves against the substitute: the failures and the moves that bypass it
		fastA(); prepare(100, 100);
		const pool4 = [pick(nonBypass), pick(nonBypass), pick(bypass), pick(nonBypass.concat(bypass))];
		moves(A, 'substitute', 'recover'); moves(B, ...[...new Set(pool4)]);
		scenario.choices = [TEAM, duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 2', 'move 3'), duo('move 2', 'move 4')];
	} else if (kind === 'phaze') {
		// B moves last (priority -6): A has substituted by then
		prepare(100, 100); fastA();
		const f = pick(FORCERS.filter(has));
		moves(A, 'substitute', 'recover'); moves(B, f, pickFor(plain, A));
		scenario.choices = [TEAM, duo('move 1', 'move 1'), duo('move 2', 'move 1')];
	} else if (kind === 'pivot') {
		// A is the faster: its substitute is up when B pivots; B's request is the only one
		prepare(100, 100); fastA();
		const p = pick(PIVOTS.filter(id => has(id) && canHit(id, A)).concat(['uturn']));
		moves(A, 'substitute', 'recover'); moves(B, p, pickFor(plain, A));
		scenario.choices = [TEAM, duo('move 1', 'move 1'), duo(null, 'switch 2'), duo('move 2', 'move 1')];
	} else if (kind === 'items') {
		prepare(100, 100); fastA();
		const v = int(5);
		moves(A, 'substitute', 'recover');
		if (v === 0) { // Knock Off or Thief against an item holder (and a Trick)
			setItem(A, pick(['leftovers', 'lifeorb', 'blacksludge', 'assaultvest', 'sharpbeak'].filter(id => pool.items.includes(id) && id !== 'assaultvest')));
			moves(B, pick(ITEM_THIEVES.filter(has)), has('trick') ? 'trick' : 'recover', pickFor(plain, A));
			scenario.choices = [TEAM, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 2', 'move 2')];
		} else if (v === 1) { // contact moves into a Rocky Helmet holder behind its substitute, and after it breaks
			setItem(A, 'rockyhelmet'); B.item = '';
			moves(B, pickFor(contact, A), pickFor(contact, A));
			scenario.choices = [TEAM, ...times(3, () => duo('move 1', 'move 1'))];
		} else if (v === 2) { // a multi-hit break into a Red Card or Eject Button holder
			setItem(A, pick(['redcard', 'ejectbutton'].filter(id => pool.items.includes(id))));
			moves(B, pickFor(multi, A), pickFor(plain, A));
			scenario.choices = [TEAM, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 2', 'move 2')];
		} else if (v === 3) { // a Red Card or Eject Button holder behind its substitute: nothing happens until the hit lands on it
			setItem(A, pick(['redcard', 'ejectbutton'].filter(id => pool.items.includes(id))));
			moves(B, pickFor(plain, A), pickFor(plain, A));
			scenario.choices = [TEAM, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 2', 'move 2')];
		} else { // the attacker's Life Orb and Expert Belt (damage modifiers in the sub's getDamage, recoil after it)
			setItem(B, pick(['lifeorb', 'expertbelt'].filter(id => pool.items.includes(id))));
			moves(B, pickFor(plain, A), pickFor(recoilMoves, A));
			scenario.choices = [TEAM, duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 2', 'move 1')];
		}
	} else if (kind === 'low') {
		// the cost: 25 percent each time (a quarter or less of the maximum fails), then Shedinja, whose maximum is 1
		prepare(int(3) === 0 ? 1 + int(100) : 100, 100); fastA();
		if (int(5) === 0) A.species = 'Shedinja';
		moves(A, 'substitute', 'recover'); moves(B, pickFor(plain, A), pickFor(plain, A));
		scenario.choices = [TEAM, ...times(5 + int(3), (i) => duo(i % 5 === 3 ? 'move 2' : 'move 1', 'move 1'))];
	} else if (kind === 'secondary') {
		prepare(100, 100); fastA();
		moves(A, 'substitute', 'recover'); moves(B, pickFor(withSecondary, A), pickFor(withSecondary, A), pickFor(withSecondary, A));
		scenario.choices = [TEAM, ...times(4, () => duo('move 1', 'move ' + (1 + int(3))))];
	} else if (kind === 'struggle') {
		// B's only attack has few PP: after they run out Struggle (recoil by max HP) meets the substitute
		prepare(100, 20 + int(30)); fastA();
		const m = pickFor(lowPp, A);
		moves(A, 'substitute', 'recover'); B.moves = [m];
		scenario.choices = [TEAM, ...times(Math.floor(mv(m).pp * 8 / 5) + 2, (i) => duo(i % 3 === 2 ? 'move 2' : 'move 1', 'move 1'))];
	}

	// Coverage: the long tier needs every accepted species in some team of the newest set. The last bench member of the second side
	// (never part of a script: the scripts use the leads) takes the species at the seed's place in the pool.
	const covered = scenario.teams[1];
	covered[covered.length === 6 ? 5 : 0].species = speciesName(pool.species[(seed - 1) % pool.species.length]);
	scenario.kind7b = kind;
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally: observation hooks on Showdown's classes. Nothing is changed: each wrapper calls the original with every argument and
// returns its result.

let tally = null;
function bump(key, n = 1) { tally[key] = (tally[key] || 0) + n; }

function install() {
	const tryPrimaryHitEvent = BattleActions.prototype.tryPrimaryHitEvent;
	BattleActions.prototype.tryPrimaryHitEvent = function (damage, targets, pokemon, move, moveData, isSecondary) {
		const subBefore = targets.map(t => (t && t.volatiles && t.volatiles.substitute ? t.volatiles.substitute.hp : 0));
		const res = tryPrimaryHitEvent.call(this, damage, targets, pokemon, move, moveData, isSecondary);
		targets.forEach((t, i) => {
			if (!t || t === pokemon) return;
			const id = move.id;
			if (!subBefore[i]) {
				if (move.__subBrokeOnHit && move.multihit) bump('multihitHitsHolderAfterBreak');
				return;
			}
			const kind = res[i] === this.battle.HIT_SUBSTITUTE ? 'took' : res[i] === null ? 'failed' : 'bypassed';
			bump(`sub.${kind}.${move.category === 'Status' ? 'status' : 'damaging'}`);
			bump(`subMove.${kind}.${id}`);
			if (kind !== 'took') return;
			if (!t.volatiles.substitute) { bump('subBroken'); if (move.multihit) { bump('subBrokenByMultihit'); move.__subBrokeOnHit = true; } }
			if (move.recoil || move.struggleRecoil) bump('subHitRecoil');
			if (move.struggleRecoil) bump('subHitStruggle');
			if (move.drain) bump('subHitDrain');
			if (move.selfSwitch) bump('subHitPivot');
			if (move.forceSwitch) bump('subHitForcer');
			if (move.self || move.selfBoost) bump('subHitSelfDrop');
			if ((move.secondaries || []).length) bump('subHitSecondaries');
			if (move.onAfterSubDamage) bump('subHitAfterSubDamage');
			if (move.flags.contact && t.item === 'rockyhelmet') bump('subHitContactRockyHelmet');
			if (t.item === 'redcard' || t.item === 'ejectbutton') bump(`subHitHolder.${t.item}`);
			if (pokemon.item === 'lifeorb') bump('subHitLifeOrb');
			if (['knockoff', 'thief'].includes(id) && t.item) bump('subHitItemThief');
		});
		return res;
	};
	const addVolatile = Battle.prototype.add;
	Battle.prototype.add = function (...args) {
		if (args[0] === '-fail' && args[2] === 'move: Substitute') bump(args[3] === '[weak]' ? 'subFailWeak' : 'subFailAlready');
		else if (args[0] === '-start' && args[2] === 'Substitute') bump('subCreated');
		else if (args[0] === '-activate' && args[2] === 'move: Substitute') bump('subActivated');
		return addVolatile.apply(this, args);
	};
	const runMoveEffects = BattleActions.prototype.runMoveEffects;
	BattleActions.prototype.runMoveEffects = function (damage, targets, source, move, moveData, isSecondary, isSelf) {
		const r = runMoveEffects.call(this, damage, targets, source, move, moveData, isSecondary, isSelf);
		if (!isSecondary && !isSelf && moveData === move && targets[0] === null && move.selfSwitch && source.switchFlag === move.id) bump('pivotFlagSetThroughSub');
		return r;
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
		console.error('usage: node gen-slice7b.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice7b-${k}.json`), JSON.stringify(pool[k]));
	}
	tally = {};
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scriptedCount = 0, dropped = 0;
	const kindsSeen = {};
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		kindsSeen[scenario.kind7b] = (kindsSeen[scenario.kind7b] || 0) + 1;
		if (result.scripted) scriptedCount++;
		if (result.dropped) { dropped++; bump(`droppedScript.${scenario.kind7b}`); if (process.env.SLICE7B_VERBOSE) console.error(`seed ${seed}: script dropped: ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice7b-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice7b seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error(`kinds: ${JSON.stringify(kindsSeen)}`);
	console.error('tally (from Showdown\'s own events):');
	const subMoves = Object.keys(tally).filter(k => k.startsWith('subMove.')).sort();
	for (const k of Object.keys(tally).filter(k => !k.startsWith('subMove.')).sort()) console.error(`  ${k}: ${tally[k]}`);
	console.error(`  moves meeting a substitute (kind.move: count): ${subMoves.map(k => `${k.slice(8)}=${tally[k]}`).join(' ')}`);
	const need = ['sub.took.damaging', 'sub.failed.status', 'sub.bypassed.status', 'subBroken', 'subBrokenByMultihit', 'multihitHitsHolderAfterBreak', 'subHitRecoil',
		'subHitDrain', 'subHitPivot', 'pivotFlagSetThroughSub', 'subHitForcer', 'subHitSelfDrop', 'subHitSecondaries', 'subHitAfterSubDamage', 'subHitContactRockyHelmet',
		'subHitItemThief', 'subFailWeak', 'subFailAlready', 'subCreated', 'subHitLifeOrb', 'subHitHolder.redcard', 'subHitHolder.ejectbutton', 'subHitStruggle'];
	const zeros = need.filter(k => !tally[k]);
	if (zeros.length) console.error(`ZERO COUNTERS: ${zeros.join(', ')}`);
	else console.error('no counter is zero');
}

module.exports = { assign };
