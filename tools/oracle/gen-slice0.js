#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 0" and records each on Showdown.
//
// Slice 0 is the narrowest set of mechanics the engine must reproduce first:
// Gen 9 singles, 6v6, no items, an ability with no effect, and only plain
// damaging moves (no secondary effects, no handler code, single target).
//
// Usage:
//   node gen-slice0.js <outDir> <count> [firstSeed=1] [format=gen9ou] [profile=random]
//
// Profiles:
//   random    Six random Pokemon a side with four random plain moves each.
//   struggle  Small teams of Ghost types that know one low-PP Normal or
//             Fighting move, so every hit is blocked by immunity, PP runs
//             out, and the battle is decided by Struggle and its recoil.
//
// Output: <outDir>/slice0-<seed>.json (random) or
// <outDir>/slice0-struggle-<seed>.json (struggle), one trace per battle, plus
// <outDir>/slice0-moves.json and <outDir>/slice0-species.json listing the move
// and species ids the slice draws from. A count of 0 writes only those lists.

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { record } = require('./record.js');

const dex = Dex.mod('gen9');

// A move may carry any key that Tackle (the reference plain move) carries;
// any other key must be unset.
const PLAIN_KEYS = new Set(Object.keys(dex.moves.get('tackle')));
// Fields that must be unset/falsy for a move to be plain.
const MUST_BE_EMPTY = [
	'isNonstandard', 'secondary', 'damage', 'volatileStatus', 'ignoreImmunity', 'pseudoWeather',
	'isZ', 'isMax', 'ohko', 'thawsTarget', 'heal', 'forceSwitch', 'selfSwitch', 'selfBoost',
	'selfdestruct', 'breaksProtect', 'recoil', 'drain', 'mindBlownRecoil', 'stealsBoosts',
	'struggleRecoil', 'overrideOffensivePokemon', 'overrideOffensiveStat',
	'overrideDefensivePokemon', 'overrideDefensiveStat', 'ignoreAbility', 'ignoreAccuracy',
	'ignoreDefensive', 'ignoreEvasion', 'ignoreNegativeOffensive', 'ignoreOffensive',
	'ignorePositiveDefensive', 'ignorePositiveEvasion', 'multiaccuracy', 'multihit',
	'noDamageVariance', 'willCrit', 'tracksTarget', 'sleepUsable', 'smartTarget',
	'stallingMove', 'status', 'weather', 'terrain', 'sideCondition', 'slotCondition',
	'boosts', 'self', 'basePowerCallback', 'callsMove', 'hasCrashDamage', 'forceSTAB',
	'realMove', 'nonGhostTarget', 'defensiveCategory',
];
// Flags with no effect when no ability or item is in play and nothing is behind a substitute.
const INERT_FLAGS = new Set([
	'contact', 'protect', 'mirror', 'metronome', 'slicing', 'punch', 'bite', 'bullet',
	'pulse', 'wind', 'distance', 'allyanim', 'noassist', 'failcopycat', 'failmefirst',
	'nosleeptalk', 'failinstruct', 'failencore', 'failmimic', 'nosketch',
]);

function isPlainMove(move) {
	if (!move.exists || move.category === 'Status' || !(move.basePower > 0)) return false;
	if (move.target !== 'normal' && move.target !== 'any') return false;
	for (const k of MUST_BE_EMPTY) if (move[k]) return false;
	if (move.secondaries && move.secondaries.length) return false;
	for (const [k, v] of Object.entries(move)) {
		if (typeof v === 'function') return false;
		if (!PLAIN_KEYS.has(k) && v) return false;
	}
	if (move.condition && Object.keys(move.condition).length) return false;
	for (const f of Object.keys(move.flags)) if (!INERT_FLAGS.has(f)) return false;
	return true;
}

const MOVES = dex.moves.all().filter(isPlainMove).map(m => m.id).sort();
const SPECIES = dex.species.all()
	.filter(s => s.exists && s.num > 0 && !s.isNonstandard && !s.forme && !s.battleOnly)
	.map(s => s.id).sort();
if (MOVES.length < 4) throw new Error(`slice 0 move filter kept only ${MOVES.length} moves`);
const NATURES = dex.natures.all().map(n => n.name).sort();
const STATS = ['hp', 'atk', 'def', 'spa', 'spd', 'spe'];

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

// Struggle profile: Ghost types (immune to Normal and Fighting) with one
// low-PP Normal or Fighting move. Shedinja is included for its fixed 1 HP.
const GHOSTS = [...new Set([...SPECIES.filter(id => dex.species.get(id).types.includes('Ghost')), 'shedinja'])].sort();
const BLOCKED_MOVES = MOVES.filter(id => {
	const m = dex.moves.get(id);
	return (m.type === 'Normal' || m.type === 'Fighting') && m.pp <= 10;
});
if (BLOCKED_MOVES.length < 2) throw new Error(`struggle profile kept only ${BLOCKED_MOVES.length} moves`);

function makeScenario(seed, format, profile = 'random') {
	if (profile === 'struggle') return makeStruggleScenario(seed, format);
	if (profile !== 'random') throw new Error(`unknown profile ${profile}`);
	const rnd = mulberry32(seed * 2654435761);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	// Some battles share one speed-relevant setup across both teams so that
	// speed ties occur; most do not.
	const mirror = int(8) === 0;

	const makeSet = () => {
		const moves = new Set();
		while (moves.size < 4) moves.add(pick(MOVES));
		const evs = {}, ivs = {};
		for (const s of STATS) { evs[s] = int(4) === 0 ? 0 : int(64) * 4; ivs[s] = int(4) === 0 ? int(32) : 31; }
		const set = {
			name: '', species: dex.species.get(pick(SPECIES)).name,
			ability: 'No Ability', item: '', moves: [...moves],
			nature: pick(NATURES), evs, ivs,
			level: int(5) === 0 ? 1 + int(100) : 100,
		};
		// Leave gender unset sometimes so Showdown draws it.
		const g = int(3);
		if (g === 0) set.gender = 'M'; else if (g === 1) set.gender = 'F';
		return set;
	};
	const makeTeam = () => Array.from({ length: 6 }, makeSet);

	const t1 = makeTeam();
	const t2 = mirror ? JSON.parse(JSON.stringify(t1)) : makeTeam();
	const hex = (n) => n.toString(16).padStart(4, '0');
	return {
		format,
		seed: `gen5,${hex(int(65536))}${hex(int(65536))}${hex(int(65536))}${hex(int(65536))}`,
		policySeed: seed,
		maxTurns: 300,
		teams: [t1, t2],
	};
}

function makeStruggleScenario(seed, format) {
	const rnd = mulberry32(seed * 2246822519 + 7);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const mirror = int(4) === 0;
	const size = 1 + int(3);
	const makeSet = () => {
		const evs = {}, ivs = {};
		for (const s of STATS) { evs[s] = int(4) === 0 ? 0 : int(64) * 4; ivs[s] = int(4) === 0 ? int(32) : 31; }
		const set = {
			name: '', species: dex.species.get(pick(GHOSTS)).name,
			ability: 'No Ability', item: '', moves: [pick(BLOCKED_MOVES)],
			nature: pick(NATURES), evs, ivs,
			level: int(3) === 0 ? 1 + int(100) : 100,
		};
		const g = int(3);
		if (g === 0) set.gender = 'M'; else if (g === 1) set.gender = 'F';
		return set;
	};
	const makeTeam = () => Array.from({ length: size }, makeSet);
	const t1 = makeTeam();
	const t2 = mirror ? JSON.parse(JSON.stringify(t1)) : makeTeam();
	const hex = (n) => n.toString(16).padStart(4, '0');
	return {
		format,
		seed: `gen5,${hex(int(65536))}${hex(int(65536))}${hex(int(65536))}${hex(int(65536))}`,
		policySeed: seed,
		maxTurns: 600,
		teams: [t1, t2],
	};
}

if (require.main === module) {
	const [outDir, countArg, firstArg, formatArg, profileArg] = process.argv.slice(2);
	if (!outDir || !countArg) {
		console.error('usage: node gen-slice0.js <outDir> <count> [firstSeed=1] [format=gen9ou] [profile=random]');
		process.exit(2);
	}
	const count = parseInt(countArg, 10), first = parseInt(firstArg || '1', 10);
	const format = formatArg || 'gen9ou';
	const profile = profileArg || 'random';
	const prefix = profile === 'random' ? 'slice0' : `slice0-${profile}`;
	fs.mkdirSync(outDir, { recursive: true });
	fs.writeFileSync(path.join(outDir, 'slice0-moves.json'), JSON.stringify(MOVES));
	fs.writeFileSync(path.join(outDir, 'slice0-species.json'), JSON.stringify([...new Set([...SPECIES, 'shedinja'])].sort()));
	let turns = 0, draws = 0, truncated = 0;
	for (let seed = first; seed < first + count; seed++) {
		const trace = record(makeScenario(seed, format, profile));
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `${prefix}-${seed}.json`), JSON.stringify(trace));
	}
	console.error(`${count} ${profile} battles, ${MOVES.length} plain moves, ${SPECIES.length} species, ` +
		`${turns} turns, ${draws} draws, ${truncated} truncated`);
}

module.exports = { makeScenario, isPlainMove, MOVES, SPECIES };
