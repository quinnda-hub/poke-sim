#!/usr/bin/env node
'use strict';
// Oracle trace recorder.
//
// Plays one battle on a pinned Pokemon Showdown build and records everything
// another engine needs to reproduce it: the teams, every random draw in order,
// the choices made at each decision point, the legal choices offered, and a
// snapshot of the game state at each decision point.
//
// Usage:
//   node record.js <scenario.json> [out.json]      (stdout when out is omitted)
//
// Scenario:
//   { "format": "gen9ou",
//     "seed": "gen5,0001000200030004",   // Showdown PRNG seed
//     "policySeed": 1,                    // seed for the random-choice policy
//     "maxTurns": 300,
//     "teams": [[set, ...], [set, ...]],  // Showdown PokemonSet JSON
//     "choices": [["team 123456", "team 123456"], ["move 1", "switch 2"], ...] }
//
// "choices" is optional. When present it is used first, one pair per decision
// point (null for a side that is not asked); once exhausted, the random policy
// takes over.
//
// The Showdown checkout is found via $SHOWDOWN_DIR, else <repo>/local/showdown.

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Battle, PRNG } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));

const TRACE_VERSION = 1;

/** A PRNG that records every draw. All Showdown randomness goes through random(). */
class RecordingPRNG extends PRNG {
	constructor(seed, sink) {
		super(seed);
		this.sink = sink;
	}
	random(from, to) {
		const result = super.random(from, to);
		// Normalise to a half-open integer range [lo, hi), matching PRNG.random:
		//   random()      -> real in [0,1): recorded as lo=0, hi=0 (not expected in battles)
		//   random(n)     -> [0, n)
		//   random(m, n)  -> [m, n)
		let lo = 0, hi = 0;
		if (from !== undefined) {
			const f = Math.floor(from);
			if (!to) { hi = f; } else { lo = f; hi = Math.floor(to); }
		}
		this.sink.push([lo, hi, result]);
		return result;
	}
}

/** Small deterministic generator for the choice policy (independent of the battle's PRNG). */
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

/** Legal choice strings for one side, derived from Showdown's request object. */
function legalChoices(request, opts) {
	if (!request || request.wait) return [];
	if (request.teamPreview) return ['team'];
	const mons = request.side.pokemon;
	const switches = [];
	for (let i = 0; i < mons.length; i++) {
		if (mons[i].active) continue;
		if (mons[i].condition.endsWith(' fnt')) continue;
		switches.push(`switch ${i + 1}`);
	}
	if (request.forceSwitch) {
		return request.forceSwitch[0] ? switches : [];
	}
	const active = request.active[0];
	const out = [];
	active.moves.forEach((m, i) => {
		if (m.disabled) return;
		if (m.pp !== undefined && m.pp <= 0) return;
		out.push(`move ${i + 1}`);
		if (opts.tera && active.canTerastallize) out.push(`move ${i + 1} terastallize`);
	});
	if (!active.trapped) out.push(...switches);
	return out;
}

function snapshotPokemon(p, origIndex) {
	const volatiles = {};
	for (const id of Object.keys(p.volatiles).sort()) {
		const v = p.volatiles[id];
		const entry = {};
		for (const k of ['duration', 'time', 'startTime', 'counter', 'layers', 'hp', 'move', 'stage']) {
			if (v[k] !== undefined) entry[k] = v[k];
		}
		volatiles[id] = entry;
	}
	const statusState = {};
	for (const k of ['duration', 'time', 'startTime', 'stage', 'counter']) {
		if (p.statusState[k] !== undefined) statusState[k] = p.statusState[k];
	}
	return {
		orig: origIndex,
		species: p.species.id,
		level: p.level,
		gender: p.gender,
		hp: p.hp,
		maxhp: p.maxhp,
		status: p.status,
		statusState,
		fainted: p.fainted,
		active: p.isActive,
		stats: { ...p.storedStats },
		boosts: { ...p.boosts },
		speed: p.speed,
		types: p.getTypes(),
		teraType: p.teraType,
		terastallized: p.terastallized || '',
		ability: p.ability,
		item: p.item,
		moves: p.moveSlots.map(m => ({ id: m.id, pp: m.pp, maxpp: m.maxpp, disabled: !!m.disabled })),
		volatiles,
	};
}

function conditionMap(conds) {
	const out = {};
	for (const id of Object.keys(conds).sort()) {
		const c = conds[id];
		const entry = {};
		for (const k of ['duration', 'layers']) if (c[k] !== undefined) entry[k] = c[k];
		out[id] = entry;
	}
	return out;
}

function snapshot(battle, origOf) {
	return {
		turn: battle.turn,
		request: battle.requestState,
		ended: battle.ended,
		winner: battle.ended ? (battle.winner ? battle.sides.find(s => s.name === battle.winner).id : 'tie') : null,
		field: {
			weather: battle.field.weather,
			weatherDuration: battle.field.weatherState.duration ?? null,
			terrain: battle.field.terrain,
			terrainDuration: battle.field.terrainState.duration ?? null,
			pseudoWeather: conditionMap(battle.field.pseudoWeather),
		},
		// Pokemon are listed in Showdown's current slot order (the order `switch N` indexes).
		sides: battle.sides.map(s => ({
			id: s.id,
			pokemonLeft: s.pokemonLeft,
			sideConditions: conditionMap(s.sideConditions),
			pokemon: s.pokemon.map(p => snapshotPokemon(p, origOf.get(p))),
		})),
	};
}

function record(scenario) {
	const draws = [];
	const prng = new RecordingPRNG(scenario.seed, draws);
	const battle = new Battle({ formatid: scenario.format, prng });
	const policy = mulberry32(scenario.policySeed ?? 1);
	const opts = { tera: !!scenario.tera };
	const maxTurns = scenario.maxTurns ?? 300;
	const scripted = (scenario.choices || []).slice();

	battle.setPlayer('p1', { name: 'P1', team: scenario.teams[0] });
	battle.setPlayer('p2', { name: 'P2', team: scenario.teams[1] });

	const origOf = new Map();
	for (const side of battle.sides) side.pokemon.forEach((p, i) => origOf.set(p, i));

	// Each step: the draws consumed since the previous decision point, then the
	// state at this decision point, the legal choices, and the choices taken.
	const steps = [];
	let drawCursor = 0;
	let truncated = false;

	const pickTeam = (side) => {
		const n = side.pokemon.length;
		const order = Array.from({ length: n }, (_, i) => i + 1);
		for (let i = n - 1; i > 0; i--) {
			const j = Math.floor(policy() * (i + 1));
			[order[i], order[j]] = [order[j], order[i]];
		}
		return `team ${order.join('')}`;
	};

	while (!battle.ended) {
		if (battle.turn > maxTurns) { truncated = true; break; }
		const newDraws = draws.slice(drawCursor);
		drawCursor = draws.length;
		const state = snapshot(battle, origOf);
		const legal = battle.sides.map(s => legalChoices(s.activeRequest, opts));
		const script = scripted.shift();
		const chosen = battle.sides.map((s, i) => {
			if (legal[i].length === 0) return null;
			if (script && script[i]) return script[i];
			if (legal[i][0] === 'team') return pickTeam(s);
			return legal[i][Math.floor(policy() * legal[i].length)];
		});
		steps.push({ draws: newDraws, state, legal, chosen });
		battle.sides.forEach((s, i) => {
			if (chosen[i] === null) return;
			if (!battle.choose(s.id, chosen[i])) {
				throw new Error(`Showdown rejected ${s.id} "${chosen[i]}": ${s.choice.error}`);
			}
		});
	}

	return {
		traceVersion: TRACE_VERSION,
		showdownVersion: require(path.join(SHOWDOWN_DIR, 'package.json')).version,
		format: scenario.format,
		seed: scenario.seed,
		policySeed: scenario.policySeed ?? 1,
		teams: scenario.teams,
		steps,
		truncated,
		// Draws after the last decision point, and the final state.
		finalDraws: draws.slice(drawCursor),
		final: snapshot(battle, origOf),
	};
}

module.exports = { record };

if (require.main === module) {
	const [scenarioPath, outPath] = process.argv.slice(2);
	if (!scenarioPath) {
		console.error('usage: node record.js <scenario.json> [out.json]');
		process.exit(2);
	}
	const scenario = JSON.parse(fs.readFileSync(scenarioPath, 'utf8'));
	const trace = JSON.stringify(record(scenario));
	if (outPath) fs.writeFileSync(outPath, trace); else process.stdout.write(trace + '\n');
}
