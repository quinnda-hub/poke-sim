#!/usr/bin/env node
'use strict';
// Showdown throughput baseline, mirroring bench/Main.hs: random playouts with uniformly random legal choices, one core,
// teams from the first 20 trace files (sorted by name) of a trace set, or from its teams.json when it has one, counted in battles and decision steps per CPU
// second. A decision step is one decision point at which at least one side chooses (the engine's `step`).
//
// Usage: node tools/bench/showdown-bench.js <repoRoot> <traceDir> [seconds=5] [warmupSeconds=2]
// Compare with `POKESIM_TRACES=<traceDir> cabal bench`, on the same pinned core (taskset), alternating runs.
const fs = require('fs');
const path = require('path');

const [repo, traceDir, secsArg, warmArg] = process.argv.slice(2);
// Absolute, so a relative repo root (`.`) is not taken for a package name by require.
const SHOWDOWN_DIR = path.resolve(repo, 'local', 'showdown');
const { Battle, PRNG } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const secs = Number(secsArg || 5);
const warm = Number(warmArg || 2);

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

// Copied from tools/oracle/record.js legalChoices (no Terastallization, as in the engine's bench teams).
function legalChoices(request) {
	if (!request || request.wait) return [];
	if (request.teamPreview) return ['team'];
	const mons = request.side.pokemon;
	const switches = [];
	for (let i = 0; i < mons.length; i++) {
		if (mons[i].active) continue;
		if (mons[i].condition.endsWith(' fnt')) continue;
		switches.push(`switch ${i + 1}`);
	}
	if (request.forceSwitch) return request.forceSwitch[0] ? switches : [];
	const active = request.active[0];
	const out = [];
	active.moves.forEach((m, i) => {
		if (m.disabled) return;
		if (m.pp !== undefined && m.pp <= 0) return;
		out.push(`move ${i + 1}`);
	});
	if (!active.trapped) out.push(...switches);
	return out;
}

// A set packed by trace-pack has teams.json (an array of {trace, teams}, in name order): its first 20 entries are the
// same matchups as the first 20 trace files.
const packed = path.join(traceDir, 'teams.json');
let matchups;
if (fs.existsSync(packed)) {
	matchups = JSON.parse(fs.readFileSync(packed, 'utf8')).slice(0, 20).map(e => e.teams);
} else {
	const names = fs.readdirSync(traceDir).filter(n => n.endsWith('.json') && !n.endsWith('-moves.json'))
		.map(n => path.join(traceDir, n)).sort().slice(0, 20);
	matchups = names.map(f => JSON.parse(fs.readFileSync(f, 'utf8')).teams);
}
if (!matchups.length) throw new Error(`no trace files in ${traceDir}`);

const policy = mulberry32(20240601);
let battleIndex = 0;

function playout(teams) {
	const i = battleIndex++;
	const hex = (i + 1).toString(16).padStart(16, '0');
	const battle = new Battle({ formatid: 'gen9ou', prng: new PRNG(`gen5,${hex}`) });
	battle.setPlayer('p1', { name: 'P1', team: teams[0] });
	battle.setPlayer('p2', { name: 'P2', team: teams[1] });
	let steps = 0;
	while (!battle.ended) {
		const chosen = battle.sides.map(s => {
			const legal = legalChoices(s.activeRequest);
			if (!legal.length) return null;
			if (legal[0] === 'team') {
				const n = s.pokemon.length, order = Array.from({ length: n }, (_, k) => k + 1);
				for (let k = n - 1; k > 0; k--) { const j = Math.floor(policy() * (k + 1)); [order[k], order[j]] = [order[j], order[k]]; }
				return `team ${order.join('')}`;
			}
			return legal[Math.floor(policy() * legal.length)];
		});
		steps++;
		battle.sides.forEach((s, k) => {
			if (chosen[k] !== null && !battle.choose(s.id, chosen[k])) throw new Error(`rejected ${chosen[k]}`);
		});
	}
	return steps;
}

function cpuSeconds() { const u = process.cpuUsage(); return (u.user + u.system) / 1e6; }

function run(budget) {
	const start = cpuSeconds();
	let battles = 0, steps = 0;
	while (cpuSeconds() - start < budget) {
		steps += playout(matchups[battles % matchups.length]);
		battles++;
	}
	return { battles, steps, cpu: cpuSeconds() - start };
}

if (warm > 0) run(warm);
const r = run(secs);
console.log(`showdown random playouts, 1 core, ${matchups.length} trace team pairs from ${traceDir}, warm-up ${warm}s`);
console.log(`battles: ${r.battles}, decision steps: ${r.steps}, cpu: ${r.cpu.toFixed(2)}s`);
console.log(`battles/s: ${(r.battles / r.cpu).toFixed(1)}   decision-steps/s: ${(r.steps / r.cpu).toFixed(0)}   steps/battle: ${(r.steps / r.battles).toFixed(1)}`);
