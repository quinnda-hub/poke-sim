#!/usr/bin/env node
'use strict';
// Exports static game data from the pinned Pokemon Showdown build, so the
// engine's tables come from the same source the engine is tested against.
//
// Usage:
//   node export-dex.js [outDir=<repo>/data/gen9] [mod=gen9]
//
// Writes species.json, moves.json, abilities.json, items.json, conditions.json,
// types.json, natures.json and meta.json. Every entry keeps Showdown's declarative fields.
// Behaviour that Showdown implements as handler code cannot be exported as
// data; for those entries the names of the handlers are listed under
// "handlers", which is the to-do list for hand-written rules.

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));

const DROP = new Set([
	'desc', 'shortDesc', 'contestType', 'zMove', 'maxMove', 'fullname', 'effectType',
	'exists', 'sourceEffect', 'rating', 'spritenum', 'color', 'heightm', 'eggGroups',
	'cosmeticFormes', 'otherFormes', 'formeOrder', 'canHatch', 'evos', 'prevo', 'evoType',
	'evoLevel', 'evoItem', 'evoMove', 'evoCondition', 'evoRegion', 'nfe', 'spriteid',
	'bst', 'fling', 'naturalGift',
]);

/** Declarative fields as JSON, plus the sorted names of any handler functions. */
function exportEntry(obj) {
	const out = {};
	const handlers = [];
	for (const [k, v] of Object.entries(obj)) {
		if (DROP.has(k) || v === undefined) continue;
		if (typeof v === 'function') { handlers.push(k); continue; }
		out[k] = exportValue(v, `${k}.`, handlers);
	}
	if (handlers.length) out.handlers = handlers.sort();
	return out;
}

function exportValue(v, prefix, handlers) {
	if (v === null || typeof v !== 'object') return v;
	if (Array.isArray(v)) return v.map((x, i) => exportValue(x, `${prefix}${i}.`, handlers));
	const out = {};
	for (const [k, x] of Object.entries(v)) {
		if (x === undefined) continue;
		if (typeof x === 'function') { handlers.push(prefix + k); continue; }
		out[k] = exportValue(x, `${prefix}${k}.`, handlers);
	}
	return out;
}

function byId(entries) {
	const out = {};
	for (const e of entries.slice().sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
		out[e.id] = exportEntry(e);
	}
	return out;
}

function main() {
	const outDir = process.argv[2] || path.resolve(__dirname, '..', '..', 'data', 'gen9');
	const mod = process.argv[3] || 'gen9';
	const dex = Dex.mod(mod);
	fs.mkdirSync(outDir, { recursive: true });
	const write = (name, value) =>
		fs.writeFileSync(path.join(outDir, name), JSON.stringify(value, null, 1) + '\n');

	const typeNames = dex.types.names();
	// chart[defending][attacking] uses Showdown's codes:
	//   0 = neutral, 1 = super effective, 2 = resisted, 3 = immune.
	const chart = {};
	for (const def of typeNames) {
		const taken = dex.types.get(def).damageTaken;
		chart[def] = typeNames.map(atk => taken[atk] ?? 0);
	}

	// The non-type keys of typechart `damageTaken` (brn, par, psn, tox, frz, powder, prankster, ...), by type:
	// the immunities `runStatusImmunity` / `getImmunity` read (value 3 = immune).
	const otherDamageTaken = {};
	for (const def of typeNames) {
		const taken = dex.types.get(def).damageTaken;
		const extra = {};
		for (const k of Object.keys(taken).sort()) if (!typeNames.includes(k)) extra[k] = taken[k];
		if (Object.keys(extra).length) otherDamageTaken[def] = extra;
	}

	// Conditions (data/conditions.ts, plus the conditions that moves, abilities and items define): the
	// statuses and volatiles whose handlers the dispatcher runs. `effectType` is kept here (it picks the
	// default subOrder in `resolvePriority`).
	const conditions = {};
	for (const id of Object.keys(dex.data.Conditions).sort()) {
		const c = dex.conditions.getByID(id);
		if (!c.exists) continue;
		conditions[id] = { ...exportEntry(c), effectType: c.effectType };
	}

	const natures = {};
	for (const n of dex.natures.all()) natures[n.id] = { name: n.name, plus: n.plus || null, minus: n.minus || null };

	write('species.json', byId(dex.species.all().filter(s => s.exists)));
	write('moves.json', byId(dex.moves.all().filter(m => m.exists)));
	write('abilities.json', byId(dex.abilities.all().filter(a => a.exists)));
	write('items.json', byId(dex.items.all().filter(i => i.exists)));
	write('conditions.json', conditions);
	write('types.json', { names: typeNames, damageTaken: chart, otherDamageTaken });
	write('natures.json', natures);
	write('meta.json', {
		source: 'pokemon-showdown',
		version: require(path.join(SHOWDOWN_DIR, 'package.json')).version,
		commit: readCommit(),
		mod,
	});
}

function readCommit() {
	try {
		const head = fs.readFileSync(path.join(SHOWDOWN_DIR, '.git', 'HEAD'), 'utf8').trim();
		if (!head.startsWith('ref:')) return head;
		return fs.readFileSync(path.join(SHOWDOWN_DIR, '.git', head.slice(5)), 'utf8').trim();
	} catch {
		return null;
	}
}

main();
