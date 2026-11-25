#!/usr/bin/env node
'use strict';
// Builds the usage whitelist: the species, moves, abilities, items and Tera types
// that matter in Smogon Gen 9 OU, from Smogon's monthly "chaos" statistics.
//
// Usage:
//   node gen-whitelist.js <YYYY-MM> [outFile=<repo>/data/usage/gen9ou.json]
//
// Stats are cached under local/usage/<YYYY-MM>/gen9ou-1695.json (downloaded if missing).
// Policy: the file reflects the latest month only; old months live in git history.
// Regenerating prints what was added and dropped against the existing outFile.
// Implemented mechanics are never removed just because an entry drops out.
//
// Cuts: a species needs weighted usage >= SPECIES_MIN; an ability, item, move or Tera
// type is kept for it when it is on >= ENTRY_MIN of that species' sets (entry weight
// divided by the sum of the species' Abilities weights; every set has one ability).
//
// Bans: entities banned by the pinned Showdown's own gen9ou rule table are moved to
// "excluded". Only single-entity bans are checked (species, move, item, ability);
// combination bans (e.g. complex team bans, Baton Pass clauses) are NOT checked.

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..', '..');
const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR || path.join(ROOT, 'local', 'showdown');
const { Dex, toID } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));

const RATING = 1695;
const FORMAT = 'gen9ou';
const SPECIES_MIN = 0.005;
const ENTRY_MIN = 0.01;

const round4 = x => Math.round(x * 10000) / 10000;
const sortedObj = o => Object.fromEntries(Object.entries(o).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)));
const die = msg => { console.error(`error: ${msg}`); process.exit(1); };

async function loadStats(month) {
	const file = path.join(ROOT, 'local', 'usage', month, `${FORMAT}-${RATING}.json`);
	const url = `https://www.smogon.com/stats/${month}/chaos/${FORMAT}-${RATING}.json`;
	if (!fs.existsSync(file)) {
		const res = await fetch(url, { headers: { 'User-Agent': 'usage-whitelist-tool/1.0' } });
		if (!res.ok) die(`download of ${url} failed: HTTP ${res.status}`);
		fs.mkdirSync(path.dirname(file), { recursive: true });
		fs.writeFileSync(file, await res.text());
	}
	return { stats: JSON.parse(fs.readFileSync(file, 'utf8')), url };
}

function loadData(name) {
	return JSON.parse(fs.readFileSync(path.join(ROOT, 'data', 'gen9', name), 'utf8'));
}

async function main() {
	const month = process.argv[2];
	if (!/^\d{4}-\d{2}$/.test(month || '')) die('usage: gen-whitelist.js <YYYY-MM> [outFile]');
	const outFile = path.resolve(process.argv[3] || path.join(ROOT, 'data', 'usage', 'gen9ou.json'));
	const { stats, url } = await loadStats(month);

	const dexData = {
		species: loadData('species.json'), moves: loadData('moves.json'),
		items: loadData('items.json'), abilities: loadData('abilities.json'),
	};
	const typeIds = new Set(loadData('types.json').names.map(toID));
	const meta0 = loadData('meta.json');

	const dex = Dex.mod('gen9');
	const rules = dex.formats.getRuleTable(dex.formats.get(FORMAT));
	const excluded = { species: {}, moves: {}, items: {}, abilities: {} };

	// Resolve and ban-check an id, failing loudly if it is unknown.
	function resolve(kind, id, display) {
		if (!(id in dexData[kind])) die(`${kind} "${display}" (id "${id}") does not resolve in data/gen9/${kind}.json`);
	}
	const banReason = (kind, id) => {
		const prefix = { moves: 'move', items: 'item', abilities: 'ability' }[kind];
		return rules.isBanned(`${prefix}:${id}`) ? `banned in ${FORMAT} (-${prefix}:${id})` : null;
	};
	function speciesBan(id) {
		const sp = dex.species.get(id);
		if (!sp.exists) die(`species id "${id}" unknown to Showdown`);
		if (!rules.isBannedSpecies(sp)) return null;
		return `banned in ${FORMAT} (tier ${sp.tier})`;
	}

	const species = {};
	for (const [name, e] of Object.entries(stats.data)) {
		if (!(e.usage >= SPECIES_MIN)) continue;
		const sid = toID(name);
		resolve('species', sid, name);
		const why = speciesBan(sid);
		if (why) { excluded.species[sid] = { usage: round4(e.usage), reason: why }; continue; }
		const sets = Object.values(e.Abilities).reduce((a, b) => a + b, 0);
		const pick = (table, kind) => {
			const out = {};
			for (const [k, w] of Object.entries(table)) {
				if (k === '' || (kind === 'items' && k === 'nothing')) continue;
				const share = w / sets;
				if (share < ENTRY_MIN) continue;
				if (kind === 'teraTypes') {
					if (!typeIds.has(k)) die(`Tera type "${k}" does not resolve in data/gen9/types.json`);
					out[k] = round4(share);
					continue;
				}
				resolve(kind, k, k);
				const r = banReason(kind, k);
				if (r) {
					const x = excluded[kind][k] || (excluded[kind][k] = { reason: r, species: [] });
					x.species.push(sid);
					continue;
				}
				out[k] = round4(share);
			}
			return sortedObj(out);
		};
		species[sid] = {
			usage: round4(e.usage),
			abilities: pick(e.Abilities, 'abilities'),
			items: pick(e.Items, 'items'),
			moves: pick(e.Moves, 'moves'),
			teraTypes: pick(e['Tera Types'], 'teraTypes'),
		};
		// Keep shares unrounded for the weights below.
		species[sid]._raw = { sets, e };
	}

	// Metagame weight = sum over whitelisted species of usage * share (from rounded shares
	// would drift with rounding; use the exact ones).
	const unions = { moves: {}, items: {}, abilities: {}, teraTypes: {} };
	const statKey = { abilities: 'Abilities', items: 'Items', moves: 'Moves', teraTypes: 'Tera Types' };
	for (const [sid, s] of Object.entries(species)) {
		const { sets, e } = s._raw;
		for (const cat of Object.keys(unions)) {
			for (const id of Object.keys(s[cat])) {
				unions[cat][id] = (unions[cat][id] || 0) + e.usage * (e[statKey[cat]][id] / sets);
			}
		}
		delete s._raw;
	}
	for (const x of Object.values(excluded)) for (const v of Object.values(x)) if (v.species) v.species.sort();

	const result = {
		meta: {
			month, metagame: FORMAT, ratingCutoff: RATING, battles: stats.info['number of battles'],
			speciesMinUsage: SPECIES_MIN, entryMinShare: ENTRY_MIN, source: url,
			showdown: { version: meta0.version, commit: meta0.commit },
		},
		species: sortedObj(species),
		moves: sortedObj(Object.fromEntries(Object.entries(unions.moves).map(([k, v]) => [k, round4(v)]))),
		items: sortedObj(Object.fromEntries(Object.entries(unions.items).map(([k, v]) => [k, round4(v)]))),
		abilities: sortedObj(Object.fromEntries(Object.entries(unions.abilities).map(([k, v]) => [k, round4(v)]))),
		teraTypes: sortedObj(Object.fromEntries(Object.entries(unions.teraTypes).map(([k, v]) => [k, round4(v)]))),
		excluded: {
			species: sortedObj(excluded.species), moves: sortedObj(excluded.moves),
			items: sortedObj(excluded.items), abilities: sortedObj(excluded.abilities),
		},
	};

	let prev = null;
	try { prev = JSON.parse(fs.readFileSync(outFile, 'utf8')); } catch (e) { /* none */ }
	fs.mkdirSync(path.dirname(outFile), { recursive: true });
	fs.writeFileSync(outFile, JSON.stringify(result, null, 2) + '\n');

	const cats = ['species', 'moves', 'items', 'abilities', 'teraTypes'];
	console.log(`${FORMAT} ${month} (${result.meta.battles} battles, cutoff ${RATING}) -> ${outFile}`);
	for (const c of cats) console.log(`  ${c}: ${Object.keys(result[c]).length}`);
	for (const c of Object.keys(result.excluded)) {
		const ids = Object.keys(result.excluded[c]);
		if (ids.length) console.log(`  excluded ${c}: ${ids.join(', ')}`);
	}
	if (prev) {
		for (const c of cats) {
			const a = new Set(Object.keys(prev[c] || {})), b = new Set(Object.keys(result[c]));
			const added = [...b].filter(x => !a.has(x)), dropped = [...a].filter(x => !b.has(x));
			if (added.length) console.log(`  + ${c}: ${added.join(', ')}`);
			if (dropped.length) console.log(`  - ${c}: ${dropped.join(', ')}`);
		}
	} else {
		console.log('  (no previous file to diff against)');
	}
}

main().catch(e => die(e.stack || e));
