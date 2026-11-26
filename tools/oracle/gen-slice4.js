#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 4" (a move's own handler code; phase 4a) and records each on Showdown.
//
// Slice 4a lets in the moves whose own handlers (`basePowerCallback`, `damageCallback`, `onBasePower`, `onModifyMove`,
// `onModifyType`, `onEffectiveness`, `onModifyPriority`, `onTryHit`, `onPrepareHit`) the engine has bodies for. The teams
// are slice 3's (gen-slice3.js `assign`, over gen-slice1.js `makeScenario`, so the draws are those of slices 1 and 3 and
// weather and terrain setters, hazards, screens, Flying and other types come with them), then each Pokemon is changed by a
// generator of its own:
//   - most Pokemon carry one or two of the slice's moves (Low Kick, Grass Knot, Heavy Slam, Hex, Stored Power, Acrobatics,
//     Rising Voltage, Facade, Psyblade, Expanding Force, Fickle Beam, Thunder, Blizzard, Bleakwind Storm, Weather Ball, Terrain
//     Pulse, Ivy Cudgel, Freeze-Dry, Grassy Glide, Ruination, and the moves that share their bodies: Power Trip, Super Fang,
//     Venoshock, Barb Barrage, Brine, Flying Press);
//   - weights: a share of the Pokemon are among the heaviest or the lightest species of the pool, or weigh exactly a band edge
//     of Low Kick and Grass Knot (or one less), so that Low Kick, Grass Knot and Heavy Slam meet each of their bands (Heavy
//     Slam compares the two weights);
//   - statuses and boosts: a share carry a status move (Will-O-Wisp, Toxic, Thunder Wave, Glare) or a set-up move, and a
//     Flame Orb or Toxic Orb, so that Hex meets statused targets and Facade burned, poisoned and paralysed users, and Stored
//     Power and Power Trip have boosts;
//   - items: Acrobatics users hold nothing about half the time, else Booster Energy, a seed or another item;
//   - fields: weather and terrain moves and setters (slice 3's) come with the Pokemon, and a share are Water, Ice, Flying or
//     Poison types (Freeze-Dry and Water types, Terrain Pulse and Rising Voltage against ungrounded Pokemon, Venoshock);
//   - priority: Grassy Glide users sit beside priority moves, and in a mirror match the two sides tie on speed.
// The policy picks a random legal choice, switches included.
//
// Usage:
//   node gen-slice4.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice4-<seed>.json, plus slice4-moves.json, slice4-species.json, slice4-abilities.json and
// slice4-items.json (copies of the pool, which the parity test compares with what the engine accepts). A tally of the
// outcomes that distinguish each handler is printed to stderr; it is computed by wrapping Showdown's own methods
// (observation only: the wrappers pass every argument and result through and call nothing the battle would not call).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { BattleActions } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'battle-actions'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');
const { assign: assignSlice3 } = require('./gen-slice3.js');

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

// The moves of the slice, by what their body branches on.
const WEIGHT_MOVES = ['lowkick', 'grassknot', 'heavyslam'];
const OWN_MOVES = [
	...WEIGHT_MOVES, 'hex', 'storedpower', 'powertrip', 'acrobatics', 'risingvoltage', 'facade', 'psyblade', 'expandingforce',
	'ficklebeam', 'thunder', 'blizzard', 'bleakwindstorm', 'weatherball', 'terrainpulse', 'ivycudgel', 'freezedry',
	'grassyglide', 'ruination', 'superfang', 'venoshock', 'barbbarrage', 'brine', 'flyingpress',
];
// Listed in the design but rejected, so not in the pool: hurricane (its confusion secondary) and mistyexplosion
// (selfdestruct). They are structurally zero in the tally.
const REJECTED_MOVES = { hurricane: 'rejected: secondary volatileStatus confusion', mistyexplosion: 'rejected: selfdestruct (and no body)' };
const FREEZERS = ['freezedry', 'blizzard', 'icebeam', 'icywind'];
const STATUS_MOVES = ['willowisp', 'toxic', 'thunderwave', 'glare'];
const BOOST_MOVES = ['swordsdance', 'nastyplot', 'calmmind', 'dragondance', 'bulkup', 'quiverdance', 'agility', 'coil', 'workup', 'shellsmash'];
const WEATHER_MOVES = ['sunnyday', 'raindance', 'sandstorm', 'snowscape'];
const TERRAIN_MOVES = ['electricterrain', 'grassyterrain', 'mistyterrain', 'psychicterrain'];
const PRIORITY_MOVES = ['machpunch', 'quickattack', 'aquajet', 'iceshard', 'bulletpunch', 'shadowsneak', 'vacuumwave', 'extremespeed'];
const SEEDS = ['electricseed', 'grassyseed', 'mistyseed', 'psychicseed'];

function assign(scenario, pool, seed) {
	assignSlice3(scenario, pool, seed);
	const rnd = mulberry32(seed * 2246822519 + 3266489917);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const own = OWN_MOVES.filter(id => pool.moves.includes(id));
	const status = STATUS_MOVES.filter(id => pool.moves.includes(id));
	const boost = BOOST_MOVES.filter(id => pool.moves.includes(id));
	const priority = PRIORITY_MOVES.filter(id => pool.moves.includes(id));
	const keep = [...WEATHER_MOVES, ...TERRAIN_MOVES].filter(id => pool.moves.includes(id));
	const byWeight = pool.species.slice().sort((a, b) => dex.species.get(a).weighthg - dex.species.get(b).weighthg);
	const tenth = Math.max(1, Math.floor(byWeight.length / 10));
	const light = byWeight.slice(0, tenth), heavy = byWeight.slice(-tenth);
	// species whose weight (tenths of a kilogram) sits on a band edge of Low Kick and Grass Knot, or just under one: a band
	// compared with `>=` is only tested there
	const EDGES = [100, 250, 500, 1000, 2000];
	const edge = pool.species.filter(id => { const w = dex.species.get(id).weighthg; return EDGES.some(e => w === e || w === e - 1); });
	const bySpeciesType = (t) => pool.species.filter(id => dex.species.get(id).types.includes(t));
	const water = bySpeciesType('Water'), ice = bySpeciesType('Ice'), flying = bySpeciesType('Flying'), poison = bySpeciesType('Poison');
	const seeds = SEEDS.filter(id => pool.items.includes(id));
	const mirror = JSON.stringify(scenario.teams[0]) === JSON.stringify(scenario.teams[1]);
	const idOf = (name) => dex.moves.get(name).id;
	const set = (s) => {
		// weights: the extremes of the pool, so the bands of Low Kick and Heavy Slam are crossed
		const k = int(100);
		if (k < 14) s.species = dex.species.get(pick(heavy)).name;
		else if (k < 28) s.species = dex.species.get(pick(light)).name;
		else if (k < 38 && edge.length) s.species = dex.species.get(pick(edge)).name;
		else if (k < 46 && water.length) s.species = dex.species.get(pick(water)).name;
		else if (k < 52 && ice.length) s.species = dex.species.get(pick(ice)).name;
		else if (k < 58 && flying.length) s.species = dex.species.get(pick(flying)).name;
		else if (k < 62 && poison.length) s.species = dex.species.get(pick(poison)).name;
		// the moves of the slice, one or two; the weather and terrain moves slice 3 gave stay
		const n = int(100) < 70 ? (int(3) === 0 ? 2 : 1) : 0;
		const slots = [0, 1, 2, 3].filter(i => !keep.includes(idOf(s.moves[i])));
		const put = (id) => { if (slots.length && !s.moves.map(idOf).includes(id)) s.moves[slots.splice(int(slots.length), 1)[0]] = id; };
		for (let i = 0; i < n; i++) put(pick(own));
		if (status.length && int(100) < 26) put(pick(status));
		if (boost.length && int(100) < 26) put(pick(boost));
		const ids = s.moves.map(idOf);
		// Grassy Glide beside a priority move (equal priority under Grassy Terrain) and a terrain setter
		if (ids.includes('grassyglide')) {
			if (priority.length && int(100) < 70) put(pick(priority));
			if (pool.moves.includes('grassyterrain') && int(100) < 60) put('grassyterrain');
		}
		// Hex wants a frozen target now and then
		if (ids.includes('hex') && int(100) < 35) put(pick(FREEZERS.filter(id => pool.moves.includes(id))));
		// Terrain Pulse, Rising Voltage, Psyblade, Expanding Force: a terrain move for the Pokemon itself
		if (['terrainpulse', 'risingvoltage', 'psyblade', 'expandingforce'].some(m => ids.includes(m)) && int(100) < 45) {
			put(pick(TERRAIN_MOVES.filter(id => pool.moves.includes(id))));
		}
		// Weather Ball and the accuracy moves: a weather move
		if (['weatherball', 'thunder', 'blizzard', 'bleakwindstorm'].some(m => ids.includes(m)) && int(100) < 45) {
			put(pick(WEATHER_MOVES.filter(id => pool.moves.includes(id))));
		}
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
		const final = s.moves.map(idOf);
		// items: Facade wants a status (a burn is exempt from the halving); Acrobatics holds nothing about half the time
		if (final.includes('facade') && int(100) < 55) {
			const orbs = ['flameorb', 'toxicorb'].filter(id => pool.items.includes(id));
			if (orbs.length) s.item = dex.items.get(pick(orbs)).name;
		} else if (final.includes('acrobatics')) {
			const r = int(100);
			if (r < 50) s.item = '';
			else if (r < 70 && pool.items.includes('boosterenergy')) { s.item = dex.items.get('boosterenergy').name; if (int(2) === 0) s.ability = dex.abilities.get(pick(['protosynthesis', 'quarkdrive'].filter(id => pool.abilities.includes(id)))).name; }
			else if (r < 85 && seeds.length) s.item = dex.items.get(pick(seeds)).name;
		} else if (int(100) < 12) {
			const orbs = ['flameorb', 'toxicorb'].filter(id => pool.items.includes(id));
			if (orbs.length) s.item = dex.items.get(pick(orbs)).name;
		}
	};
	// a mirror match keeps its two teams identical (the two sides tie on speed and priority)
	for (const s of scenario.teams[0]) set(s);
	// a Grassy Glide duel in about one battle in eight: a mirror match whose lead sets Grassy Terrain and carries Grassy Glide
	// and a priority move, so the two leads tie on speed at the priority Grassy Terrain gives them (an equal-priority tie
	// is shuffled by a draw)
	const duel = pool.moves.includes('grassyglide') && pool.abilities.includes('grassysurge') && priority.length && int(100) < 13;
	if (duel) {
		const lead = scenario.teams[0][0];
		lead.ability = dex.abilities.get('grassysurge').name;
		lead.moves = lead.moves.slice();
		lead.moves[0] = 'grassyglide';
		lead.moves[1] = pick(priority);
		lead.moves = [...new Set(lead.moves)];
		while (lead.moves.length < 4) { const m = pick(pool.moves); if (!lead.moves.includes(m)) lead.moves.push(m); }
		scenario.choices = [['team 123456', 'team 123456']];
	}
	if (mirror || duel) scenario.teams[1] = JSON.parse(JSON.stringify(scenario.teams[0]));
	else for (const s of scenario.teams[1]) set(s);
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally: observation hooks on Showdown's classes. Nothing is changed: each wrapper calls the original with every argument
// and returns its result.

let activeTally = null;
const BANDS = [20, 40, 60, 80, 100, 120];
function newTally() {
	const z = (keys) => Object.fromEntries(keys.map(k => [k, 0]));
	return {
		moveUsed: z(OWN_MOVES),
		structurallyZero: z(Object.keys(REJECTED_MOVES)),
		// basePowerCallback
		lowKickBand: z(BANDS), grassKnotBand: z(BANDS), heavySlamBand: z([40, 60, 80, 100, 120]),
		hexOnStatusedTarget: z(['brn', 'par', 'psn', 'tox', 'frz']), hexOnCleanTarget: 0,
		storedPowerBoosted: 0, storedPowerUnboosted: 0, powerTripBoosted: 0, powerTripUnboosted: 0,
		acrobaticsWithItem: 0, acrobaticsNoItem: 0, acrobaticsItemUsedUp: 0,
		risingVoltageGroundedTarget: 0, risingVoltageUngroundedTarget: 0, risingVoltageNoTerrain: 0, risingVoltageOtherTerrain: 0,
		// onBasePower
		facadeUserStatus: z(['brn', 'par', 'psn', 'tox']), facadeNoStatus: 0,
		psybladeElectricTerrain: 0, psybladeOtherTerrain: 0,
		expandingForceGrounded: 0, expandingForceUngrounded: 0, expandingForceNoTerrain: 0,
		fickleBeamBoosted: 0, fickleBeamPlain: 0, fickleBeamBoostedWithAnotherModifier: 0, fickleBeamPlainWithAnotherModifier: 0,
		venoshockPoisoned: 0, venoshockClean: 0, barbBarragePoisoned: 0, brineHalfHp: 0, brineAboveHalf: 0,
		// onModifyMove (accuracy) and onModifyType
		thunderAccuracy: z(['rain', 'sun', 'other']), blizzardAccuracy: z(['snow', 'other']), bleakwindAccuracy: z(['rain', 'other']),
		weatherBallType: z(['Normal', 'Fire', 'Water', 'Rock', 'Ice']), weatherBallDoubled: 0,
		terrainPulseType: z(['Normal', 'Electric', 'Grass', 'Fairy', 'Psychic']), terrainPulseDoubled: 0,
		terrainPulseUngroundedInTerrain: 0,
		ivyCudgelUsed: 0,
		// onEffectiveness
		freezeDryVsWaterMono: 0, freezeDryVsWaterDualType: 0, freezeDryVsOther: 0, flyingPressDualEffect: 0,
		// onModifyPriority
		grassyGlideInTerrain: 0, grassyGlideOutOfTerrain: 0, grassyGlideUngroundedInTerrain: 0,
		grassyGlideSamePriorityAsAnotherMove: 0, grassyGlideTieOnSpeedAndPriority: 0,
		// damageCallback
		ruinationUsed: 0, superFangUsed: 0, halfHpTargetAtOneHp: 0,
		// structurally zero: no species the engine has is an Ogerpon forme (a species with `forme` is not in the pool), so
		// Ivy Cudgel never changes type; a frozen user cannot use Facade (it has no defrost flag), so there is no Facade counter
		// for a frozen user
		ivyCudgelTypeChanged: 0, facadeByFrozenUser: 0,
	};
}

function install() {
	if (Battle.prototype.__slice4Instrumented) return;
	Battle.prototype.__slice4Instrumented = true;
	const t = () => activeTally;
	const grounded = (p) => !p.types.includes('Flying'); // the pool has no Levitate, Air Balloon, Iron Ball or Gravity
	const terrainOf = (b) => b.field.terrain;

	const useMove = BattleActions.prototype.useMove;
	BattleActions.prototype.useMove = function (move, pokemon, ...rest) {
		const c = t();
		const id = typeof move === 'string' ? dex.toID(move) : move.id;
		if (c && id in c.moveUsed) c.moveUsed[id]++;
		if (c && id in REJECTED_MOVES) c.structurallyZero[id]++;
		if (c && id === 'ivycudgel') c.ivyCudgelUsed++;
		return useMove.call(this, move, pokemon, ...rest);
	};

	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, ...rest) {
		const c = t();
		if (c && eventid === 'BasePower' && rest[0] && rest[0].id && target && source && typeof rest[1] === 'number') {
			// getDamage: runEvent('BasePower', attacker, defender, move, basePower, true)
			const move = rest[0], atk = target, def = source, bp = rest[1];
			const terrain = terrainOf(this);
			switch (move.id) {
			case 'lowkick': c.lowKickBand[bp]++; break;
			case 'grassknot': c.grassKnotBand[bp]++; break;
			case 'heavyslam': c.heavySlamBand[bp]++; break;
			case 'hex': if (def.status) c.hexOnStatusedTarget[def.status]++; else c.hexOnCleanTarget++; break;
			case 'storedpower': if (bp > 20) c.storedPowerBoosted++; else c.storedPowerUnboosted++; break;
			case 'powertrip': if (bp > 20) c.powerTripBoosted++; else c.powerTripUnboosted++; break;
			case 'acrobatics':
				if (atk.item) c.acrobaticsWithItem++; else { c.acrobaticsNoItem++; if (atk.lastItem) c.acrobaticsItemUsedUp++; }
				break;
			case 'risingvoltage':
				if (terrain === 'electricterrain') { if (grounded(def)) c.risingVoltageGroundedTarget++; else c.risingVoltageUngroundedTarget++; }
				else if (terrain) c.risingVoltageOtherTerrain++; else c.risingVoltageNoTerrain++;
				break;
			case 'facade':
				if (atk.status === 'frz') c.facadeByFrozenUser++;
				else if (atk.status) c.facadeUserStatus[atk.status]++; else c.facadeNoStatus++;
				break;
			case 'psyblade': if (terrain === 'electricterrain') c.psybladeElectricTerrain++; else c.psybladeOtherTerrain++; break;
			case 'expandingforce':
				if (terrain === 'psychicterrain') { if (grounded(atk)) c.expandingForceGrounded++; else c.expandingForceUngrounded++; }
				else c.expandingForceNoTerrain++;
				break;
			case 'venoshock': if (def.status === 'psn' || def.status === 'tox') c.venoshockPoisoned++; else c.venoshockClean++; break;
			case 'barbbarrage': if (def.status === 'psn' || def.status === 'tox') c.barbBarragePoisoned++; break;
			case 'brine': if (def.hp * 2 <= def.maxhp) c.brineHalfHp++; else c.brineAboveHalf++; break;
			case 'weatherball': c.weatherBallType[move.type]++; if (bp === 100) c.weatherBallDoubled++; break;
			case 'terrainpulse':
				c.terrainPulseType[move.type]++;
				if (bp === 100) c.terrainPulseDoubled++;
				if (terrain && !grounded(atk)) c.terrainPulseUngroundedInTerrain++;
				break;
			case 'grassyglide':
				if (terrain === 'grassyterrain') { if (grounded(atk)) c.grassyGlideInTerrain++; else c.grassyGlideUngroundedInTerrain++; }
				else c.grassyGlideOutOfTerrain++;
				break;
			case 'ficklebeam': {
				const r = runEvent.call(this, eventid, target, source, ...rest);
				// the doubling is the move's: another handler (a type item, Technician, a terrain) changed the result too when it is not 2x
				if (r > bp) { c.fickleBeamBoosted++; if (r !== bp * 2) c.fickleBeamBoostedWithAnotherModifier++; }
				else { c.fickleBeamPlain++; if (r !== bp) c.fickleBeamPlainWithAnotherModifier++; }
				return r;
			}
			default:
			}
		}
		if (c && eventid === 'ModifyAccuracy' && rest[0] && rest[0].id) {
			// hitStepAccuracy: runEvent('ModifyAccuracy', target, pokemon, move, accuracy)
			const w = this.field.effectiveWeather();
			switch (rest[0].id) {
			case 'thunder': c.thunderAccuracy[w === 'raindance' ? 'rain' : w === 'sunnyday' ? 'sun' : 'other']++; break;
			case 'blizzard': c.blizzardAccuracy[w === 'snowscape' || w === 'hail' ? 'snow' : 'other']++; break;
			case 'bleakwindstorm': c.bleakwindAccuracy[w === 'raindance' ? 'rain' : 'other']++; break;
			default:
			}
		}
		return runEvent.call(this, eventid, target, source, ...rest);
	};

	// pokemon.runEffectiveness: singleEvent('Effectiveness', move, null, pokemon, type, move, typeMod)
	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, source, ...rest) {
		const c = t();
		if (c && eventid === 'Effectiveness' && effect && target) {
			if (effect.id === 'freezedry') {
				if (source === 'Water') { if (target.types.length === 2) c.freezeDryVsWaterDualType++; else c.freezeDryVsWaterMono++; } else c.freezeDryVsOther++;
			} else if (effect.id === 'flyingpress' && target.types.length === 2) c.flyingPressDualEffect++;
		}
		return singleEvent.call(this, eventid, effect, state, target, source, ...rest);
	};

	// getDamage: a damageCallback skips the crit roll and the damage modifiers
	const getDamage = BattleActions.prototype.getDamage;
	BattleActions.prototype.getDamage = function (source, target, move, ...rest) {
		const c = t();
		if (c && move && move.damageCallback) {
			if (move.id === 'ruination') c.ruinationUsed++; else if (move.id === 'superfang') c.superFangUsed++;
			if (target.hp === 1) c.halfHpTargetAtOneHp++;
		}
		return getDamage.call(this, source, target, move, ...rest);
	};

	// the move actions of a turn: Grassy Glide at the priority of another move, and tied on speed
	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, ...rest) {
		const c = t();
		if (c && list.length === 2 && list[0] && list[0].choice === 'move' && list[1].choice === 'move') {
			const [a, b] = list;
			if ((a.move.id === 'grassyglide' || b.move.id === 'grassyglide') && a.priority === b.priority) {
				c.grassyGlideSamePriorityAsAnotherMove++;
				if (a.speed === b.speed) c.grassyGlideTieOnSpeedAndPriority++;
			}
		}
		return speedSort.call(this, list, ...rest);
	};
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice4.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice4-${k}.json`), JSON.stringify(pool[k]));
	}
	const tally = newTally();
	activeTally = tally;
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		const trace = record(scenario);
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		// a battle cut off at the turn limit is still a trace: the parity test checks it up to the cut
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice4-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice4 seeds, ${written} traces written, ${truncated} truncated at the turn limit, ${pool.moves.length} moves, ${pool.species.length} species, ` +
		`${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error('tally (from Showdown\'s own events):');
	for (const [k, v] of Object.entries(tally)) console.error(`  ${k}: ${JSON.stringify(v)}`);
}
