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
// Slice 4b adds the healing moves (Moonlight, Synthesis, Morning Sun by the weather, Shore Up, Jungle Healing, Lunar Blessing,
// Life Dew and Howl, which share the target `allies`), Strength Sap and Pain Split, Roost (the Type event), the stat overrides
// (Body Press, Psyshock, Psystrike, Secret Sword, Foul Play), the ignored defensive stage (Sacred Sword, Darkest Lariat), Scale
// Shot and Clanging Scales (`selfBoost`), and the fixed damage of Seismic Toss and Night Shade. On top of the above:
//   - a share of the Pokemon are Flying types with Roost, beside a Ground, terrain or status move, and a share of every team
//     carries a Ground move, so that a Ground move meets a Roosted Flying type, and Grassy and Misty Terrain meet one;
//   - healing users carry a weather move about half the time, so that each weather is met, and are hurt by the other side;
//   - defensive boosting moves (Iron Defense, Amnesia, Cosmic Power, Acid Armor, Bulk Up) and the evasion moves (Double Team,
//     Sweet Scent) go to a share of the Pokemon, so that Body Press, Psyshock, Sacred Sword and Foul Play meet boosted stats;
//   - a Roosted Pokemon never shows at a decision point (the volatile ends in the same turn's Residual, and a switch request for a
//     fainted Pokemon comes after the Residual action: battle.ts runAction raises it only when the queue is empty, and nothing
//     supported sets a `switchFlag` mid-turn), so the snapshot's `types` is always the base types; the Roost is observed through
//     what it changes in the turn (a Ground move, the terrains, effectiveness);
//   - levels come from slice 1's generator (one Pokemon in five has a level between 1 and 100), for Seismic Toss.
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
const { Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'pokemon'));
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
// Slice 4b
const WEATHER_HEALS = ['moonlight', 'synthesis', 'morningsun'];
const HEALERS = [...WEATHER_HEALS, 'shoreup', 'junglehealing', 'lunarblessing', 'lifedew', 'howl'];
const SAPPERS = ['strengthsap', 'painsplit'];
const OVERRIDES = ['bodypress', 'psyshock', 'psystrike', 'secretsword', 'foulplay'];
const IGNORE_DEFENSIVE = ['sacredsword', 'darkestlariat'];
const SELF_BOOSTERS = ['scaleshot', 'clangingscales'];
const LEVEL_DAMAGE = ['seismictoss', 'nightshade'];
const NEW_MOVES = [...HEALERS, ...SAPPERS, 'roost', ...OVERRIDES, ...IGNORE_DEFENSIVE, ...SELF_BOOSTERS, ...LEVEL_DAMAGE];
const GROUND_MOVES = ['earthquake', 'earthpower', 'bulldoze', 'drillrun', 'highhorsepower', 'mudshot', 'headlongrush'];
const DEFENSIVE_BOOSTS = ['irondefense', 'amnesia', 'cosmicpower', 'acidarmor', 'bulkup', 'calmmind', 'barrier'];
const EVASION_MOVES = ['doubleteam', 'sweetscent'];
const DEF_DROPS = ['screech', 'metalsound', 'faketears', 'acid'];
const HURTERS = ['tackle', 'quickattack', 'shadowball', 'flamethrower', 'icebeam', 'thunderbolt', 'rockslide', 'closecombat'];
// moves that lower the user's own Defense (Body Press after them reads a negative stage), the target's Attack (Foul Play against
// it), and recoil moves (a foe that faints of its own recoil while a Roosted Pokemon is in play makes a mid-turn switch request)
const SELF_DEF_DROPS = ['closecombat', 'superpower'];
const ATK_DROPS = ['growl', 'charm', 'featherdance', 'tickle', 'scaryface'];
const RECOIL_MOVES = ['bravebird', 'flareblitz', 'doubleedge', 'headsmash', 'woodhammer', 'wildcharge', 'volttackle', 'takedown', 'submission'];

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
	const newMoves = NEW_MOVES.filter(id => pool.moves.includes(id));
	const grounds = GROUND_MOVES.filter(id => pool.moves.includes(id));
	const defBoosts = DEFENSIVE_BOOSTS.filter(id => pool.moves.includes(id));
	const evasion = EVASION_MOVES.filter(id => pool.moves.includes(id));
	const defDrops = DEF_DROPS.filter(id => pool.moves.includes(id));
	const hurters = HURTERS.filter(id => pool.moves.includes(id));
	const only = (xs) => xs.filter(id => pool.moves.includes(id));
	const selfDefDrops = only(SELF_DEF_DROPS), atkDrops = only(ATK_DROPS), recoil = only(RECOIL_MOVES);
	const pureFlying = flying.filter(id => dex.species.get(id).types.length === 1);
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
		// slice 4b: one role in about three Pokemon in four; the rest keep what 4a gave them
		const role = int(100);
		if (role < 24 && flying.length && pool.moves.includes('roost')) {
			// a Roost user: a pure or dual Flying type (a Pokemon that is not Flying now and then), beside a Ground move, a
			// terrain move or a status move, holding Rocky Helmet or Life Orb (a foe that faints of its recoil, or of the
			// helmet, after the Roost ends the move with the Roosted Pokemon still in play)
			const k = int(100);
			if (k < 40 && pureFlying.length) s.species = dex.species.get(pick(pureFlying)).name;
			else if (k < 94) s.species = dex.species.get(pick(flying)).name;
			put('roost');
			if (int(100) < 30 && recoil.length) put(pick(recoil));
			const it = int(100);
			if (it < 40 && pool.items.includes('rockyhelmet')) s.item = dex.items.get('rockyhelmet').name;
			else if (it < 55 && pool.items.includes('lifeorb')) s.item = dex.items.get('lifeorb').name;
			if (int(100) < 22 && pool.abilities.includes('grassysurge')) s.ability = dex.abilities.get('grassysurge').name;
			if (grounds.length && int(100) < 35) put(pick(grounds));
			if (int(100) < 30) put(pick(['grassyterrain', 'mistyterrain', 'electricterrain'].filter(id => pool.moves.includes(id))));
			if (status.length && int(100) < 30) put(pick(status));
		} else if (role < 40 && only(HEALERS).length) {
			// a healer: a weather move beside it about half the time, so that each weather meets each heal
			put(pick(only(HEALERS)));
			if (int(100) < 22) put(pick(only(HEALERS)));
			if (int(100) < 55) put(pick(WEATHER_MOVES.filter(id => pool.moves.includes(id))));
			if (int(100) < 40 && hurters.length) put(pick(hurters));
		} else if (role < 52 && only(SAPPERS).length) {
			put(pick(only(SAPPERS)));
			if (int(100) < 50 && boost.length) put(pick(boost));
		} else if (role < 66 && only(OVERRIDES).length) {
			put(pick(only(OVERRIDES)));
			if (int(100) < 55 && defBoosts.length) put(pick(defBoosts));
			if (int(100) < 30 && selfDefDrops.length) put(pick(selfDefDrops));
			if (int(100) < 30 && atkDrops.length) put(pick(atkDrops));
		} else if (role < 71 && int(100) < 45 && pool.moves.includes('foulplay')) {
			// Foul Play after a drop of the target's Attack (Strength Sap's own, or Growl and its kin)
			put('foulplay');
			if (int(100) < 60 && pool.moves.includes('strengthsap')) put('strengthsap');
			if (int(100) < 60 && atkDrops.length) put(pick(atkDrops));
		} else if (role < 74 && only(IGNORE_DEFENSIVE).length) {
			put(pick(only(IGNORE_DEFENSIVE)));
			if (int(100) < 40 && defBoosts.length) put(pick(defBoosts));
			if (int(100) < 30 && evasion.length) put(pick(evasion));
		} else if (role < 80 && only(SELF_BOOSTERS).length) {
			put(pick(only(SELF_BOOSTERS)));
		} else if (role < 86 && only(LEVEL_DAMAGE).length) {
			put(pick(only(LEVEL_DAMAGE)));
			// Seismic Toss and Night Shade deal the user's level: level 1 and the middle of the range as well as 100
			const l = int(100);
			if (l < 35) s.level = 1; else if (l < 65) s.level = 2 + int(98);
		}
		// every team: Ground moves for the Roosted Flying types, defensive boosts and drops and evasion moves for the overrides
		if (grounds.length && int(100) < 18) put(pick(grounds));
		if (defBoosts.length && int(100) < 16) put(pick(defBoosts));
		if (defDrops.length && int(100) < 9) put(pick(defDrops));
		if (atkDrops.length && int(100) < 6) put(pick(atkDrops));
		if (evasion.length && int(100) < 13) put(pick(evasion));
		if (recoil.length && int(100) < 28) put(pick(recoil));
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
		// recoil and Rocky Helmet: a foe that faints on its own move, or of the helmet, while a Roosted Pokemon is in play
		if (!final.includes('acrobatics') && !final.includes('facade')) {
			const r = int(100);
			if (r < 14 && pool.items.includes('lifeorb')) s.item = dex.items.get('lifeorb').name;
			else if (r < 28 && pool.items.includes('rockyhelmet')) s.item = dex.items.get('rockyhelmet').name;
		}
		// Psyshock against an Assault Vest holder (its Special Defense boost is not the Defense Psyshock reads)
		if (pool.items.includes('assaultvest') && !final.includes('acrobatics') && !final.includes('facade') && int(100) < 9) s.item = dex.items.get('assaultvest').name;
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
		// ---- slice 4b ----
		moveUsed4b: z(NEW_MOVES),
		// onHit heals (Hit event of the move: the user's HP and the weather as the handler sees them)
		weatherHealOk: z(['sun', 'rain', 'sand', 'snow', 'none']), weatherHealAtFullHp: 0,
		weatherHealMove: z(WEATHER_HEALS),
		shoreUpOk: z(['sand', 'other weather', 'none']), shoreUpAtFullHp: 0,
		// junglehealing, lunarblessing: the heal first, then the cure
		healCure: z(['healed and cured', 'healed only', 'cured only (full HP)', 'neither (fails)']),
		lifeDewHealed: 0, lifeDewAtFullHp: 0, howlUsed: 0,
		// strengthsap
		sapTargetAtkStage: z(['positive', 'negative', 'zero', 'minus six (fails)']),
		sapUserHurt: 0, sapUserFullHp: 0, sapHealClampedToMax: 0,
		// painsplit
		painSplitUserHigher: 0, painSplitUserLower: 0, painSplitEqual: 0, painSplitOddSum: 0, painSplitClampedToMaxHp: 0,
		// roost: the types at use, the volatile, and what the missing Flying type changes
		roostUser: z(['pure Flying', 'dual Flying', 'not Flying']), roostVolatileAdded: 0, roostAtFullHp: 0,
		groundMoveVsRoostedPureFlying: 0, groundMoveVsRoostedDualFlying: 0, groundMoveVsUnroostedFlying: 0,
		roostedFlyingGroundedQuery: 0, roostedFlyingGroundedInTerrain: 0,
		effectivenessVsRoosted: z(['Electric', 'Ice', 'Rock', 'Fighting', 'Grass', 'Ground', 'other']),
		grassyHealOnRoosted: 0, mistyTerrainStatusBlockOnRoosted: 0, statusOnRoostedFlying: 0,
		roostRemovedAtEndOfTurn: 0,
		// body press, psyshock and psystrike, secret sword, foul play: whose stage the formula reads
		bodyPressUserDefStage: z(['positive', 'negative', 'zero']), bodyPressDefStageDiffersFromAtk: 0, bodyPressCritIgnoredNegativeDef: 0,
		psyshockTargetDefStage: z(['positive', 'negative', 'zero']), psyshockSpdStageDiffersFromDef: 0,
		psyshockCritIgnoredPositiveDef: 0, psyshockVsSnowDefModifier: 0, psyshockVsAssaultVest: 0,
		psyshockMove: z(['psyshock', 'psystrike', 'secretsword']),
		foulPlayTargetAtkStage: z(['positive', 'negative', 'zero']), foulPlayUserAtkStageDiffersFromTarget: 0, foulPlayCritIgnoredNegativeAtk: 0,
		// sacred sword and darkest lariat
		sacredSwordTargetDefStage: z(['positive', 'negative', 'zero']), sacredSwordCrit: 0,
		ignoreEvasionTargetStage: z(['positive', 'negative', 'zero']),
		// scale shot, clanging scales: one boost after the loop
		scaleShotHits: z([1, 2, 3, 4, 5]), selfBoostApplied: z(['scaleshot', 'clangingscales']), selfBoostSkippedOnMiss: 0,
		selfBoostAfterMultiHit: 0,
		// seismic toss, night shade
		levelDamage: z(['seismictoss', 'nightshade']), levelDamageUserLevel: z(['1', '2 to 99', '100']), levelDamageAtLeastTargetHp: 0,
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
		if (c && id in c.moveUsed4b) c.moveUsed4b[id]++;
		if (c && id === 'roost') {
			const flying = pokemon.types.includes('Flying');
			c.roostUser[!flying ? 'not Flying' : pokemon.types.length === 1 ? 'pure Flying' : 'dual Flying']++;
			if (pokemon.hp >= pokemon.maxhp) c.roostAtFullHp++;
		}
		if (c && id === 'lifedew') { if (pokemon.hp >= pokemon.maxhp) c.lifeDewAtFullHp++; else c.lifeDewHealed++; }
		if (c && id === 'howl') c.howlUsed++;
		const result = useMove.call(this, move, pokemon, ...rest);
		// useMoveInner: `if (move.selfBoost && moveResult) this.moveHit(pokemon, pokemon, move, move.selfBoost, false, true)`
		if (c && dex.moves.get(id).selfBoost && result === false) c.selfBoostSkippedOnMiss++;
		return result;
	};

	// the self boost of Scale Shot and Clanging Scales: moveHit with the move's own `selfBoost` as the hit effect, once per use
	const moveHit = BattleActions.prototype.moveHit;
	BattleActions.prototype.moveHit = function (targets, pokemon, moveOrMoveName, moveData, isSecondary, isSelf) {
		const c = t();
		if (c && isSelf && moveData && moveOrMoveName && moveData === moveOrMoveName.selfBoost) {
			c.selfBoostApplied[moveOrMoveName.id]++;
			if (moveOrMoveName.multihit) c.selfBoostAfterMultiHit++;
		}
		return moveHit.call(this, targets, pokemon, moveOrMoveName, moveData, isSecondary, isSelf);
	};
	// the number of hits of Scale Shot
	const hitLoop = BattleActions.prototype.hitStepMoveHitLoop;
	BattleActions.prototype.hitStepMoveHitLoop = function (targets, pokemon, move) {
		const r = hitLoop.call(this, targets, pokemon, move);
		const c = t();
		if (c && move.id === 'scaleshot' && move.hit in c.scaleShotHits) c.scaleShotHits[move.hit]++;
		return r;
	};

	// Roost and the Type event: what a Roosted Flying type is asked
	const roosted = (p) => !!(p && p.volatiles && p.volatiles.roost && p.baseSpecies.types.includes('Flying'));
	const runImmunity = Pokemon.prototype.runImmunity;
	Pokemon.prototype.runImmunity = function (source, message) {
		const c = t();
		const type = typeof source === 'string' ? source : source && source.type;
		if (c && type === 'Ground' && this.baseSpecies.types.includes('Flying')) {
			if (roosted(this)) c[this.baseSpecies.types.length === 1 ? 'groundMoveVsRoostedPureFlying' : 'groundMoveVsRoostedDualFlying']++;
			else c.groundMoveVsUnroostedFlying++;
		}
		return runImmunity.call(this, source, message);
	};
	const isGrounded = Pokemon.prototype.isGrounded;
	Pokemon.prototype.isGrounded = function (negateImmunity) {
		const c = t();
		if (c && roosted(this)) { c.roostedFlyingGroundedQuery++; if (this.battle.field.terrain) c.roostedFlyingGroundedInTerrain++; }
		return isGrounded.call(this, negateImmunity);
	};
	const runEffectiveness = Pokemon.prototype.runEffectiveness;
	Pokemon.prototype.runEffectiveness = function (move) {
		const c = t();
		if (c && roosted(this)) c.effectivenessVsRoosted[move.type in c.effectivenessVsRoosted ? move.type : 'other']++;
		return runEffectiveness.call(this, move);
	};
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, ...rest) {
		const r = addVolatile.call(this, status, ...rest);
		const c = t();
		if (c && r && (typeof status === 'string' ? dex.toID(status) : status.id) === 'roost') c.roostVolatileAdded++;
		return r;
	};
	const removeVolatile = Pokemon.prototype.removeVolatile;
	Pokemon.prototype.removeVolatile = function (status) {
		const r = removeVolatile.call(this, status);
		const c = t();
		if (c && r && (typeof status === 'string' ? dex.toID(status) : status.id) === 'roost') c.roostRemovedAtEndOfTurn++;
		return r;
	};
	const heal = Battle.prototype.heal;
	Battle.prototype.heal = function (damage, target, source, effect) {
		const c = t();
		// the callbacks call `this.heal(amount, pokemon, pokemon)`: the effect is the running one
		const eff = effect || this.effect;
		if (c && eff && eff.id === 'grassyterrain' && roosted(target || (this.event && this.event.target))) c.grassyHealOnRoosted++;
		return heal.call(this, damage, target, source, effect);
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
		if (c && eventid === 'SetStatus' && target && target.volatiles && target.volatiles.roost && target.baseSpecies.types.includes('Flying')) {
			c.statusOnRoostedFlying++;
			if (this.field.terrain === 'mistyterrain') c.mistyTerrainStatusBlockOnRoosted++;
		}
		if (c && eventid === 'ModifyAccuracy' && rest[0] && rest[0].ignoreEvasion && typeof rest[1] === 'number') {
			const ev = target.boosts.evasion;
			c.ignoreEvasionTargetStage[ev > 0 ? 'positive' : ev < 0 ? 'negative' : 'zero']++;
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
		if (c && eventid === 'Hit' && effect && effect.id && target && source) {
			const id = effect.id, w = this.field.effectiveWeather();
			const wname = w === 'sunnyday' ? 'sun' : w === 'raindance' ? 'rain' : w === 'sandstorm' ? 'sand' : (w === 'snowscape' || w === 'hail') ? 'snow' : w === '' ? 'none' : 'other';
			const full = target.hp >= target.maxhp;
			if (WEATHER_HEALS.includes(id)) {
				c.weatherHealMove[id]++;
				if (full) c.weatherHealAtFullHp++; else c.weatherHealOk[wname]++;
			} else if (id === 'shoreup') {
				if (full) c.shoreUpAtFullHp++; else c.shoreUpOk[w === 'sandstorm' ? 'sand' : w ? 'other weather' : 'none']++;
			} else if (id === 'junglehealing' || id === 'lunarblessing') {
				c.healCure[!full && target.status ? 'healed and cured' : !full ? 'healed only' : target.status ? 'cured only (full HP)' : 'neither (fails)']++;
			} else if (id === 'strengthsap') {
				const st = target.boosts.atk;
				c.sapTargetAtkStage[st === -6 ? 'minus six (fails)' : st > 0 ? 'positive' : st < 0 ? 'negative' : 'zero']++;
				if (source.hp >= source.maxhp) c.sapUserFullHp++; else c.sapUserHurt++;
				if (source.hp + target.getStat('atk', false, true) > source.maxhp) c.sapHealClampedToMax++;
			} else if (id === 'painsplit') {
				if (source.hp > target.hp) c.painSplitUserHigher++; else if (source.hp < target.hp) c.painSplitUserLower++; else c.painSplitEqual++;
				if ((source.hp + target.hp) % 2 === 1) c.painSplitOddSum++;
				const avg = Math.floor((source.hp + target.hp) / 2);
				if (avg > source.maxhp || avg > target.maxhp) c.painSplitClampedToMaxHp++;
			}
		}
		if (c && eventid === 'Effectiveness' && effect && target) {
			if (effect.id === 'freezedry') {
				if (source === 'Water') { if (target.types.length === 2) c.freezeDryVsWaterDualType++; else c.freezeDryVsWaterMono++; } else c.freezeDryVsOther++;
			} else if (effect.id === 'flyingpress' && target.types.length === 2) c.flyingPressDualEffect++;
		}
		return singleEvent.call(this, eventid, effect, state, target, source, ...rest);
	};

	// getDamage: a damageCallback skips the crit roll and the damage modifiers; a fixed `damage` too (Seismic Toss, Night Shade);
	// the stat overrides read other stats and stages (Body Press, Psyshock, Foul Play) and Sacred Sword ignores the defensive stage
	const getDamage = BattleActions.prototype.getDamage;
	const stage3 = (v) => (v > 0 ? 'positive' : v < 0 ? 'negative' : 'zero');
	BattleActions.prototype.getDamage = function (source, target, move, ...rest) {
		const c = t();
		if (c && move && move.damageCallback) {
			if (move.id === 'ruination') c.ruinationUsed++; else if (move.id === 'superfang') c.superFangUsed++;
			if (target.hp === 1) c.halfHpTargetAtOneHp++;
		}
		if (c && move && move.damage === 'level') {
			c.levelDamage[move.id]++;
			c.levelDamageUserLevel[source.level === 1 ? '1' : source.level < 100 ? '2 to 99' : '100']++;
			if (source.level >= target.hp) c.levelDamageAtLeastTargetHp++;
			return getDamage.call(this, source, target, move, ...rest);
		}
		const tracked = c && move && ['bodypress', 'psyshock', 'psystrike', 'secretsword', 'foulplay', 'sacredsword', 'darkestlariat'].includes(move.id);
		const r = getDamage.call(this, source, target, move, ...rest);
		if (tracked) {
			// the crit roll has been made: the hit data carries it
			const crit = target.getMoveHitData(move).crit;
			switch (move.id) {
			case 'bodypress':
				c.bodyPressUserDefStage[stage3(source.boosts.def)]++;
				if (source.boosts.def !== source.boosts.atk) c.bodyPressDefStageDiffersFromAtk++;
				if (crit && source.boosts.def < 0) c.bodyPressCritIgnoredNegativeDef++;
				break;
			case 'psyshock': case 'psystrike': case 'secretsword':
				c.psyshockMove[move.id]++;
				c.psyshockTargetDefStage[stage3(target.boosts.def)]++;
				if (target.boosts.def !== target.boosts.spd) c.psyshockSpdStageDiffersFromDef++;
				if (crit && target.boosts.def > 0) c.psyshockCritIgnoredPositiveDef++;
				if (this.battle.field.effectiveWeather() === 'snowscape' && target.hasType('Ice')) c.psyshockVsSnowDefModifier++;
				if (target.item === 'assaultvest') c.psyshockVsAssaultVest++;
				break;
			case 'foulplay':
				c.foulPlayTargetAtkStage[stage3(target.boosts.atk)]++;
				if (target.boosts.atk !== source.boosts.atk) c.foulPlayUserAtkStageDiffersFromTarget++;
				if (crit && target.boosts.atk < 0) c.foulPlayCritIgnoredNegativeAtk++;
				break;
			default:
				c.sacredSwordTargetDefStage[stage3(target.boosts.def)]++;
				if (crit) c.sacredSwordCrit++;
			}
		}
		return r;
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
