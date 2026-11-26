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
// Slice 4c adds the moves that remove, move or set field state: Rapid Spin, Mortal Spin and Ice Spinner (`onAfterHit`), Defog and Tidy Up
// and Clear Smog (`onHit`), Court Change and Haze (`onHitField`), Ceaseless Edge and Stone Axe (`onAfterHit` sets a hazard), Aurora Veil
// (`onTry`, and its condition), and the screen breakers Brick Break, Psychic Fangs and Raging Bull (`onTryHit`). On top of the above,
// from a stream of random draws of its own (so the 4a and 4b choices are the same for a seed):
//   - a share of the Pokemon set hazards (two or three of Stealth Rock, Spikes, Toxic Spikes and Sticky Web, and Ceaseless Edge or
//     Stone Axe) and a share carry a remover or a swapper (the Spins, Ice Spinner, Defog, Tidy Up, Court Change, Haze, Clear Smog), so
//     that a removal move meets one, two, three or four hazards on either side;
//   - a share carry the screens (Reflect, Light Screen, Aurora Veil, Tailwind) with Light Clay, Snowscape or Snow Warning, so that
//     Aurora Veil is used in and out of snow and with Reflect or Light Screen up, Court Change moves screens and durations, Defog
//     removes the foe's screens, and the screen breakers meet each screen;
//   - a share of the Pokemon are Shedinja holding Heavy-Duty Boots with a Spin, so that a user that faints of the foe's Rocky Helmet
//     in the same hit (one HP, a helmet that deals at least 1) makes the HP gate of `AfterHit` matter;
//   - the removers come with boost moves, terrain moves and Light Clay, so that Haze and Clear Smog meet boosts, and Ice Spinner and
//     Defog meet each terrain.
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

// Slice 4c
const HAZARDS = ['stealthrock', 'spikes', 'toxicspikes', 'stickyweb'];
const HAZARD_ATTACKS = ['ceaselessedge', 'stoneaxe'];
const SPINS = ['rapidspin', 'mortalspin'];
const REMOVERS = [...SPINS, 'icespinner', 'defog', 'tidyup', 'courtchange', 'haze', 'clearsmog'];
const BREAKERS = ['brickbreak', 'psychicfangs', 'ragingbull'];
const SCREENS = ['reflect', 'lightscreen', 'auroraveil', 'tailwind'];
const MOVES_4C = [...REMOVERS, ...HAZARD_ATTACKS, ...BREAKERS, 'auroraveil'];
// the conditions of the state a Pokemon can hold (the entry hazards, the screens and Tailwind) and what Showdown's own lists include
const HAZ_IDS = ['spikes', 'toxicspikes', 'stealthrock', 'stickyweb'];
const SCREEN_IDS = ['reflect', 'lightscreen', 'auroraveil'];
const COURT_CHANGE_IDS = [
	'mist', 'lightscreen', 'reflect', 'spikes', 'safeguard', 'tailwind', 'toxicspikes', 'stealthrock', 'waterpledge', 'firepledge',
	'grasspledge', 'stickyweb', 'auroraveil', 'luckychant', 'gmaxsteelsurge', 'gmaxcannonade', 'gmaxvinelash', 'gmaxwildfire', 'gmaxvolcalith',
];
const TERRAIN_IDS = ['electricterrain', 'grassyterrain', 'mistyterrain', 'psychicterrain'];
// listed but rejected or not whitelisted: nothing to report (Substitute, Leech Seed, Safeguard, Mist, Knock Off and partial trapping are
// not moves of the pool, so their branches in the bodies above are structurally zero)

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
	// slice 4c: a stream of draws of its own, so that the choices above are those of slice 4b for a seed
	const rnd4 = mulberry32(seed * 3266489917 + 668265263);
	const int4 = (n) => Math.floor(rnd4() * n);
	const pick4 = (xs) => xs[int4(xs.length)];
	const has = (ids) => ids.filter(id => pool.moves.includes(id));
	const hazards = has(HAZARDS), hazardAttacks = has(HAZARD_ATTACKS), removers = has(REMOVERS), breakers = has(BREAKERS);
	const screens = has(SCREENS), spins = has(SPINS);
	const boostsAll = has([...BOOST_MOVES, ...DEFENSIVE_BOOSTS]);
	const terrains = has(TERRAIN_MOVES), weathers = has(WEATHER_MOVES);
	const shedinja = pool.species.includes('shedinja') ? dex.species.get('shedinja').name : null;
	const set4c = (s) => {
		const slots = [0, 1, 2, 3].filter(i => !keep.includes(idOf(s.moves[i])));
		const put = (id) => { if (id && slots.length && !s.moves.map(idOf).includes(id)) s.moves[slots.splice(int4(slots.length), 1)[0]] = id; };
		const item = (id) => { if (pool.items.includes(id)) s.item = dex.items.get(id).name; };
		const r = int4(100);
		if (r < 20 && hazards.length) {
			// a hazard setter: two or three hazards (Spikes more often: three layers), and a hazard attack now and then
			put(int4(100) < 70 && hazards.includes('spikes') ? 'spikes' : pick4(hazards));
			put(pick4(hazards));
			if (int4(100) < 50) put(pick4(hazards));
			if (hazardAttacks.length && int4(100) < 40) put(pick4(hazardAttacks));
		} else if (r < 52 && removers.length) {
			// a remover, with a hazard move now and then (its own side's hazards come from the foe's setters)
			const m = pick4(removers);
			put(m);
			if (int4(100) < 30) put(pick4(removers));
			if (hazards.length && int4(100) < 30) put(pick4(hazards));
			if (m === 'icespinner' && terrains.length && int4(100) < 55) put(pick4(terrains));
			if ((m === 'haze' || m === 'clearsmog') && boostsAll.length && int4(100) < 60) put(pick4(boostsAll));
			if (m === 'defog' && screens.length && int4(100) < 40) put(pick4(screens));
			if (m === 'courtchange') { if (screens.length) put(pick4(screens)); if (hazards.length && int4(100) < 60) put(pick4(hazards)); }
			if (m === 'rapidspin' || m === 'mortalspin') { if (int4(100) < 25) item('lifeorb'); else if (int4(100) < 15) item('toxicorb'); }
		} else if (r < 66 && screens.length) {
			// screens: Aurora Veil with snow (Snowscape or Snow Warning), Light Clay, Reflect or Light Screen beside it, Tailwind
			put(pick4(screens));
			if (int4(100) < 50) put(pick4(screens));
			if (pool.moves.includes('auroraveil') && int4(100) < 55) put('auroraveil');
			if (weathers.length && int4(100) < 50) put(pool.moves.includes('snowscape') && int4(100) < 70 ? 'snowscape' : pick4(weathers));
			if (int4(100) < 22 && pool.abilities.includes('snowwarning')) s.ability = dex.abilities.get('snowwarning').name;
			if (int4(100) < 45) item('lightclay');
		} else if (r < 80 && breakers.length) {
			// a screen breaker, with a screen setter of its own sometimes (a breaker into its own side's screens is not a case)
			put(pick4(breakers));
			if (int4(100) < 25) put(pick4(breakers));
		} else if (r < 88 && shedinja && spins.length) {
			// Shedinja: one HP, so the foe's Rocky Helmet always faints it in the hit; Heavy-Duty Boots keep it alive into hazards
			s.species = shedinja;
			s.ability = 'No Ability';
			put(pick4(spins));
			item('heavydutyboots');
		} else if (r < 97) {
			// the rarer cases of 4a and 4b, which a thousand battles of random teams meet only now and then (a critical hit that
			// ignores a stage: Body Press after a Defense drop, Foul Play against an Attack drop, Psyshock against a raised Defense;
			// Fickle Beam with another modifier; a raised evasion for the moves that ignore it; Hex against a frozen target)
			const k = int4(6);
			if (k === 0) { put('bodypress'); put(pick4(has(SELF_DEF_DROPS))); }
			else if (k === 1) { put('foulplay'); put(pick4(has(ATK_DROPS))); put('strengthsap'); }
			else if (k === 2) { put(pick4(has(['psyshock', 'psystrike', 'secretsword']))); put(pick4(has(DEFENSIVE_BOOSTS))); }
			else if (k === 3) { put('ficklebeam'); item('dragonfang'); if (int4(100) < 50) put(pick4(terrains)); }
			else if (k === 4) { put(pick4(has(EVASION_MOVES))); put(pick4(has(['sacredsword', 'darkestlariat']))); }
			else { put('hex'); put(pick4(has(FREEZERS))); put(pick4(has(FREEZERS))); }
		}
		// Rocky Helmet is the foe's side of the HP gate
		if (int4(100) < 12 && !['lifeorb', 'heavydutyboots', 'lightclay'].some(i => s.item === dex.items.get(i).name)) item('rockyhelmet');
	};
	// a mirror match keeps its two teams identical (the two sides tie on speed and priority)
	for (const s of scenario.teams[0]) { set(s); set4c(s); }
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
	else for (const s of scenario.teams[1]) { set(s); set4c(s); }
	// slice 4c, scripted openings (`scenario.choices`, a pair per decision point, then the random policy): about one battle in ten
	//  - a hazard stack: one lead sets Stealth Rock, three layers of Spikes, two of Toxic Spikes and Sticky Web on the foe's side
	//    (all four kinds, at their limits) while the foe lead boosts; then either the foe lead uses a remover or swapper on its own
	//    side's stack (Rapid Spin, Defog, Tidy Up, Court Change, Haze, Clear Smog, Mortal Spin), or the setter switches to its second
	//    Pokemon, which uses one on the stack it set (hazards on the foe's side: Defog, Tidy Up, Court Change, Ice Spinner);
	//  - a Defog that fails: three Sweet Scents take the foe's evasion to -6, and Defog finds nothing else to do.
	const script = int4(100);
	const kinds = script < 4 ? 'stackSelf' : script < 7 ? 'stackFoe' : script < 9 ? 'defogFails' : null;
	const lead = (s, moves) => {
		const neutral = pool.species.filter(id => { const ty = dex.species.get(id).types; return !ty.includes('Ghost') && !ty.includes('Steel'); });
		s.species = dex.species.get(pick4(neutral)).name; s.ability = 'No Ability'; s.item = ''; s.moves = moves;
	};
	const present = (ids) => ids.every(id => pool.moves.includes(id));
	if (kinds && !duel && present(['stealthrock', 'spikes', 'toxicspikes', 'stickyweb', 'calmmind', 'swordsdance', 'nastyplot', 'agility', 'sweetscent', 'defog'])) {
		const setter = int4(2), foe = 1 - setter;
		const pair = (a, b) => (setter === 0 ? [a, b] : [b, a]);
		const idle = 'move 1';
		const boosts = ['calmmind', 'swordsdance', 'nastyplot', 'agility'];
		if (kinds === 'defogFails') {
			lead(scenario.teams[setter][0], ['sweetscent', 'defog', 'calmmind', 'tackle'].filter(id => pool.moves.includes(id)));
			lead(scenario.teams[foe][0], boosts);
			if (scenario.teams[setter][0].moves.length === 4) {
				scenario.choices = [['team 123456', 'team 123456'], ...[1, 1, 1, 2].map(m => pair(`move ${m}`, idle))];
			}
		} else {
			const removers = REMOVERS.filter(id => pool.moves.includes(id));
			const x = pick4(removers);
			lead(scenario.teams[setter][0], ['stealthrock', 'spikes', 'toxicspikes', 'stickyweb']);
			const slots = [1, 2, 2, 2, 3, 3, 4];
			const turns = slots.map(m => pair(`move ${m}`, idle));
			if (kinds === 'stackSelf') {
				// the foe lead removes from its own side: its first move is the remover; it boosts with the others until then
				lead(scenario.teams[foe][0], [x, 'calmmind', 'swordsdance', 'nastyplot']);
				const boostTurns = slots.map(m => pair(`move ${m}`, 'move 2'));
				scenario.choices = [['team 123456', 'team 123456'], ...boostTurns, pair(null, 'move 1')];
			} else {
				// the setter's second Pokemon comes in and uses the remover on the stack its lead set
				lead(scenario.teams[foe][0], boosts);
				const bench = scenario.teams[setter][1];
				bench.species = dex.species.get(pick4(pool.species.filter(id => { const ty = dex.species.get(id).types; return !ty.includes('Ghost') && !ty.includes('Steel'); }))).name;
				bench.ability = 'No Ability'; bench.item = '';
				bench.moves = [x, 'calmmind', 'swordsdance', 'nastyplot'];
				const boostTurns = slots.map(m => pair(`move ${m}`, 'move 1'));
				scenario.choices = [['team 123456', 'team 123456'], ...boostTurns, pair('switch 2', 'move 1'), pair('move 1', 'move 1')];
			}
		}
	}
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
		// ---- slice 4c ----
		moveUsed4c: z(MOVES_4C),
		// onAfterHit of Rapid Spin and Mortal Spin: the hazards on the user's side (removed) and on the foe's (kept) at the hit, and
		// the HP gate: a user that is down at the end of the hit (the helmet, in the same hit) with hazards still on its side
		spinHazardsOnUserSide: z(['0', '1', '2', '3', '4']), spinHazardKindOnUserSide: z(HAZ_IDS), spinHazardsOnFoeSide: z(['0', '1 or more']),
		spinUserFaintedInHit: 0, spinUserFaintedToRockyHelmet: 0, spinGateSuppressedRemoval: 0, spinUserFaintedNoHazards: 0,
		spinSpikesLayers: z(['1', '2', '3']), toxicSpikesLayersOnSpinSide: z(['1', '2']),
		// icespinner: the terrain it clears
		iceSpinnerTerrain: z(['none', ...TERRAIN_IDS]),
		// ceaselessedge and stoneaxe: the layers on the foe's side before the hit; the user's own side is not touched
		ceaselessEdgeSpikesBefore: z(['0', '1', '2', '3']), stoneAxeRockBefore: z(['0', '1']),
		hazardAttackUserSideHadHazards: 0, hazardAttackUserFainted: 0,
		// defog: what each side holds, the terrain, the evasion stage, and the result
		defogHazardsOnTarget: z(['0', '1', '2', '3', '4']), defogHazardsOnUser: z(['0', '1', '2', '3', '4']),
		defogScreensOnTarget: z([...SCREEN_IDS, 'none']), defogScreensOnUser: z([...SCREEN_IDS, 'none']),
		defogTerrain: z(['none', ...TERRAIN_IDS]), defogTargetEvasion: z(['minus six', 'other']),
		defogResult: z(['ok', 'fails']), defogOkByBoostOnly: 0, defogOkByHazardOnly: 0, defogScreensOnlyNoBoost: 0,
		// tidyup: the hazards on each side and the result
		tidyUpHazardsOnUser: z(['0', '1 or more']), tidyUpHazardsOnFoe: z(['0', '1 or more']),
		tidyUpResult: z(['ok', 'fails']), tidyUpOkByHazardsOnly: 0, tidyUpUserStagesAtMax: 0,
		// clearsmog: the target's stages at the hit
		clearSmogTargetStages: z(['boosted', 'lowered', 'both', 'none']), clearSmogFaintedTarget: 0,
		// courtchange: which sides hold swappable conditions, layers, durations, and what the order of the states becomes
		courtChangeSides: z(['both', 'user side only', 'foe side only', 'neither (fails)']),
		courtChangeMultiLayerHazard: 0, courtChangeTimedCondition: 0, courtChangeSameConditionOnBothSides: 0, courtChangeTailwind: 0,
		courtChangeOrdersNotIncreasing: 0, courtChangeScreen: z(SCREEN_IDS),
		// haze: the stages it clears
		hazeUserStages: z(['boosted', 'lowered', 'none']), hazeFoeStages: z(['boosted', 'lowered', 'none']),
		// auroraveil: onTry (snow or not), Light Clay, Reflect and Light Screen up, and the damage it halves
		auroraVeilTry: z(['snow', 'no snow (fails)']), auroraVeilWithLightClay: 0, auroraVeilAlreadyUp: 0,
		auroraVeilWithReflect: 0, auroraVeilWithLightScreen: 0,
		veilVsPhysical: z(['alone', 'with reflect']), veilVsSpecial: z(['alone', 'with light screen']), veilCritNotHalved: 0,
		// brickbreak, psychicfangs, ragingbull: the screens on the target's side before the damage
		screenBreakScreens: z([...SCREEN_IDS, 'none']), screenBreakReflectOnPhysical: 0, screenBreakMove: z(BREAKERS),
		// structurally zero: no species of the pool is a Tauros forme, so Raging Bull never changes type; Safeguard, Mist, Leech Seed,
		// partial trapping, Substitute and G-Max Steelsurge are not conditions here, so the removals of the bodies never meet them;
		// and Court Change takes every condition a side can hold (all of them are in its list), so each side ends with the other's
		// whole list, in the order it had, and the effect orders along a side stay increasing (a state that stayed behind would be
		// followed by arrivals with lower orders)
		ragingBullTypeChanged: 0,
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
		if (c && id in c.moveUsed4c) c.moveUsed4c[id]++;
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


	// ---- slice 4c ----
	const haz = (side) => HAZ_IDS.filter(id => side.sideConditions[id]);
	const scr = (side) => SCREEN_IDS.filter(id => side.sideConditions[id]);
	const terrainName = (b) => b.field.terrain || 'none';
	const stages = (p) => { const v = Object.values(p.boosts); return v.some(x => x > 0) ? (v.some(x => x < 0) ? 'both' : 'boosted') : v.some(x => x < 0) ? 'lowered' : 'none'; };
	const spreadMoveHit = BattleActions.prototype.spreadMoveHit;
	BattleActions.prototype.spreadMoveHit = function (targets, pokemon, moveOrMoveName, hitEffect, isSecondary, isSelf) {
		const c = t();
		const id = moveOrMoveName && moveOrMoveName.id;
		const watched = c && !isSecondary && !isSelf && (SPINS.includes(id) || HAZARD_ATTACKS.includes(id));
		const r = spreadMoveHit.call(this, targets, pokemon, moveOrMoveName, hitEffect, isSecondary, isSelf);
		if (watched && pokemon.hp === 0) {
			// the HP gate of AfterHit: the user is down at the end of the hit (its faint is only queued)
			const target = targets[0];
			if (SPINS.includes(id)) {
				c.spinUserFaintedInHit++;
				if (target && target.item === 'rockyhelmet') c.spinUserFaintedToRockyHelmet++;
				if (haz(pokemon.side).length) c.spinGateSuppressedRemoval++; else c.spinUserFaintedNoHazards++;
			} else c.hazardAttackUserFainted++;
		}
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
		if (c && eventid === 'ModifyDamage' && source && rest[0] && rest[0].id && source.side.sideConditions.auroraveil) {
			// modifyDamage: runEvent('ModifyDamage', attacker, defender, move, baseDamage); the veil's handler is onAnyModifyDamage
			const move = rest[0], def = source, physical = move.category === 'Physical';
			if (move.category !== 'Status') {
				if (def.getMoveHitData(move).crit) c.veilCritNotHalved++;
				else if (physical) c.veilVsPhysical[def.side.sideConditions.reflect ? 'with reflect' : 'alone']++;
				else c.veilVsSpecial[def.side.sideConditions.lightscreen ? 'with light screen' : 'alone']++;
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
		if (c && effect && effect.id && target) {
			const id = effect.id;
			// onTryHit of the screen breakers: the screens on the target's side, before the damage
			if (eventid === 'TryHit' && BREAKERS.includes(id)) {
				c.screenBreakMove[id]++;
				const sc = scr(target.side);
				if (sc.length) for (const x of sc) c.screenBreakScreens[x]++; else c.screenBreakScreens.none++;
				if (sc.includes('reflect') && effect.category === 'Physical') c.screenBreakReflectOnPhysical++;
				if (id === 'ragingbull' && effect.type !== 'Normal') c.ragingBullTypeChanged++;
			}
			// onTry of Aurora Veil (the user is the event's target): snow or not
			if (eventid === 'Try' && id === 'auroraveil') {
				const r = singleEvent.call(this, eventid, effect, state, target, source, ...rest);
				c.auroraVeilTry[r ? 'snow' : 'no snow (fails)']++;
				if (r) {
					if (target.hasItem('lightclay')) c.auroraVeilWithLightClay++;
					if (target.side.sideConditions.auroraveil) c.auroraVeilAlreadyUp++;
					if (target.side.sideConditions.reflect) c.auroraVeilWithReflect++;
					if (target.side.sideConditions.lightscreen) c.auroraVeilWithLightScreen++;
				}
				return r;
			}
			// onAfterHit: target is the foe that was hit and source the user
			if (eventid === 'AfterHit' && source) {
				if (SPINS.includes(id)) {
					const mine = haz(source.side);
					c.spinHazardsOnUserSide[String(mine.length)]++;
					for (const x of mine) c.spinHazardKindOnUserSide[x]++;
					if (source.side.sideConditions.spikes) c.spinSpikesLayers[String(source.side.sideConditions.spikes.layers)]++;
					if (source.side.sideConditions.toxicspikes) c.toxicSpikesLayersOnSpinSide[String(source.side.sideConditions.toxicspikes.layers)]++;
					c.spinHazardsOnFoeSide[haz(target.side).length ? '1 or more' : '0']++;
				} else if (id === 'icespinner') {
					c.iceSpinnerTerrain[terrainName(this)]++;
				} else if (id === 'ceaselessedge') {
					const l = target.side.sideConditions.spikes ? target.side.sideConditions.spikes.layers : 0;
					c.ceaselessEdgeSpikesBefore[String(l)]++;
					if (haz(source.side).length) c.hazardAttackUserSideHadHazards++;
				} else if (id === 'stoneaxe') {
					c.stoneAxeRockBefore[target.side.sideConditions.stealthrock ? '1' : '0']++;
					if (haz(source.side).length) c.hazardAttackUserSideHadHazards++;
				}
			}
			if (eventid === 'Hit' && source && id === 'defog') {
				const th = haz(target.side).length, uh = haz(source.side).length;
				const ts = scr(target.side), us = scr(source.side);
				c.defogHazardsOnTarget[String(th)]++;
				c.defogHazardsOnUser[String(uh)]++;
				if (ts.length) for (const x of ts) c.defogScreensOnTarget[x]++; else c.defogScreensOnTarget.none++;
				if (us.length) for (const x of us) c.defogScreensOnUser[x]++; else c.defogScreensOnUser.none++;
				c.defogTerrain[terrainName(this)]++;
				const ev = target.boosts.evasion;
				c.defogTargetEvasion[ev === -6 ? 'minus six' : 'other']++;
				const r = singleEvent.call(this, eventid, effect, state, target, source, ...rest);
				c.defogResult[r ? 'ok' : 'fails']++;
				if (r && th + uh === 0 && ev > -6) c.defogOkByBoostOnly++;
				if (r && th + uh > 0 && ev === -6) c.defogOkByHazardOnly++;
				if (ts.length && th + uh === 0 && ev === -6) c.defogScreensOnlyNoBoost++;
				return r;
			}
			if (eventid === 'Hit' && source && id === 'tidyup') {
				const uh = haz(source.side).length, fh = haz(target.side.foe).length;
				c.tidyUpHazardsOnUser[uh ? '1 or more' : '0']++;
				c.tidyUpHazardsOnFoe[fh ? '1 or more' : '0']++;
				const maxed = target.boosts.atk === 6 && target.boosts.spe === 6;
				if (maxed) c.tidyUpUserStagesAtMax++;
				const r = singleEvent.call(this, eventid, effect, state, target, source, ...rest);
				c.tidyUpResult[r ? 'ok' : 'fails']++;
				if (r && maxed) c.tidyUpOkByHazardsOnly++;
				return r;
			}
			if (eventid === 'Hit' && source && id === 'clearsmog') {
				c.clearSmogTargetStages[stages(target)]++;
				if (target.hp === 0) c.clearSmogFaintedTarget++;
			}
			if (eventid === 'HitField' && id === 'haze') {
				for (const p of this.getAllActive()) {
					const who = p === source ? c.hazeUserStages : c.hazeFoeStages;
					const st = stages(p);
					who[st === 'both' ? 'boosted' : st]++;
				}
			}
			if (eventid === 'HitField' && id === 'courtchange' && source) {
				const mine = COURT_CHANGE_IDS.filter(x => source.side.sideConditions[x]);
				const theirs = COURT_CHANGE_IDS.filter(x => source.side.foe.sideConditions[x]);
				c.courtChangeSides[mine.length && theirs.length ? 'both' : mine.length ? 'user side only' : theirs.length ? 'foe side only' : 'neither (fails)']++;
				const all = [...mine.map(x => source.side.sideConditions[x]), ...theirs.map(x => source.side.foe.sideConditions[x])];
				const ids = [...mine, ...theirs];
				if (all.some(st => (st.id === 'spikes' || st.id === 'toxicspikes') && st.layers > 1)) c.courtChangeMultiLayerHazard++;
				if (all.some(st => st.duration)) c.courtChangeTimedCondition++;
				if (ids.includes('tailwind')) c.courtChangeTailwind++;
				for (const x of SCREEN_IDS) if (ids.includes(x)) c.courtChangeScreen[x]++;
				if (mine.some(x => theirs.includes(x))) c.courtChangeSameConditionOnBothSides++;
				const r = singleEvent.call(this, eventid, effect, state, target, source, ...rest);
				// each state kept its effectOrder: the order along a side is no longer increasing once states of the other side arrive
				const inc = (side) => { const o = Object.values(side.sideConditions).map(st => st.effectOrder); return o.every((v, i) => i === 0 || o[i - 1] < v); };
				if (!inc(source.side) || !inc(source.side.foe)) c.courtChangeOrdersNotIncreasing++;
				return r;
			}
		}
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
