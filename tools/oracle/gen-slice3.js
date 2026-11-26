#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 3" (field state: 3a entry hazards, 3b screens, Tailwind and Trick Room, 3c
// weather, 3d terrain, 3e weather- and terrain-driven boosts and item use) and records each on Showdown.
//
// Slice 3a adds side conditions (Stealth Rock, Spikes, Toxic Spikes, Sticky Web), Heavy-Duty Boots and Toxic Debris,
// and with them the effect order (`battle.effectOrder`) that sorts two hazards on one side. Slice 3b adds durations:
// Reflect, Light Screen (and Light Clay), Tailwind and Trick Room. The teams are slice 1's
// (gen-slice1.js makeScenario, so the move, species and spread draws are the same), then each Pokemon is changed by a
// generator of its own, so slices 1 and 2 stay reproducible from their seeds:
//   - a share of the Pokemon get a hazard setter in a move slot (two setters often, so a side holds two or three
//     hazards and `effectOrder` decides their order), and a share get a field setter (Reflect, Light Screen,
//     Tailwind, Trick Room: both screens on one side tie in ModifyDamage, a screen on each side tie in Residual);
//   - a share get a weather move (Sunny Day, Rain Dance, Sandstorm, Snowscape) and a share a weather ability (Drought,
//     Drizzle, Sand Stream, Snow Warning, or Swift Swim, Chlorophyll, Sand Rush, Slush Rush, Hydration) and a rock;
//     Rock, Ground and Steel types meet the sand; in a mirror match the lead is often a weather setter, so the two
//     leads tie on speed and both set weather at the turn-0 switch-in;
//   - a share get a terrain move (Electric, Grassy, Misty, Psychic Terrain), a terrain ability (the surges), Terrain
//     Extender, Overcoat or Leaf Guard, and a priority move, a powder move, Earthquake or Bulldoze; Flying types (not
//     grounded), Grass types (powder immune, Grassy Terrain) and Dragon types (Misty Terrain) meet them; in a mirror
//     match the lead is a terrain or weather setter, so the two leads tie on speed and both set a field condition at
//     the turn-0 switch-in;
//   - a share are Paradox-style holders (Protosynthesis, Quark Drive, with and without Booster Energy), seed holders
//     (Electric, Grassy, Misty, Psychic Seed) and Unburden holders; low-level Pokemon often tie on their best stat; in
//     a mirror match the lead often holds one of these, so two listeners tie on speed at the same WeatherChange or
//     TerrainChange;
//   - a share get a paralysing move (Tailwind and Trick Room against paralysed Pokemon), and in a mirror match the
//     two sides tie on speed, under Trick Room too;
//   - a share are Flying types (not grounded: the hazards skip them), Poison types (a grounded one absorbs Toxic
//     Spikes), Steel types (immune to the poison), or Shedinja (1 HP: any hazard knocks it out);
//   - a share are level 1-20 (small HP, so a hazard can knock it out and the damage floors at 1);
//   - abilities and items are drawn from the pools the engine accepts (`cabal run -v0 engine-pool`), weighted toward
//     Toxic Debris and Heavy-Duty Boots; a share keep "No Ability" and no item.
// A mirror match keeps its two teams identical, so the two sides tie on speed at switch-in. The policy picks a random
// legal choice, switches included, so Pokemon enter often and the hazards fire.
//
// Usage:
//   node gen-slice3.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice3-<seed>.json, plus slice3-moves.json, slice3-species.json, slice3-abilities.json and
// slice3-items.json (copies of the pool, which the parity test compares with what the engine accepts). A tally of the
// outcomes that distinguish a hazard handler is printed to stderr; it is computed by wrapping Showdown's own methods
// (observation only: the wrappers pass every argument and result through and call nothing the battle would not call;
// in particular not pokemon.hasType or getTypes, which run events, but the `types` property).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Side, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');

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

const HAZARDS = ['stealthrock', 'spikes', 'toxicspikes', 'stickyweb'];
const FIELD_SETTERS = ['reflect', 'lightscreen', 'tailwind', 'trickroom'];
const SCREENS = ['reflect', 'lightscreen'];
const WEATHER_MOVES = ['sunnyday', 'raindance', 'sandstorm', 'snowscape'];
const WEATHER_SETTERS = ['drought', 'drizzle', 'sandstream', 'snowwarning'];
const WEATHER_SPEED = ['swiftswim', 'chlorophyll', 'sandrush', 'slushrush'];
const TERRAIN_MOVES = ['electricterrain', 'grassyterrain', 'mistyterrain', 'psychicterrain'];
const TERRAIN_SETTERS = ['electricsurge', 'grassysurge', 'mistysurge', 'psychicsurge'];
const TERRAINS = ['electricterrain', 'grassyterrain', 'mistyterrain', 'psychicterrain'];
const POWDER_MOVES = ['stunspore', 'poisonpowder', 'cottonspore'];
const GROUND_MOVES = ['earthquake', 'bulldoze'];
const PARADOX = ['protosynthesis', 'quarkdrive'];
const SEEDS = ['electricseed', 'grassyseed', 'mistyseed', 'psychicseed'];
const SEED_TERRAIN = { electricseed: 'electricterrain', grassyseed: 'grassyterrain', mistyseed: 'mistyterrain', psychicseed: 'psychicterrain' };
const ROCKS = ['heatrock', 'damprock', 'smoothrock', 'icyrock'];
const WEATHERS = ['sunnyday', 'raindance', 'sandstorm', 'snowscape'];
const PARALYSERS = ['thunderwave', 'glare', 'stunspore', 'nuzzle'];

function assign(scenario, pool, seed) {
	const rnd = mulberry32(seed * 1103515245 + 12345);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const setters = HAZARDS.filter(id => pool.moves.includes(id));
	if (setters.length === 0) throw new Error('the pool has no hazard setter');
	const fieldSetters = FIELD_SETTERS.filter(id => pool.moves.includes(id));
	if (fieldSetters.length === 0) throw new Error('the pool has no field setter');
	const paralysers = PARALYSERS.filter(id => pool.moves.includes(id));
	const bySpeciesType = (t) => pool.species.filter(id => dex.species.get(id).types.includes(t));
	const flying = bySpeciesType('Flying'), poison = bySpeciesType('Poison'), steel = bySpeciesType('Steel');
	const rock = bySpeciesType('Rock'), ground = bySpeciesType('Ground'), ice = bySpeciesType('Ice');
	const grass = bySpeciesType('Grass'), dragon = bySpeciesType('Dragon'), electric = bySpeciesType('Electric');
	const terrainMoves = TERRAIN_MOVES.filter(id => pool.moves.includes(id));
	if (terrainMoves.length === 0) throw new Error('the pool has no terrain move');
	const powderMoves = POWDER_MOVES.filter(id => pool.moves.includes(id));
	const groundMoves = GROUND_MOVES.filter(id => pool.moves.includes(id));
	const priorityMoves = pool.moves.filter(id => dex.moves.get(id).priority > 0);
	const weatherMoves = WEATHER_MOVES.filter(id => pool.moves.includes(id));
	if (weatherMoves.length === 0) throw new Error('the pool has no weather move');
	const abilities = pool.abilities.filter(id => id !== 'noability');
	const items = pool.items.filter(id => id !== '');
	const weightedAbilities = abilities.concat(abilities.filter(id => id === 'toxicdebris').flatMap(id => [id, id, id, id, id]))
		.concat(abilities.filter(id => WEATHER_SETTERS.includes(id) || WEATHER_SPEED.includes(id) || id === 'hydration' ||
			TERRAIN_SETTERS.includes(id) || id === 'overcoat' || id === 'leafguard' || PARADOX.includes(id) || id === 'unburden').flatMap(id => [id, id, id]));
	const weightedItems = items.concat(items.filter(id => id === 'heavydutyboots' || id === 'lightclay').flatMap(id => [id, id, id, id]))
		.concat(items.filter(id => ROCKS.includes(id) || id === 'terrainextender' || id === 'boosterenergy' || SEEDS.includes(id)).flatMap(id => [id, id, id]));
	const setterAbilities = abilities.filter(id => WEATHER_SETTERS.includes(id) || TERRAIN_SETTERS.includes(id));
	const mirror = JSON.stringify(scenario.teams[0]) === JSON.stringify(scenario.teams[1]);
	const set = (s) => {
		// species
		const k = int(100);
		if (k < 18 && flying.length) s.species = dex.species.get(pick(flying)).name;
		else if (k < 38 && poison.length) s.species = dex.species.get(pick(poison)).name;
		else if (k < 46 && steel.length) s.species = dex.species.get(pick(steel)).name;
		else if (k < 49 && pool.species.includes('shedinja')) s.species = 'Shedinja';
		else if (k < 58 && rock.length) s.species = dex.species.get(pick(rock)).name;
		else if (k < 66 && ground.length) s.species = dex.species.get(pick(ground)).name;
		else if (k < 70 && ice.length) s.species = dex.species.get(pick(ice)).name;
		else if (k < 79 && grass.length) s.species = dex.species.get(pick(grass)).name;
		else if (k < 85 && dragon.length) s.species = dex.species.get(pick(dragon)).name;
		else if (k < 89 && electric.length) s.species = dex.species.get(pick(electric)).name;
		// level: small Pokemon have little HP
		if (int(100) < 14) s.level = 1 + int(20);
		// hazard setters: one in two Pokemon has one, a quarter have two
		const nSetters = int(100) < 40 ? (int(2) === 0 ? 2 : 1) : 0;
		const slots = [0, 1, 2, 3];
		for (let i = 0; i < nSetters; i++) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(setters);
		// field setters: half of the Pokemon have one, a quarter of those two
		const nField = int(100) < 55 ? (int(3) === 0 ? 2 : 1) : 0;
		for (let i = 0; i < nField && slots.length; i++) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(fieldSetters);
		// weather moves: a third of the Pokemon have one, a quarter of those two
		const nWeather = int(100) < 34 ? (int(4) === 0 ? 2 : 1) : 0;
		for (let i = 0; i < nWeather && slots.length; i++) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(weatherMoves);
		// terrain moves: a third of the Pokemon have one
		const nTerrain = int(100) < 34 ? (int(4) === 0 ? 2 : 1) : 0;
		for (let i = 0; i < nTerrain && slots.length; i++) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(terrainMoves);
		// a priority move, a powder move, a Ground move that Grassy Terrain halves
		if (priorityMoves.length && slots.length && int(100) < 28) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(priorityMoves);
		if (powderMoves.length && slots.length && int(100) < 18) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(powderMoves);
		if (groundMoves.length && slots.length && int(100) < 16) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(groundMoves);
		// a paralysing move
		if (paralysers.length && slots.length && int(100) < 22) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(paralysers);
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
		const ab = int(100) < 25 ? 'noability' : pick(weightedAbilities);
		const it = int(100) < 20 ? '' : pick(weightedItems);
		s.ability = ab === 'noability' ? 'No Ability' : dex.abilities.get(ab).name;
		s.item = it === '' ? '' : dex.items.get(it).name;
		// Paradox holders often carry Booster Energy; Unburden holders often a seed (the item it loses)
		if (PARADOX.includes(ab) && pool.items.includes('boosterenergy') && int(100) < 45) s.item = dex.items.get('boosterenergy').name;
		if (ab === 'unburden' && int(100) < 55) s.item = dex.items.get(pick(SEEDS.filter(id => pool.items.includes(id)))).name;
	};
	for (const s of scenario.teams[0]) set(s);
	// in a mirror match the lead is a weather setter in a third of the battles: two setters tie on speed at turn 0
	if (mirror && setterAbilities.length && int(3) === 0) scenario.teams[0][0].ability = dex.abilities.get(pick(setterAbilities)).name;
	// ... or the lead is a listener: a Paradox ability or a seed, so two of them tie on speed at a WeatherChange or TerrainChange
	if (mirror && int(3) === 0) {
		const lead = scenario.teams[0][0];
		if (int(2) === 0 && pool.abilities.includes('protosynthesis')) lead.ability = dex.abilities.get(pick(PARADOX.filter(id => pool.abilities.includes(id)))).name;
		else if (pool.items.includes('grassyseed')) lead.item = dex.items.get(pick(SEEDS.filter(id => pool.items.includes(id)))).name;
	}
	if (mirror) scenario.teams[1] = JSON.parse(JSON.stringify(scenario.teams[0]));
	else for (const s of scenario.teams[1]) set(s);
	// the leads (the first Pokemon of each team, chosen by a scripted team order) are paired on purpose in about one battle in
	// six: a listener (a Protosynthesis or Quark Drive holder with Booster Energy, or a seed holder) that is FASTER than an
	// opposing setter of the field condition it listens to (Drought, Electric Surge, or the seed's surge), at distinct speeds.
	// At the turn-0 switch-in the two sit in one SwitchIn event, where the listener's onSwitchInPriority (-2 for the ability and
	// Booster Energy, -1 for the seeds) is what makes it run after the setter: with it Drought sets sun first and Booster Energy
	// is kept; without it the faster holder runs first and Booster Energy is used
	const pairing = [];
	if (pool.abilities.includes('protosynthesis') && pool.abilities.includes('drought') && pool.items.includes('boosterenergy')) pairing.push(['protosynthesis', 'boosterenergy', 'drought']);
	if (pool.abilities.includes('quarkdrive') && pool.abilities.includes('electricsurge') && pool.items.includes('boosterenergy')) pairing.push(['quarkdrive', 'boosterenergy', 'electricsurge']);
	for (const [seed, surge] of [['electricseed', 'electricsurge'], ['grassyseed', 'grassysurge'], ['mistyseed', 'mistysurge'], ['psychicseed', 'psychicsurge']]) {
		if (pool.items.includes(seed) && pool.abilities.includes(surge)) pairing.push(['noability', seed, surge]);
	}
	if (pairing.length && int(100) < 16) {
		const [listenerAbility, listenerItem, setterAbility] = pick(pairing.concat(pairing.slice(0, 2), pairing.slice(0, 2)));
		const listenerSide = int(2);
		const L = scenario.teams[listenerSide][0];
		const S = JSON.parse(JSON.stringify(L));
		scenario.teams[1 - listenerSide][0] = S;
		L.level = 100; S.level = 50;
		L.ability = listenerAbility === 'noability' ? 'No Ability' : dex.abilities.get(listenerAbility).name;
		L.item = dex.items.get(listenerItem).name;
		S.ability = dex.abilities.get(setterAbility).name;
		S.item = '';
		scenario.choices = [['team 123456', 'team 123456']];
	} else if (mirror && (scenario.teams[0][0].ability !== 'No Ability' || scenario.teams[0][0].item !== '')) {
		// a mirror match with a special lead: the same scripted team order, so the lead is the one that was set up
		scenario.choices = [['team 123456', 'team 123456']];
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally: observation hooks on Showdown's Battle class.

let activeTally = null;
function newTally() {
	return {
		hazardSet: { stealthrock: 0, spikes: 0, toxicspikes: 0, stickyweb: 0 },
		spikesSetToLayer2: 0, spikesSetToLayer3: 0, toxicSpikesSetToLayer2: 0, restartRefused: 0,
		stealthRockDamage: 0, stealthRockKO: 0,
		spikesDamageLayer1: 0, spikesDamageLayer2: 0, spikesDamageLayer3: 0, spikesKO: 0,
		spikesSkippedByFlying: 0,
		toxicSpikesAbsorbed: 0, toxicSpikesPoison: 0, toxicSpikesBadPoison: 0,
		toxicSpikesSkippedBySteel: 0, toxicSpikesAlreadyStatused: 0, toxicSpikesSkippedByFlying: 0,
		stickyWebDrop: 0,
		bootsBlock: { stealthrock: 0, spikes: 0, toxicspikes: 0, stickyweb: 0 },
		bootsOnPoisonAbsorb: 0,
		toxicDebrisSet: 0, toxicDebrisSecondLayer: 0, toxicDebrisAtTwoLayers: 0,
		koByHazardOnSwitchIn: 0,
		twoHazardsOnOneSwitchIn: 0, threeHazardsOnOneSwitchIn: 0,
		hazardOrders: {},
		poisonKilledBeforeToxicSpikesAbsorb: 0,
		switchInSpeedTie: 0, switchInSpeedTieWithHazards: 0,
		screenSet: { reflect: 0, lightscreen: 0 }, lightClayScreen8: 0, screenWithoutLightClay5: 0,
		screenReducedDamage: { reflect: 0, lightscreen: 0 },
		screenIgnoredByCrit: { reflect: 0, lightscreen: 0 },
		screenOtherCategoryUnchanged: 0,
		bothScreensOnTarget: 0, screensOnBothSides: 0,
		screenExpired: { reflect: 0, lightscreen: 0 }, screenRestartRefused: 0,
		residualTieAcrossSides: 0,
		tailwindSet: 0, tailwindExpired: 0, tailwindSpeedDoubled: 0, tailwindWithParalysis: 0, tailwindRestartRefused: 0,
		trickRoomSet: 0, trickRoomRestartEnded: 0, trickRoomExpired: 0,
		trickRoomSwitchInSpeedTie: 0, trickRoomMoveSpeedTie: 0, trickRoomTurns: 0,
		weatherSetByMove: { sunnyday: 0, raindance: 0, sandstorm: 0, snowscape: 0 },
		weatherSetByAbility: { sunnyday: 0, raindance: 0, sandstorm: 0, snowscape: 0 },
		weatherReplaced: 0, weatherRefusedByAbility: 0, weatherRefusedByMove: 0,
		weatherExpired: { sunnyday: 0, raindance: 0, sandstorm: 0, snowscape: 0 }, weatherSetAfterExpiry: 0,
		weatherRock8: { sunnyday: 0, raindance: 0, sandstorm: 0, snowscape: 0 },
		weatherNoRock5: { sunnyday: 0, raindance: 0, sandstorm: 0, snowscape: 0 },
		leadSettersBothSet: 0, leadSettersTieOnSpeed: 0,
		sandChip: 0, sandKO: 0, sandImmuneRock: 0, sandImmuneGround: 0, sandImmuneSteel: 0, sandRushImmune: 0,
		sunFireBoost: 0, sunWaterSuppress: 0, rainWaterBoost: 0, rainFireSuppress: 0, weatherDamageOtherType: 0,
		sandSpDBoost: 0, snowDefBoost: 0,
		sunBlocksFreeze: 0, iceTypeFreezeImmune: 0,
		swiftSwimDoubled: 0, chlorophyllDoubled: 0, sandRushDoubled: 0, slushRushDoubled: 0,
		weatherSpeedWithTailwind: 0, weatherSpeedUnderTrickRoom: 0,
		hydrationCure: 0, hydrationRainNoStatus: 0, hydrationStatusNoRain: 0,
		weatherEventSpeedTie: 0, weatherChangeSpeedTie: 0,
		terrainSetByMove: { electricterrain: 0, grassyterrain: 0, mistyterrain: 0, psychicterrain: 0 },
		terrainSetByAbility: { electricterrain: 0, grassyterrain: 0, mistyterrain: 0, psychicterrain: 0 },
		terrainReplaced: 0, terrainRefused: 0,
		terrainExpired: { electricterrain: 0, grassyterrain: 0, mistyterrain: 0, psychicterrain: 0 }, terrainSetAfterExpiry: 0,
		terrainExtender8: { electricterrain: 0, grassyterrain: 0, mistyterrain: 0, psychicterrain: 0 },
		terrainNoExtender5: { electricterrain: 0, grassyterrain: 0, mistyterrain: 0, psychicterrain: 0 },
		leadSurgesBothSet: 0, leadSurgesTieOnSpeed: 0, leadWeatherAndTerrainSetters: 0,
		terrainChangeSpeedTie: 0, terrainAndWeatherResidual: 0,
		electricBoost: 0, electricUngroundedUnboosted: 0,
		grassyBoost: 0, grassyEarthquakeHalved: 0, grassyBulldozeHalved: 0, grassyHeal: 0, grassyHealFullHp: 0,
		grassyHealSkippedUngrounded: 0, grassyHealOnStatusedMon: 0,
		mistyStatusBlocked: 0, mistyStatusAllowedUngrounded: 0, mistyDragonHalved: 0,
		psychicBlocksPriority: 0, psychicPriorityAllowedUngrounded: 0, psychicNonPriorityAllowed: 0, psychicBoost: 0,
		overcoatPowderBlocked: 0, overcoatSandImmune: 0, grassTypePowderImmune: 0, powderMoveHit: 0,
		leafGuardBlocksStatus: 0, leafGuardStatusAllowedNoSun: 0, statusUnderSunWithoutLeafGuard: 0,
		paradoxStarted: { protoFromAbility: 0, protoFromBooster: 0, quarkFromAbility: 0, quarkFromBooster: 0 },
		paradoxBestStat: { atk: 0, def: 0, spa: 0, spd: 0, spe: 0 }, paradoxBestStatTie: 0,
		paradoxStatBoost: { atk: 0, def: 0, spa: 0, spd: 0, spe: 0 },
		paradoxRemovedByFieldChange: 0, paradoxFromBoosterKept: 0, boosterThenFieldArrives: 0,
		boosterEnergyUsed: { proto: 0, quark: 0 }, boosterUsedWithNoField: 0, boosterKeptInSun: 0,
		seedUsed: { electricseed: 0, grassyseed: 0, mistyseed: 0, psychicseed: 0 },
		seedUsedAtSwitchIn: 0, seedUsedOnTerrainChange: 0,
		unburdenAfterSeed: 0, unburdenSpeedDoubled: 0,
		// (no Unburden-after-Booster-Energy counter: Booster Energy is used only by a Protosynthesis or Quark Drive holder, and a
		// Pokemon has one ability, so it can never also have Unburden: structurally zero)
		listenerTieWeatherChange: 0, listenerTieTerrainChange: 0,
		fasterListenerVsSetter: { proto: 0, quark: 0, seed: 0 },
		boosterKeptBecauseSetterFirst: { proto: 0, quark: 0 },
	};
}

function install() {
	if (Battle.prototype.__slice3Instrumented) return;
	Battle.prototype.__slice3Instrumented = true;
	const t = () => activeTally;
	// per fieldEvent('SwitchIn'): for each switching Pokemon the hazards that ran and the ones that changed something
	let switchIn = null;
	let inResidual = false;

	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, ...rest) {
		const c = t();
		if (!c || !effect) return singleEvent.call(this, eventid, effect, state, target, ...rest);
		if ((eventid === 'SideStart' || eventid === 'SideRestart' || eventid === 'SideEnd') && (SCREENS.includes(effect.id) || effect.id === 'tailwind')) {
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			if (eventid === 'SideStart') {
				if (effect.id === 'tailwind') c.tailwindSet++;
				else {
					c.screenSet[effect.id]++;
					if (state.duration === 8) c.lightClayScreen8++;
					else if (state.duration === 5 && rest[0] && rest[0].item !== 'lightclay') c.screenWithoutLightClay5++;
				}
			} else if (eventid === 'SideEnd') {
				if (effect.id === 'tailwind') c.tailwindExpired++; else c.screenExpired[effect.id]++;
			}
			return r;
		}
		if (eventid === 'FieldStart' && effect.id === 'trickroom') {
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			c.trickRoomSet++;
			return r;
		}
		if (eventid === 'FieldRestart' && effect.id === 'trickroom') {
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			c.trickRoomRestartEnded++;
			return r;
		}
		if (eventid === 'FieldEnd' && effect.id === 'trickroom') {
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			if (inResidual) c.trickRoomExpired++;
			return r;
		}
		if ((eventid === 'SideStart' || eventid === 'SideRestart') && HAZARDS.includes(effect.id)) {
			const layersBefore = state && state.layers;
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			if (eventid === 'SideStart') c.hazardSet[effect.id]++;
			else if (r === false) c.restartRefused++;
			else if (effect.id === 'spikes') { if (state.layers === 2) c.spikesSetToLayer2++; if (state.layers === 3) c.spikesSetToLayer3++; }
			else if (effect.id === 'toxicspikes' && state.layers === 2 && layersBefore === 1) c.toxicSpikesSetToLayer2++;
			return r;
		}
		if (eventid !== 'SwitchIn' || !HAZARDS.includes(effect.id) || !target || target.hp === undefined) {
			return singleEvent.call(this, eventid, effect, state, target, ...rest);
		}
		const hp = target.hp, status = target.status, spe = target.boosts.spe, layers = state.layers;
		const types = target.types, boots = target.item === 'heavydutyboots';
		const flies = types.includes('Flying');
		const sideHad = !!target.side.sideConditions[effect.id];
		const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
		let acted = false;
		switch (effect.id) {
		case 'stealthrock':
			if (boots) c.bootsBlock.stealthrock++;
			else if (target.hp < hp) { c.stealthRockDamage++; acted = true; if (target.hp === 0) c.stealthRockKO++; }
			break;
		case 'spikes':
			if (flies) c.spikesSkippedByFlying++;
			else if (boots) c.bootsBlock.spikes++;
			else if (target.hp < hp) {
				c['spikesDamageLayer' + layers]++; acted = true;
				if (target.hp === 0) c.spikesKO++;
			}
			break;
		case 'toxicspikes':
			if (flies) c.toxicSpikesSkippedByFlying++;
			else if (types.includes('Poison')) {
				if (sideHad && !target.side.sideConditions.toxicspikes) { c.toxicSpikesAbsorbed++; acted = true; if (boots) c.bootsOnPoisonAbsorb++; }
			} else if (types.includes('Steel')) c.toxicSpikesSkippedBySteel++;
			else if (boots) c.bootsBlock.toxicspikes++;
			else if (status) c.toxicSpikesAlreadyStatused++;
			else if (target.status === 'psn') { c.toxicSpikesPoison++; acted = true; }
			else if (target.status === 'tox') { c.toxicSpikesBadPoison++; acted = true; }
			break;
		case 'stickyweb':
			if (flies) break;
			if (boots) c.bootsBlock.stickyweb++;
			else if (target.boosts.spe < spe) { c.stickyWebDrop++; acted = true; }
			// (a Pokemon enters at stage 0, so the drop always lands: no counter for a failed one)
			break;
		default:
		}
		if (switchIn) {
			const e = switchIn.get(target);
			if (e) { e.ran.push(effect.id); if (acted) e.acted.push(effect.id); if (target.hp === 0 && hp > 0) e.killedBy = effect.id; }
		}
		return r;
	};

	const fieldEvent = Battle.prototype.fieldEvent;
	Battle.prototype.fieldEvent = function (eventid, targets, ...rest) {
		const c = t();
		if (c && eventid === 'Residual') {
			if (this.field.pseudoWeather.trickroom) c.trickRoomTurns++;
			inResidual = true;
			try { return fieldEvent.call(this, eventid, targets, ...rest); } finally { inResidual = false; }
		}
		if (!c || eventid !== 'SwitchIn' || !targets) return fieldEvent.call(this, eventid, targets, ...rest);
		const outer = switchIn;
		switchIn = new Map();
		for (const p of targets) {
			switchIn.set(p, { had: Object.keys(p.side.sideConditions).filter(id => HAZARDS.includes(id)), ran: [], acted: [], killedBy: null });
		}
		const mine = switchIn;
		const r = fieldEvent.call(this, eventid, targets, ...rest);
		for (const [p, e] of mine) {
			if (e.acted.length >= 2) {
				c.twoHazardsOnOneSwitchIn++;
				if (e.acted.length >= 3) c.threeHazardsOnOneSwitchIn++;
				const key = e.acted.join('>');
				c.hazardOrders[key] = (c.hazardOrders[key] || 0) + 1;
			}
			if (e.killedBy) c.koByHazardOnSwitchIn++;
			if (e.had.includes('toxicspikes') && p.hp === 0 && p.types.includes('Poison') && !p.types.includes('Flying') &&
				!e.ran.includes('toxicspikes')) c.poisonKilledBeforeToxicSpikesAbsorb++;
		}
		switchIn = outer;
		return r;
	};

	// Toxic Debris: the attacker's Toxic Spikes layers around runEvent('DamagingHit', ...)
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, ...rest) {
		const c = t();
		const holder = Array.isArray(target) ? target[0] : target;
		if (c && eventid === 'ModifyDamage' && source && source.side && rest[0]) {
			// the attacker is the target and the defender the source
			const sc = source.side.sideConditions, move = rest[0];
			const r = runEvent.call(this, eventid, target, source, ...rest);
			const crit = source.getMoveHitData(move).crit;
			const cat = move.category;
			const both = !!(sc.reflect && sc.lightscreen);
			if (both) c.bothScreensOnTarget++;
			if (Object.keys(this.sides[0].sideConditions).some(id => SCREENS.includes(id)) &&
				Object.keys(this.sides[1].sideConditions).some(id => SCREENS.includes(id))) c.screensOnBothSides++;
			for (const [id, category] of [['reflect', 'Physical'], ['lightscreen', 'Special']]) {
				if (!sc[id]) continue;
				if (cat !== category) { if (!sc[id === 'reflect' ? 'lightscreen' : 'reflect']) c.screenOtherCategoryUnchanged++; continue; }
				if (crit) c.screenIgnoredByCrit[id]++;
				// a drop attributable to the screen: the defender (the event's source) has no other damage reducer
				// (Filter, Solid Rock and Prism Armor are the supported ones; the attacker's items only raise damage)
				else if (r < rest[1] && !['filter', 'solidrock', 'prismarmor'].includes(source.ability)) c.screenReducedDamage[id]++;
			}
			return r;
		}
		if (c && eventid === 'ModifySpe' && holder && holder.side && holder.side.sideConditions.tailwind) {
			const r = runEvent.call(this, eventid, target, source, ...rest);
			if (r > rest[1]) c.tailwindSpeedDoubled++;
			if (holder.status === 'par') c.tailwindWithParalysis++;
			return r;
		}
		if (!c || eventid !== 'DamagingHit' || !holder || holder.ability !== 'toxicdebris' || !source || !source.side) {
			return runEvent.call(this, eventid, target, source, ...rest);
		}
		const ts = source.side.sideConditions.toxicspikes;
		const before = ts ? ts.layers : 0;
		const r = runEvent.call(this, eventid, target, source, ...rest);
		const after = source.side.sideConditions.toxicspikes ? source.side.sideConditions.toxicspikes.layers : 0;
		if (after > before) { c.toxicDebrisSet++; if (before === 1) c.toxicDebrisSecondLayer++; }
		else if (before >= 2 && rest[0] && rest[0].category === 'Physical') c.toxicDebrisAtTwoLayers++;
		return r;
	};

	// a screen or Tailwind used again while in place: addSideCondition returns false (no onSideRestart)
	const addSideCondition = Side.prototype.addSideCondition;
	Side.prototype.addSideCondition = function (status, ...rest) {
		const c = t();
		const id = typeof status === 'string' ? status : status.id;
		if (!c || !(SCREENS.includes(id) || id === 'tailwind') || !this.sideConditions[id]) return addSideCondition.call(this, status, ...rest);
		const r = addSideCondition.call(this, status, ...rest);
		if (r === false) { if (id === 'tailwind') c.tailwindRestartRefused++; else c.screenRestartRefused++; }
		return r;
	};

	installWeather(t);
	installTerrain(t);
	installParadox(t);

	// speed ties among the Pokemon in play when they switch in (runSwitch's speedSort of getAllActive)
	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, ...rest) {
		const c = t();
		if (c && list.length >= 2 && list[0]) {
			const tr = !!this.field.pseudoWeather.trickroom;
			if (list.length === 2 && typeof list[0].getStat === 'function' && list[0].speed === list[1].speed) {
				c.switchInSpeedTie++;
				if (list.some(p => Object.keys(p.side.sideConditions).some(id => HAZARDS.includes(id)))) c.switchInSpeedTieWithHazards++;
				if (tr) c.trickRoomSwitchInSpeedTie++;
			}
			// the move actions of a turn: equal priority and speed under Trick Room
			if (tr && list.length === 2 && list[0].choice === 'move' && list[1].choice === 'move' &&
				list[0].priority === list[1].priority && list[0].speed === list[1].speed) c.trickRoomMoveSpeedTie++;
			// two Residual handlers of side conditions on different sides that tie on every key
			const hs = list.filter(h => h.effect && h.effectHolder && h.effectHolder.sideConditions);
			for (let i = 0; i < hs.length; i++) {
				for (let j = i + 1; j < hs.length; j++) {
					const a = hs[i], b = hs[j];
					if (a.effectHolder !== b.effectHolder && a.order === b.order && a.priority === b.priority &&
						a.subOrder === b.subOrder && a.speed === b.speed && a.order === 26) c.residualTieAcrossSides++;
				}
			}
		}
		return speedSort.call(this, list, ...rest);
	};
}

// The weather observations (3c): all observation only.
function installWeather(t) {
	const wmap = new WeakMap(); // per battle: has a weather expired
	const Field = new Battle({ formatid: 'gen9ou' }).field.constructor;
	const setWeather = Field.prototype.setWeather;
	Field.prototype.setWeather = function (status, source, sourceEffect) {
		const c = t();
		if (!c) return setWeather.call(this, status, source, sourceEffect);
		const id = typeof status === 'string' ? status.toLowerCase().replace(/[^a-z0-9]/g, '') : status.id;
		const prev = this.weather;
		const eff = sourceEffect || this.battle.effect;
		const byAbility = !!eff && eff.effectType === 'Ability';
		const r = setWeather.call(this, status, source, sourceEffect);
		if (prev === id && r === false) {
			if (byAbility) c.weatherRefusedByAbility++; else c.weatherRefusedByMove++;
		} else if (r === true && WEATHERS.includes(id)) {
			(byAbility ? c.weatherSetByAbility : c.weatherSetByMove)[id]++;
			if (prev && prev !== id) c.weatherReplaced++;
			if (!prev && wmap.get(this.battle)) c.weatherSetAfterExpiry++;
			if (this.weatherState.duration === 8) c.weatherRock8[id]++; else if (this.weatherState.duration === 5) c.weatherNoRock5[id]++;
		}
		return r;
	};

	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, ...rest) {
		const c = t();
		if (!c || !effect) return singleEvent.call(this, eventid, effect, state, target, ...rest);
		if (eventid === 'FieldEnd' && WEATHERS.includes(effect.id)) {
			c.weatherExpired[effect.id]++;
			wmap.set(this, true);
		}
		if (eventid === 'Residual' && effect.id === 'hydration' && target && target.hp !== undefined) {
			const status = target.status, w = this.field.weather;
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			if (status && w === 'raindance') { if (!target.status) c.hydrationCure++; } else if (!status && w === 'raindance') c.hydrationRainNoStatus++;
			else if (status) c.hydrationStatusNoRain++;
			return r;
		}
		return singleEvent.call(this, eventid, effect, state, target, ...rest);
	};

	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, ...rest) {
		const c = t();
		const holder = Array.isArray(target) ? target[0] : target;
		if (!c || !holder || holder.hp === undefined) return runEvent.call(this, eventid, target, source, ...rest);
		const w = this.field.weather;
		if (eventid === 'Weather' && w === 'sandstorm') {
			const hp = holder.hp, types = holder.types;
			const r = runEvent.call(this, eventid, target, source, ...rest);
			if (hp > 0) {
				if (holder.hp < hp) { c.sandChip++; if (holder.hp === 0) c.sandKO++; }
				// the type is checked first: an ability is credited only where it is what made the Pokemon immune
				else if (types.includes('Rock')) c.sandImmuneRock++;
				else if (types.includes('Ground')) c.sandImmuneGround++;
				else if (types.includes('Steel')) c.sandImmuneSteel++;
				else if (holder.ability === 'sandrush') c.sandRushImmune++;
				else if (holder.ability === 'overcoat') c.overcoatSandImmune++;
			}
			return r;
		}
		if (eventid === 'WeatherModifyDamage' && source && rest[0] && typeof rest[1] === 'number') {
			const r = runEvent.call(this, eventid, target, source, ...rest);
			const type = rest[0].type;
			if (w === 'sunnyday' && type === 'Fire') { if (r > rest[1]) c.sunFireBoost++; }
			else if (w === 'sunnyday' && type === 'Water') { if (r < rest[1]) c.sunWaterSuppress++; }
			else if (w === 'raindance' && type === 'Water') { if (r > rest[1]) c.rainWaterBoost++; }
			else if (w === 'raindance' && type === 'Fire') { if (r < rest[1]) c.rainFireSuppress++; }
			else if (w) c.weatherDamageOtherType++;
			return r;
		}
		if ((eventid === 'ModifySpD' || eventid === 'ModifyDef') && typeof rest[1] === 'number') {
			const r = runEvent.call(this, eventid, target, source, ...rest);
			if (eventid === 'ModifySpD' && w === 'sandstorm' && holder.types.includes('Rock') && r > rest[1]) c.sandSpDBoost++;
			if (eventid === 'ModifyDef' && w === 'snowscape' && holder.types.includes('Ice') && r > rest[1]) c.snowDefBoost++;
			return r;
		}
		if (eventid === 'ModifySpe' && typeof rest[1] === 'number' && ['swiftswim', 'chlorophyll', 'sandrush', 'slushrush'].includes(holder.ability)) {
			const r = runEvent.call(this, eventid, target, source, ...rest);
			const ability = holder.ability;
			const doubled = ability === 'swiftswim' ? w === 'raindance' : ability === 'chlorophyll' ? w === 'sunnyday' :
				ability === 'sandrush' ? w === 'sandstorm' : w === 'snowscape';
			if (doubled && r > rest[1]) {
				c[{ swiftswim: 'swiftSwimDoubled', chlorophyll: 'chlorophyllDoubled', sandrush: 'sandRushDoubled', slushrush: 'slushRushDoubled' }[ability]]++;
				if (holder.side.sideConditions.tailwind) c.weatherSpeedWithTailwind++;
				if (this.field.pseudoWeather.trickroom) c.weatherSpeedUnderTrickRoom++;
			}
			return r;
		}
		return runEvent.call(this, eventid, target, source, ...rest);
	};

	// the Immunity event on a status: Sunny Day blocks freezing, and an Ice type is immune anyway
	const Pokemon = require(path.join(SHOWDOWN_DIR, 'dist', 'sim')).Pokemon;
	const runStatusImmunity = Pokemon.prototype.runStatusImmunity;
	Pokemon.prototype.runStatusImmunity = function (type, message) {
		const r = runStatusImmunity.call(this, type, message);
		const c = t();
		if (c && type === 'frz' && !r && !this.fainted) {
			if (this.types.includes('Ice')) c.iceTypeFreezeImmune++;
			else if (this.battle.field.weather === 'sunnyday') c.sunBlocksFreeze++;
		}
		return r;
	};

	// weather events sort the Pokemon in play by speed: equal speeds draw
	const eachEvent = Battle.prototype.eachEvent;
	Battle.prototype.eachEvent = function (eventid, ...rest) {
		const c = t();
		if (c && (eventid === 'Weather' || eventid === 'WeatherChange')) {
			const a = this.getAllActive();
			if (a.length === 2 && a[0].speed === a[1].speed) { if (eventid === 'Weather') c.weatherEventSpeedTie++; else c.weatherChangeSpeedTie++; }
		}
		return eachEvent.call(this, eventid, ...rest);
	};

	// both leads are weather setters: they switch in together at turn 0
	const fieldEvent = Battle.prototype.fieldEvent;
	Battle.prototype.fieldEvent = function (eventid, targets, ...rest) {
		const c = t();
		if (c && eventid === 'SwitchIn' && targets && targets.length === 2 && targets.every(p => WEATHER_SETTERS.includes(p.ability))) {
			c.leadSettersBothSet++;
			if (targets[0].speed === targets[1].speed) c.leadSettersTieOnSpeed++;
		}
		return fieldEvent.call(this, eventid, targets, ...rest);
	};
}

// The terrain observations (3d): all observation only.
function installTerrain(t) {
	const tmap = new WeakMap(); // per battle: has a terrain expired
	const Field = new Battle({ formatid: 'gen9ou' }).field.constructor;
	const setTerrain = Field.prototype.setTerrain;
	Field.prototype.setTerrain = function (status, source, sourceEffect) {
		const c = t();
		if (!c) return setTerrain.call(this, status, source, sourceEffect);
		const id = typeof status === 'string' ? status.toLowerCase().replace(/[^a-z0-9]/g, '') : status.id;
		const prev = this.terrain;
		const eff = sourceEffect || this.battle.effect;
		const byAbility = !!eff && eff.effectType === 'Ability';
		const r = setTerrain.call(this, status, source, sourceEffect);
		if (prev === id && r === false) c.terrainRefused++;
		else if (r === true && TERRAINS.includes(id)) {
			(byAbility ? c.terrainSetByAbility : c.terrainSetByMove)[id]++;
			if (prev && prev !== id) c.terrainReplaced++;
			if (!prev && tmap.get(this.battle)) c.terrainSetAfterExpiry++;
			if (this.terrainState.duration === 8) c.terrainExtender8[id]++; else if (this.terrainState.duration === 5) c.terrainNoExtender5[id]++;
		}
		return r;
	};

	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, ...rest) {
		const c = t();
		if (c && effect && eventid === 'FieldEnd' && TERRAINS.includes(effect.id)) { c.terrainExpired[effect.id]++; tmap.set(this, true); }
		if (c && effect && eventid === 'Residual' && effect.id === 'grassyterrain' && target && target.hp !== undefined) {
			const hp = target.hp, status = target.status, flies = target.types.includes('Flying');
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			if (hp > 0) {
				if (target.hp > hp) { c.grassyHeal++; if (status === 'psn' || status === 'tox' || status === 'brn') c.grassyHealOnStatusedMon++; }
				else if (flies) c.grassyHealSkippedUngrounded++;
				else c.grassyHealFullHp++;
			}
			return r;
		}
		return singleEvent.call(this, eventid, effect, state, target, ...rest);
	};

	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, ...rest) {
		const c = t();
		const holder = Array.isArray(target) ? target[0] : target;
		if (!c || !holder || holder.hp === undefined) return runEvent.call(this, eventid, target, source, ...rest);
		const terrain = this.field.terrain, w = this.field.weather;
		if (eventid === 'BasePower' && typeof rest[1] === 'number' && source && rest[0]) {
			// the attacker is the target and the defender the source
			const r = runEvent.call(this, eventid, target, source, ...rest);
			const move = rest[0], type = move.type;
			const attackerFlies = holder.types.includes('Flying'), defenderFlies = source.types.includes('Flying');
			if (terrain === 'electricterrain' && type === 'Electric') { if (attackerFlies) c.electricUngroundedUnboosted++; else if (r > rest[1]) c.electricBoost++; }
			if (terrain === 'psychicterrain' && type === 'Psychic' && !attackerFlies && r > rest[1]) c.psychicBoost++;
			if (terrain === 'grassyterrain') {
				if (move.id === 'earthquake' && !defenderFlies && r < rest[1]) c.grassyEarthquakeHalved++;
				else if (move.id === 'bulldoze' && !defenderFlies && r < rest[1]) c.grassyBulldozeHalved++;
				else if (type === 'Grass' && !attackerFlies && r > rest[1]) c.grassyBoost++;
			}
			if (terrain === 'mistyterrain' && type === 'Dragon' && !defenderFlies && r < rest[1]) c.mistyDragonHalved++;
			return r;
		}
		if (eventid === 'TryHit' && Array.isArray(target) && source && rest[0]) {
			const move = rest[0];
			const r = runEvent.call(this, eventid, target, source, ...rest);
			const blocked = !r[0] || r[0] === null;
			if (terrain === 'psychicterrain' && move.priority > 0 && holder !== source) {
				if (blocked) c.psychicBlocksPriority++; else if (holder.types.includes('Flying')) c.psychicPriorityAllowedUngrounded++;
			} else if (terrain === 'psychicterrain' && move.priority <= 0 && !blocked) c.psychicNonPriorityAllowed++;
			if (move.flags.powder && holder !== source) {
				if (holder.ability === 'overcoat' && blocked) c.overcoatPowderBlocked++;
				else if (holder.types.includes('Grass')) c.grassTypePowderImmune++;
				else if (!blocked) c.powderMoveHit++;
			}
			return r;
		}
		if (eventid === 'SetStatus') {
			const r = runEvent.call(this, eventid, target, source, ...rest);
			const flies = holder.types.includes('Flying');
			if (terrain === 'mistyterrain') { if (!r && !flies) c.mistyStatusBlocked++; else if (r && flies) c.mistyStatusAllowedUngrounded++; }
			if (w === 'sunnyday') {
				if (holder.ability === 'leafguard') { if (!r) c.leafGuardBlocksStatus++; }
				else if (r) c.statusUnderSunWithoutLeafGuard++;
			} else if (holder.ability === 'leafguard' && r) c.leafGuardStatusAllowedNoSun++;
			return r;
		}
		return runEvent.call(this, eventid, target, source, ...rest);
	};

	const eachEvent = Battle.prototype.eachEvent;
	Battle.prototype.eachEvent = function (eventid, ...rest) {
		const c = t();
		if (c && eventid === 'TerrainChange') {
			const a = this.getAllActive();
			if (a.length === 2 && a[0].speed === a[1].speed) c.terrainChangeSpeedTie++;
		}
		return eachEvent.call(this, eventid, ...rest);
	};

	// weather and terrain in place together at the Residual event
	const fieldEvent = Battle.prototype.fieldEvent;
	Battle.prototype.fieldEvent = function (eventid, targets, ...rest) {
		const c = t();
		if (c && eventid === 'Residual' && this.field.weather && this.field.terrain) c.terrainAndWeatherResidual++;
		if (c && eventid === 'SwitchIn' && targets && targets.length === 2) {
			const surge = p => TERRAIN_SETTERS.includes(p.ability), weather = p => WEATHER_SETTERS.includes(p.ability);
			if (targets.every(surge)) {
				c.leadSurgesBothSet++;
				if (targets[0].speed === targets[1].speed) c.leadSurgesTieOnSpeed++;
			}
			if (targets.some(surge) && targets.some(weather)) c.leadWeatherAndTerrainSetters++;
		}
		return fieldEvent.call(this, eventid, targets, ...rest);
	};
}

// The Paradox, item-use and Unburden observations (3e): all observation only.
function installParadox(t) {
	const STATS = ['atk', 'def', 'spa', 'spd', 'spe'];
	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, ...rest) {
		const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
		const c = t();
		if (c && effect && eventid === 'Start' && (effect.id === 'protosynthesis' || effect.id === 'quarkdrive') && state && target && target.hp !== undefined) {
			const key = (effect.id === 'protosynthesis' ? 'proto' : 'quark') + (state.fromBooster ? 'FromBooster' : 'FromAbility');
			c.paradoxStarted[key]++;
			// getStat with unmodified true runs no event
			const stats = STATS.map(i => target.getStat(i, false, true));
			const best = Math.max(...stats);
			c.paradoxBestStat[state.bestStat]++;
			if (stats.filter(v => v === best).length > 1) c.paradoxBestStatTie++;
		}
		return r;
	};

	const removeVolatile = Pokemon.prototype.removeVolatile;
	Pokemon.prototype.removeVolatile = function (status) {
		const r = removeVolatile.call(this, status);
		const c = t();
		const id = typeof status === 'string' ? status : status.id;
		if (c && r && (id === 'protosynthesis' || id === 'quarkdrive') && this.battle.event &&
			(this.battle.event.id === 'WeatherChange' || this.battle.event.id === 'TerrainChange')) c.paradoxRemovedByFieldChange++;
		return r;
	};

	const useItem = Pokemon.prototype.useItem;
	Pokemon.prototype.useItem = function (...args) {
		const c = t();
		const item = this.item, ability = this.ability, ev = this.battle.event && this.battle.event.id;
		const noField = !this.battle.field.weather && !this.battle.field.terrain;
		const r = useItem.call(this, ...args);
		if (!c || !r) return r;
		if (SEEDS.includes(item)) {
			c.seedUsed[item]++;
			if (ev === 'SwitchIn') c.seedUsedAtSwitchIn++; else if (ev === 'TerrainChange') c.seedUsedOnTerrainChange++;
		}
		if (item === 'boosterenergy') {
			if (ability === 'protosynthesis') c.boosterEnergyUsed.proto++; else if (ability === 'quarkdrive') c.boosterEnergyUsed.quark++;
			if (noField) c.boosterUsedWithNoField++;
		}
		if (ability === 'unburden' && SEEDS.includes(item)) c.unburdenAfterSeed++;
		return r;
	};

	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, ...rest) {
		const c = t();
		const holder = Array.isArray(target) ? target[0] : target;
		if (!c || !holder || holder.hp === undefined || !holder.volatiles) return runEvent.call(this, eventid, target, source, ...rest);
		if (eventid === 'WeatherChange' || eventid === 'TerrainChange') {
			const key = eventid === 'WeatherChange' ? 'protosynthesis' : 'quarkdrive';
			const before = holder.volatiles[key];
			const r = runEvent.call(this, eventid, target, source, ...rest);
			if (before && before.fromBooster) {
				const active = eventid === 'WeatherChange' ? this.field.weather === 'sunnyday' : this.field.terrain === 'electricterrain';
				if (holder.volatiles[key]) { if (active) c.boosterThenFieldArrives++; else c.paradoxFromBoosterKept++; }
			}
			return r;
		}
		const statOf = { ModifyAtk: 'atk', ModifyDef: 'def', ModifySpA: 'spa', ModifySpD: 'spd', ModifySpe: 'spe' }[eventid];
		if (statOf && typeof rest[1] === 'number') {
			const v = holder.volatiles.protosynthesis || holder.volatiles.quarkdrive;
			if (v && v.bestStat === statOf) {
				const r = runEvent.call(this, eventid, target, source, ...rest);
				if (r > rest[1]) c.paradoxStatBoost[statOf]++;
				return r;
			}
			if (eventid === 'ModifySpe' && holder.ability === 'unburden' && holder.volatiles.unburden && !holder.item) {
				const r = runEvent.call(this, eventid, target, source, ...rest);
				if (r > rest[1]) c.unburdenSpeedDoubled++;
				return r;
			}
		}
		if (eventid === 'Update' && holder.item === 'boosterenergy' && holder.ability === 'protosynthesis' && this.field.weather === 'sunnyday') {
			const r = runEvent.call(this, eventid, target, source, ...rest);
			if (holder.item === 'boosterenergy') c.boosterKeptInSun++;
			return r;
		}
		return runEvent.call(this, eventid, target, source, ...rest);
	};

	// a two-Pokemon switch-in with a faster listener against a slower setter of what it listens to: the listener's
	// onSwitchInPriority puts it after the setter. For Booster Energy the outcome is visible: the item is kept
	const fieldEvent = Battle.prototype.fieldEvent;
	Battle.prototype.fieldEvent = function (eventid, targets, ...rest) {
		const c = t();
		if (!c || eventid !== 'SwitchIn' || !targets || targets.length !== 2) return fieldEvent.call(this, eventid, targets, ...rest);
		const pairs = [];
		for (const [a, b] of [[targets[0], targets[1]], [targets[1], targets[0]]]) {
			if (a.speed <= b.speed) continue;
			if (a.item === 'boosterenergy' && a.ability === 'protosynthesis' && b.ability === 'drought') pairs.push(['proto', a]);
			else if (a.item === 'boosterenergy' && a.ability === 'quarkdrive' && b.ability === 'electricsurge') pairs.push(['quark', a]);
			else if (SEEDS.includes(a.item) && b.ability === { electricseed: 'electricsurge', grassyseed: 'grassysurge', mistyseed: 'mistysurge', psychicseed: 'psychicsurge' }[a.item]) pairs.push(['seed', a]);
		}
		const r = fieldEvent.call(this, eventid, targets, ...rest);
		for (const [kind, a] of pairs) {
			c.fasterListenerVsSetter[kind]++;
			if (kind !== 'seed' && a.item === 'boosterenergy') c.boosterKeptBecauseSetterFirst[kind]++;
		}
		return r;
	};

	// two listeners tie on speed at the same WeatherChange or TerrainChange
	const eachEvent = Battle.prototype.eachEvent;
	Battle.prototype.eachEvent = function (eventid, ...rest) {
		const c = t();
		if (c && (eventid === 'WeatherChange' || eventid === 'TerrainChange')) {
			const a = this.getAllActive();
			if (a.length === 2 && a[0].speed === a[1].speed) {
				if (eventid === 'WeatherChange' && a.every(p => p.ability === 'protosynthesis')) c.listenerTieWeatherChange++;
				if (eventid === 'TerrainChange' && a.every(p => p.ability === 'quarkdrive' || SEEDS.includes(p.item))) c.listenerTieTerrainChange++;
			}
		}
		return eachEvent.call(this, eventid, ...rest);
	};
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice3.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice3-${k}.json`), JSON.stringify(pool[k]));
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
		fs.writeFileSync(path.join(outDir, `slice3-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice3 seeds, ${written} traces written, ${truncated} truncated at the turn limit, ${pool.moves.length} moves, ${pool.species.length} species, ` +
		`${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error('tally (from Showdown\'s own events):');
	for (const [k, v] of Object.entries(tally)) console.error(`  ${k}: ${JSON.stringify(v)}`);
}

module.exports = { assign };
