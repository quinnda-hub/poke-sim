#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 3" (field state: 3a entry hazards, 3b screens, Tailwind and Trick Room, 3c
// weather) and records each on Showdown.
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
const { Dex, Battle, Side } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
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
	const weatherMoves = WEATHER_MOVES.filter(id => pool.moves.includes(id));
	if (weatherMoves.length === 0) throw new Error('the pool has no weather move');
	const abilities = pool.abilities.filter(id => id !== 'noability');
	const items = pool.items.filter(id => id !== '');
	const weightedAbilities = abilities.concat(abilities.filter(id => id === 'toxicdebris').flatMap(id => [id, id, id, id, id]))
		.concat(abilities.filter(id => WEATHER_SETTERS.includes(id) || WEATHER_SPEED.includes(id) || id === 'hydration').flatMap(id => [id, id, id]));
	const weightedItems = items.concat(items.filter(id => id === 'heavydutyboots' || id === 'lightclay').flatMap(id => [id, id, id, id]))
		.concat(items.filter(id => ROCKS.includes(id)).flatMap(id => [id, id, id]));
	const setterAbilities = abilities.filter(id => WEATHER_SETTERS.includes(id));
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
		// a paralysing move
		if (paralysers.length && slots.length && int(100) < 22) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(paralysers);
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) s.moves.push(pick(pool.moves));
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) s.moves.push(pick(pool.moves));
		const ab = int(100) < 25 ? 'noability' : pick(weightedAbilities);
		const it = int(100) < 20 ? '' : pick(weightedItems);
		s.ability = ab === 'noability' ? 'No Ability' : dex.abilities.get(ab).name;
		s.item = it === '' ? '' : dex.items.get(it).name;
	};
	for (const s of scenario.teams[0]) set(s);
	// in a mirror match the lead is a weather setter in a third of the battles: two setters tie on speed at turn 0
	if (mirror && setterAbilities.length && int(3) === 0) scenario.teams[0][0].ability = dex.abilities.get(pick(setterAbilities)).name;
	if (mirror) scenario.teams[1] = JSON.parse(JSON.stringify(scenario.teams[0]));
	else for (const s of scenario.teams[1]) set(s);
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
				else if (r < rest[1]) c.screenReducedDamage[id]++;
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
				else if (holder.ability === 'sandrush') c.sandRushImmune++;
				else if (types.includes('Rock')) c.sandImmuneRock++;
				else if (types.includes('Ground')) c.sandImmuneGround++;
				else if (types.includes('Steel')) c.sandImmuneSteel++;
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
	let turns = 0, draws = 0, truncated = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		const trace = record(scenario);
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice3-${seed}.json`), JSON.stringify(trace));
	}
	console.error(`${count} slice3 battles, ${pool.moves.length} moves, ${pool.species.length} species, ` +
		`${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws, ${truncated} truncated`);
	console.error('tally (from Showdown\'s own events):');
	for (const [k, v] of Object.entries(tally)) console.error(`  ${k}: ${JSON.stringify(v)}`);
}
