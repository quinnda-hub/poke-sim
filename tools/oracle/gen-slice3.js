#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 3" (field state: 3a entry hazards, 3b screens, Tailwind and Trick Room) and
// records each on Showdown.
//
// Slice 3a adds side conditions (Stealth Rock, Spikes, Toxic Spikes, Sticky Web), Heavy-Duty Boots and Toxic Debris,
// and with them the effect order (`battle.effectOrder`) that sorts two hazards on one side. Slice 3b adds durations:
// Reflect, Light Screen (and Light Clay), Tailwind and Trick Room. The teams are slice 1's
// (gen-slice1.js makeScenario, so the move, species and spread draws are the same), then each Pokemon is changed by a
// generator of its own, so slices 1 and 2 stay reproducible from their seeds:
//   - a share of the Pokemon get a hazard setter in a move slot (two setters often, so a side holds two or three
//     hazards and `effectOrder` decides their order), and a share get a field setter (Reflect, Light Screen,
//     Tailwind, Trick Room: both screens on one side tie in ModifyDamage, a screen on each side tie in Residual);
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
	const abilities = pool.abilities.filter(id => id !== 'noability');
	const items = pool.items.filter(id => id !== '');
	const weightedAbilities = abilities.concat(abilities.filter(id => id === 'toxicdebris').flatMap(id => [id, id, id, id, id]));
	const weightedItems = items.concat(items.filter(id => id === 'heavydutyboots' || id === 'lightclay').flatMap(id => [id, id, id, id]));
	const mirror = JSON.stringify(scenario.teams[0]) === JSON.stringify(scenario.teams[1]);
	const set = (s) => {
		// species
		const k = int(100);
		if (k < 18 && flying.length) s.species = dex.species.get(pick(flying)).name;
		else if (k < 38 && poison.length) s.species = dex.species.get(pick(poison)).name;
		else if (k < 46 && steel.length) s.species = dex.species.get(pick(steel)).name;
		else if (k < 49 && pool.species.includes('shedinja')) s.species = 'Shedinja';
		// level: small Pokemon have little HP
		if (int(100) < 14) s.level = 1 + int(20);
		// hazard setters: one in two Pokemon has one, a quarter have two
		const nSetters = int(100) < 40 ? (int(2) === 0 ? 2 : 1) : 0;
		const slots = [0, 1, 2, 3];
		for (let i = 0; i < nSetters; i++) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(setters);
		// field setters: half of the Pokemon have one, a quarter of those two
		const nField = int(100) < 55 ? (int(3) === 0 ? 2 : 1) : 0;
		for (let i = 0; i < nField && slots.length; i++) s.moves[slots.splice(int(slots.length), 1)[0]] = pick(fieldSetters);
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
