#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 5" (Terastallization without Stellar; phase 5a) and records each on Showdown.
//
// Every scenario is recorded with `tera: true`, so the policy is offered `move N terastallize` next to every move, and with a
// `teraChance` (0.1 to 0.5, by battle) so that a side terastallizes at varied turns instead of almost always on the first
// (record.js: the policy picks a plain choice uniformly, then takes the move's Tera variant with that probability). The teams
// are slice 3's (gen-slice1.js `makeScenario` under gen-slice3.js `assign`, so hazards, terrain, weather, screens, Trick Room,
// Flying, Poison and Steel types, Toxic Debris, Roost-less Ground moves and the rest come with them) and then each Pokemon gets
// a Tera type and a role from a generator of its own, so that slices 1 and 3 stay reproducible from their seeds.
//
// The Tera type: about half the sets draw it from the species' usage (data/usage/gen9ou.json `teraTypes`, a species the usage
// file does not list falls back to the next rule), the rest uniformly from the 18 types (Stellar is not supported until 5b).
//
// Roles (about two Pokemon in three have one):
//   - a Roost user (a Flying type or not), with a Tera type of Flying about two times in five: Roost under Terastallization, as a
//     Flying type (its volatile fails and the type stays) and as another type (its volatile fails; nothing to remove);
//   - a low-base-power move of the Pokemon's own Tera type (the 60 floor), a priority move or a multi-hit move (which the floor must
//     skip), or a base-power-callback move (Low Kick, Grass Knot, Heavy Slam at base power 0, Acrobatics at 55: the floor skips the
//     first and not the last); the Tera type is that move's type three times in four;
//   - a Tera Flying non-Flying Pokemon (Ground moves, Spikes, Toxic Spikes and terrain on the field), a Tera Poison or Steel one
//     (Toxic Spikes absorbed or skipped, status immunity), a Pokemon that is Flying, Poison or Steel and Terastallizes out of it;
//   - an Ogerpon, with a Tera type of Fire, Grass, Rock or Water (the forme change, Embody Aspect (Teal)) or another type (the
//     choice is accepted and nothing happens, on every later turn too), with the abilities of the pool, so that the old ability's
//     End (Protosynthesis, Quark Drive, Unburden) runs at the forme change.
// Battles by kind (from the seed): about one in eight is slice 0's Struggle profile with Tera types (Struggle after Terastallization; the Struggle request itself carries no Tera choice),
// about one in twelve a scripted double Terastallization on turn 1 (half of them in a mirror match, where the two sides tie on
// speed, so the two Tera actions tie and draw), one in twelve a scripted Trick Room turn followed by both leads Terastallizing under
// it, and one in twelve a scripted duel of the two leads in which the Tera type of one is a resistance or immunity to the other's
// move, with either side the faster.
//
// Usage:
//   node gen-slice5.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice5-<seed>.json, plus slice5-moves.json, slice5-species.json, slice5-abilities.json and slice5-items.json
// (copies of the pool, which the parity test compares with what the engine accepts). A tally of the outcomes that distinguish
// each part of the mechanic is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the
// wrappers pass every argument and result through and call nothing the battle would not call, and read stored fields, not
// getTypes or hasType).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { BattleActions } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'battle-actions'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');
const { makeScenario: makeScenario0 } = require('./gen-slice0.js');
const { assign: assignSlice3 } = require('./gen-slice3.js');

const dex = Dex.mod('gen9');
const USAGE = JSON.parse(fs.readFileSync(path.resolve(__dirname, '..', '..', 'data', 'usage', 'gen9ou.json'), 'utf8'));

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

const TYPES = [
	'Normal', 'Fire', 'Water', 'Electric', 'Grass', 'Ice', 'Fighting', 'Poison', 'Ground', 'Flying', 'Psychic', 'Bug', 'Rock', 'Ghost',
	'Dragon', 'Dark', 'Steel', 'Fairy',
];
const OGERPON_OK = ['Fire', 'Grass', 'Rock', 'Water'];
const OGERPON_NO = TYPES.filter(t => !OGERPON_OK.includes(t));
const GROUND_MOVES = ['earthquake', 'earthpower', 'bulldoze', 'drillrun', 'highhorsepower', 'mudshot', 'headlongrush'];
const STATUS_MOVES = ['toxic', 'willowisp', 'thunderwave', 'glare'];
const IDLE_MOVES = ['swordsdance', 'calmmind', 'dragondance', 'bulkup', 'agility', 'nastyplot', 'irondefense', 'amnesia', 'recover', 'roost'];
// (a move, a Tera type that is immune to it or resists it)
const RESISTED = [
	['earthquake', 'Flying'], ['bulldoze', 'Flying'], ['earthpower', 'Flying'], ['thunderbolt', 'Ground'], ['thunderwave', 'Ground'],
	['closecombat', 'Ghost'], ['tackle', 'Ghost'], ['shadowball', 'Normal'], ['flamethrower', 'Water'], ['icebeam', 'Steel'],
	['surf', 'Grass'], ['rockslide', 'Steel'], ['psychic', 'Dark'], ['dragonpulse', 'Fairy'], ['toxic', 'Steel'],
];

const hasMove = (pool, id) => pool.moves.includes(id);

// the usage-weighted Tera type of a species, or undefined (the species is not in the usage file)
function usageTera(speciesId, rnd) {
	const u = USAGE.species[speciesId];
	if (!u || !u.teraTypes) return undefined;
	const entries = Object.entries(u.teraTypes).filter(([t]) => TYPES.includes(t[0].toUpperCase() + t.slice(1)));
	const total = entries.reduce((n, [, w]) => n + w, 0);
	if (!(total > 0)) return undefined;
	let x = rnd() * total;
	for (const [t, w] of entries) {
		x -= w;
		if (x < 0) return t[0].toUpperCase() + t.slice(1);
	}
	const last = entries[entries.length - 1][0];
	return last[0].toUpperCase() + last.slice(1);
}

function assign(scenario, pool, seed) {
	const rnd = mulberry32(seed * 3266489917 + 668265263);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const only = (xs) => xs.filter(id => hasMove(pool, id));
	const kindRoll = int(100);
	const kind = kindRoll < 12 ? 'struggle' : kindRoll < 20 ? 'dual' : kindRoll < 28 ? 'trickroom' : kindRoll < 36 ? 'duel' : 'random';
	const teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	if (kind === 'struggle') {
		scenario = makeScenario0(seed, 'gen9ou', 'struggle');
	} else {
		assignSlice3(scenario, pool, seed);
	}
	scenario.tera = true;
	scenario.teraChance = teraChance;
	const mirror = JSON.stringify(scenario.teams[0]) === JSON.stringify(scenario.teams[1]);

	const move = (id) => dex.moves.get(id);
	const attacks = pool.moves.filter(id => move(id).category !== 'Status');
	// low-base-power moves the floor lifts, and those it skips
	const lowBp = attacks.filter(id => { const m = move(id); return m.basePower > 0 && m.basePower < 60 && m.priority <= 0 && !m.multihit && !m.basePowerCallback; });
	const lowPriority = attacks.filter(id => { const m = move(id); return m.basePower > 0 && m.basePower < 60 && m.priority > 0; });
	const multi = attacks.filter(id => { const m = move(id); return m.basePower > 0 && m.basePower < 60 && m.multihit && m.priority <= 0; });
	const callback = attacks.filter(id => move(id).basePowerCallback && (move(id).basePower === 0 || move(id).basePower === 150 || move(id).basePower < 60));
	const lowPool = [lowBp, lowBp, lowBp, lowPriority, multi, multi, callback].filter(xs => xs.length);
	const byType = (t) => pool.species.filter(id => dex.species.get(id).types.includes(t));
	const flying = byType('Flying');
	const nonFlying = pool.species.filter(id => !dex.species.get(id).types.includes('Flying'));
	const grounds = only(GROUND_MOVES), statuses = only(STATUS_MOVES);
	const hasOgerpon = pool.species.includes('ogerpon');

	const put = (s, id) => {
		if (!id || s.moves.includes(id)) return;
		s.moves[int(4)] = id;
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const tera = (s, t) => { s.teraType = t; };
	const randomTera = (s) => {
		const id = dex.species.get(s.species).id;
		const t = int(100) < 55 ? usageTera(id, rnd) : undefined;
		tera(s, t || pick(TYPES));
	};
	const typeOfMove = (id) => move(id).type;

	const tune = (s) => {
		randomTera(s);
		const role = int(100);
		if (kind === 'struggle') return;
		if (role < 14 && hasMove(pool, 'roost')) {
			// Roost: a Flying type or not, with a Tera type of Flying two times in five
			const k = int(100);
			if (k < 55 && flying.length) s.species = dex.species.get(pick(flying)).name;
			else if (nonFlying.length) s.species = dex.species.get(pick(nonFlying)).name;
			put(s, 'roost');
			tera(s, int(100) < 40 ? 'Flying' : pick(TYPES));
			if (grounds.length && int(100) < 35) put(s, pick(grounds));
		} else if (role < 30 && lowPool.length) {
			const group = pick(lowPool);
			const id = pick(group);
			put(s, id);
			if (int(100) < 40) put(s, pick(pick(lowPool)));
			if (int(100) < 75) tera(s, typeOfMove(id));
		} else if (role < 40 && nonFlying.length) {
			// a Tera Flying non-Flying Pokemon
			s.species = dex.species.get(pick(nonFlying)).name;
			tera(s, 'Flying');
		} else if (role < 50) {
			// a Tera Poison or Steel Pokemon (not that type already, mostly)
			const t = pick(['Poison', 'Steel']);
			const other = pool.species.filter(id => !dex.species.get(id).types.includes(t));
			if (other.length && int(100) < 70) s.species = dex.species.get(pick(other)).name;
			tera(s, t);
			if (statuses.length && int(100) < 30) put(s, pick(statuses));
		} else if (role < 58) {
			// a Flying, Poison or Steel type that Terastallizes out of it
			const t = pick(['Flying', 'Poison', 'Steel']);
			const mine = byType(t);
			if (mine.length) s.species = dex.species.get(pick(mine)).name;
			tera(s, pick(TYPES.filter(x => x !== t)));
		} else if (role < 68 && hasOgerpon) {
			s.species = 'Ogerpon';
			tera(s, int(100) < 70 ? pick(OGERPON_OK) : pick(OGERPON_NO));
			s.level = int(100) < 85 ? 100 : s.level;
		}
		// a share carry a Ground move (against Tera Flying), a share a status move (against Tera Poison and Steel)
		if (grounds.length && int(100) < 14) put(s, pick(grounds));
		if (statuses.length && int(100) < 10) put(s, pick(statuses));
	};
	for (const s of scenario.teams[0]) tune(s);
	if (mirror) scenario.teams[1] = JSON.parse(JSON.stringify(scenario.teams[0]));
	else for (const s of scenario.teams[1]) tune(s);

	// scripted openings (`scenario.choices`: a pair per decision point, then the random policy). The leads are the first Pokemon
	// of the teams, with the scripted team order 123456; they are level 100 and not Shedinja, and the scripted turns use no
	// damage the next scripted pair could not follow
	const lead = (side) => scenario.teams[side][0];
	const steady = (s) => {
		s.level = 100;
		// (Assault Vest disables status moves: an idle lead would have to Struggle, which has no Tera choice)
		if (s.item === 'Assault Vest') s.item = '';
		if (s.species === 'Shedinja') s.species = dex.species.get(pick(pool.species.filter(id => id !== 'shedinja'))).name;
	};
	const idles = only(IDLE_MOVES);
	const idleSet = (s) => {
		steady(s);
		s.moves = [...new Set([...idles.slice().sort(() => rnd() - 0.5).slice(0, 4)])];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	if (kind === 'dual') {
		// both leads Terastallize on turn 1; in a mirror match the speeds tie, so do the Tera actions
		const twins = int(100) < 50;
		if (twins) scenario.teams[1] = JSON.parse(JSON.stringify(scenario.teams[0]));
		for (const s of [lead(0), lead(1)]) steady(s);
		if (twins) scenario.teams[1][0] = JSON.parse(JSON.stringify(lead(0)));
		const m = 1 + int(4);
		scenario.choices = [['team 123456', 'team 123456'], [`move ${m} terastallize`, `move ${twins ? m : 1 + int(4)} terastallize`]];
	} else if (kind === 'trickroom' && hasMove(pool, 'trickroom')) {
		// turn 1: the first lead sets Trick Room, the other idles; turn 2: both Terastallize, in the reverse speed order
		const a = lead(0), b = lead(1);
		idleSet(a); idleSet(b);
		a.moves[0] = 'trickroom';
		a.moves = [...new Set(a.moves)];
		while (a.moves.length < 4) { const m = pick(idles); if (!a.moves.includes(m)) a.moves.push(m); }
		scenario.choices = [
			['team 123456', 'team 123456'], ['move 1', `move ${2 + int(3)}`],
			[`move ${1 + int(4)} terastallize`, `move ${1 + int(4)} terastallize`],
		];
	} else if (kind === 'duel') {
		// one lead Terastallizes into a resistance or immunity to the other's move, which comes later in the same turn
		const pairs = RESISTED.filter(([m]) => hasMove(pool, m));
		if (pairs.length) {
			const [m, t] = pick(pairs);
			const side = int(2);
			const a = lead(side), b = lead(1 - side);
			steady(a); steady(b);
			a.moves = [...new Set([pick(idles), pick(idles), pick(idles), pick(idles)])];
			while (a.moves.length < 4) { const x = pick(pool.moves); if (!a.moves.includes(x)) a.moves.push(x); }
			tera(a, t);
			b.moves[0] = m;
			b.moves = [...new Set(b.moves)];
			if (int(100) < 50) {
				// the attacker is faster or slower than the Pokemon that Terastallizes: Tera acts first either way
				const bySpe = pool.species.slice().sort((x, y) => dex.species.get(x).baseStats.spe - dex.species.get(y).baseStats.spe);
				const third = Math.max(1, Math.floor(bySpe.length / 3));
				const fast = bySpe.slice(-third), slow = bySpe.slice(0, third);
				const aFaster = int(2) === 0;
				a.species = dex.species.get(pick(aFaster ? fast : slow)).name;
				b.species = dex.species.get(pick(aFaster ? slow : fast)).name;
			}
			const pairChoice = [null, null];
			pairChoice[side] = `move ${1 + int(a.moves.length)} terastallize`;
			pairChoice[1 - side] = 'move 1';
			scenario.choices = [['team 123456', 'team 123456'], pairChoice];
		}
	}
	// (a scenario whose own first pair needs the free policy keeps no script)
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally: observation hooks on Showdown's classes. Nothing is changed: each wrapper calls the original with every argument and
// returns its result.

let activeTally = null;
function newTally() {
	const z = (keys) => Object.fromEntries(keys.map(k => [k, 0]));
	return {
		// the action
		terastallizeCalled: 0, teraType: z(TYPES), teraIntoOriginalType: 0, teraIntoNewType: 0,
		teraTurnOne: 0, teraLaterTurn: 0, terastallizedThenStruggle: 0,
		bothTeraSameTurn: 0, teraActionsSorted: 0, teraActionsSpeedTie: 0, teraActionsUnderTrickRoom: 0,
		teraActionBeforeHigherPriorityMove: 0, teraOfFasterSide: 0, teraOfSlowerSide: 0,
		// STAB
		stab2xOriginalTeraType: 0, stabNewTeraType: 0, stabKeptOriginalTypeAfterTera: 0,
		// the 60 floor, by the move's outcome (the Tera type is the move's type)
		floorApplied: 0, floorSkippedPriority: 0, floorSkippedMultihit: 0, floorSkippedVariablePower: 0,
		floorNotNeededBasePowerAtLeast60: 0, teraAttackerMoveOfOtherType: 0, floorAppliedCallbackMove: 0,
		// defence
		teraTargetResistsNew: 0, teraTargetWeakNew: 0, teraTargetLostResist: 0, teraTargetLostWeakness: 0,
		teraImmunityGained: 0, teraImmunityLost: 0, teraFlyingHitByGround: 0, teraLostFlyingHitByGround: 0,
		hitOnTurnOfTera: 0, hitByMoveAfterTeraResisted: 0,
		// hazards and statuses
		teraSwitchInSpikesFlying: 0, teraSwitchInSpikesGrounded: 0, teraSwitchInStealthRock: 0,
		teraPoisonAbsorbsToxicSpikes: 0, teraSteelSkipsToxicSpikes: 0, teraFlyingSkipsToxicSpikes: 0, teraLostPoisonAbsorb: 0,
		teraPsnImmuneByPoisonOrSteel: 0, teraLostPsnImmunity: 0,
		teraFlyingInGrassyTerrain: 0, teraFlyingInMistyTerrain: 0, teraGroundedInGrassyTerrain: 0,
		// switching and fainting
		teraSwitchedOut: 0, teraSwitchedBackIn: 0, teraFainted: 0,
		// Roost
		roostTeraFlying: 0, roostTeraNonFlying: 0, roostVolatileFailedUnderTera: 0, roostNotTera: 0,
		// Ogerpon
		ogerponTealTera: 0, ogerponNoOp: 0, ogerponNoOpRepeated: 0, ogerponTeraOverParadoxOrUnburden: 0, ogerponTeraOverOtherAbility: 0,
		embodyBoostAtTera: 0, embodyBoostOnSwitchIn: 0, ogerponTealTeraFainted: 0, ogerponFaintedAbilityRestored: 0,
		ogerponSwitchedBackInAsTealTera: 0,
		// structurally zero
		// - Tera offered to a Pokemon that must Struggle: the request omits canTerastallize (pokemon.ts 1133-1158), so the policy
		//   is never offered it, and the engine offers none
		// - Stellar: a Stellar Tera type is rejected at parseTeam until 5b, and the generator never draws it
		// - Morpeko: the Tera branch needs Morpeko-Hangry, which only Hunger Switch makes, and it is unsupported
		// - a Pokemon locked into a move: no locking move or volatile is supported yet (no Choice item, no Outrage-style move)
		// - Illusion, Terapagos-Terastal and a transformed Ogerpon: no such ability, forme or move is supported
		// - an Ogerpon whose ability is cantsuppress: no supported ability carries the flag
		teraOfferedWhileStruggling: 0, stellarTera: 0, morpekoTeraBranch: 0, teraWhileLockedIntoMove: 0, illusionOrTerapagosTera: 0, ogerponWithCantSuppressAbility: 0,
	};
}

function install() {
	if (Battle.prototype.__slice5Instrumented) return;
	Battle.prototype.__slice5Instrumented = true;
	const t = () => activeTally;
	const teraTurn = new WeakMap(); // Pokemon -> the turn it terastallized
	const noops = new WeakMap(); // Pokemon -> times an Ogerpon's Tera did nothing
	const state = { inSwitch: false, inTera: false };

	const terastallize = BattleActions.prototype.terastallize;
	BattleActions.prototype.terastallize = function (pokemon) {
		const c = t();
		let before;
		if (c) {
			before = { ability: pokemon.ability, species: pokemon.species.id };
			c.terastallizeCalled++;
			c.teraType[pokemon.teraType]++;
			if (this.battle.turn === 1) c.teraTurnOne++; else c.teraLaterTurn++;
			const foe = pokemon.side.foe.active[0];
			if (foe && teraTurn.get(foe) === this.battle.turn) c.bothTeraSameTurn++;
			if (foe) { if (pokemon.speed > foe.speed) c.teraOfFasterSide++; else if (pokemon.speed < foe.speed) c.teraOfSlowerSide++; }
			if (pokemon.types.includes(pokemon.teraType)) c.teraIntoOriginalType++; else c.teraIntoNewType++;
		}
		state.inTera = true;
		const r = terastallize.call(this, pokemon);
		state.inTera = false;
		if (c) {
			if (pokemon.terastallized) {
				teraTurn.set(pokemon, this.battle.turn);
				if (pokemon.species.id === 'ogerpontealtera') {
					c.ogerponTealTera++;
					if (['protosynthesis', 'quarkdrive', 'unburden'].includes(before.ability)) c.ogerponTeraOverParadoxOrUnburden++;
					else c.ogerponTeraOverOtherAbility++;
				}
			} else if (pokemon.species.baseSpecies === 'Ogerpon') {
				c.ogerponNoOp++;
				if (noops.get(pokemon)) c.ogerponNoOpRepeated++;
				noops.set(pokemon, (noops.get(pokemon) || 0) + 1);
			}
		}
		return r;
	};

	// Struggle after Terastallization (the Struggle request carries no Tera choice: `lockedMove = 'struggle'`)
	const useMove = BattleActions.prototype.useMove;
	BattleActions.prototype.useMove = function (move, pokemon, ...rest) {
		const c = t();
		const id = typeof move === 'string' ? dex.toID(move) : move.id;
		if (c && id === 'struggle' && pokemon.terastallized) c.terastallizedThenStruggle++;
		return useMove.call(this, move, pokemon, ...rest);
	};

	// the Tera actions in the queue sort: speed ties, Trick Room, and Tera before a faster or higher-priority move
	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, ...rest) {
		const c = t();
		if (c && list.length >= 2 && list[0] && list[0].choice) {
			const teras = list.filter(a => a.choice === 'terastallize');
			if (teras.length === 2) {
				c.teraActionsSorted++;
				if (teras[0].speed === teras[1].speed) c.teraActionsSpeedTie++;
				if ('trickroom' in this.field.pseudoWeather) c.teraActionsUnderTrickRoom++;
			}
			if (teras.length >= 1 && list.some(a => a.choice === 'move' && a.priority > 0)) c.teraActionBeforeHigherPriorityMove++;
		}
		return speedSort.call(this, list, ...rest);
	};

	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, relayVar, ...rest) {
		const r = runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
		const c = t();
		if (c && eventid === 'ModifySTAB' && target && target.terastallized && sourceEffect) {
			// target is the attacker, sourceEffect the move, relayVar the STAB before the event
			if (relayVar === 2) c.stab2xOriginalTeraType++;
			else if (relayVar === 1.5 && target.terastallized === sourceEffect.type) c.stabNewTeraType++;
			else if (relayVar === 1.5) c.stabKeptOriginalTypeAfterTera++;
		}
		if (c && eventid === 'BasePower' && target && target.terastallized && sourceEffect && sourceEffect.type) {
			const dexMove = dex.moves.get(sourceEffect.id);
			const bp = r === undefined ? relayVar : r;
			if (target.terastallized !== sourceEffect.type) {
				if (bp < 60) c.teraAttackerMoveOfOtherType++;
			} else if (bp >= 60) c.floorNotNeededBasePowerAtLeast60++;
			else if (dexMove.priority > 0) c.floorSkippedPriority++;
			else if (dexMove.multihit) c.floorSkippedMultihit++;
			else if ((sourceEffect.basePower === 0 || sourceEffect.basePower === 150) && sourceEffect.basePowerCallback) c.floorSkippedVariablePower++;
			else {
				c.floorApplied++;
				if (sourceEffect.basePowerCallback) c.floorAppliedCallbackMove++;
			}
		}
		return r;
	};

	// a hit on a terastallized target: what its Tera type changed
	const getDamage = BattleActions.prototype.getDamage;
	BattleActions.prototype.getDamage = function (source, target, move, ...rest) {
		const c = t();
		if (c && target && target.terastallized && move && move.type && move.type !== '???' && move.category !== 'Status') {
			const orig = dex.getEffectiveness(move.type, target.types);
			const now = dex.getEffectiveness(move.type, [target.terastallized]);
			if (now < 0 && orig >= 0) c.teraTargetResistsNew++;
			if (now > 0 && orig <= 0) c.teraTargetWeakNew++;
			if (now >= 0 && orig < 0) c.teraTargetLostResist++;
			if (now <= 0 && orig > 0) c.teraTargetLostWeakness++;
			if (teraTurn.get(target) === this.battle.turn) {
				c.hitOnTurnOfTera++;
				if (now < 0) c.hitByMoveAfterTeraResisted++;
			}
		}
		return getDamage.call(this, source, target, move, ...rest);
	};

	const runImmunity = Pokemon.prototype.runImmunity;
	Pokemon.prototype.runImmunity = function (source, message) {
		const r = runImmunity.call(this, source, message);
		const c = t();
		const type = typeof source === 'string' ? source : source.type; // (a move or a type)
		if (c && this.terastallized && type && type !== '???') {
			const wasImmune = !dex.getImmunity(type, this.types) || (type === 'Ground' && this.types.includes('Flying'));
			if (!r && !wasImmune) c.teraImmunityGained++;
			if (r && wasImmune) c.teraImmunityLost++;
			if (type === 'Ground' && this.terastallized === 'Flying' && !r) c.teraFlyingHitByGround++;
			if (type === 'Ground' && this.types.includes('Flying') && this.terastallized !== 'Flying' && r) c.teraLostFlyingHitByGround++;
		}
		return r;
	};

	const runStatusImmunity = Pokemon.prototype.runStatusImmunity;
	Pokemon.prototype.runStatusImmunity = function (type, message) {
		const r = runStatusImmunity.call(this, type, message);
		const c = t();
		if (c && this.terastallized && type === 'psn') {
			const was = this.types.includes('Poison') || this.types.includes('Steel');
			const now = this.terastallized === 'Poison' || this.terastallized === 'Steel';
			if (!r && now && !was) c.teraPsnImmuneByPoisonOrSteel++;
			if (r && was && !now) c.teraLostPsnImmunity++;
		}
		return r;
	};

	// hazards on arrival
	const runSwitch = BattleActions.prototype.runSwitch;
	BattleActions.prototype.runSwitch = function (pokemon) {
		const c = t();
		const conds = pokemon.side.sideConditions;
		const had = { spikes: 'spikes' in conds, toxicspikes: 'toxicspikes' in conds, stealthrock: 'stealthrock' in conds };
		const wasTera = !!pokemon.terastallized;
		const types = pokemon.types.slice();
		state.inSwitch = true;
		const r = runSwitch.call(this, pokemon);
		state.inSwitch = false;
		if (c && wasTera) {
			c.teraSwitchedBackIn += 0; // counted at switchIn
			if (had.stealthrock) c.teraSwitchInStealthRock++;
			if (had.spikes) { if (pokemon.terastallized === 'Flying') c.teraSwitchInSpikesFlying++; else c.teraSwitchInSpikesGrounded++; }
			if (had.toxicspikes) {
				if (pokemon.terastallized === 'Poison' && !('toxicspikes' in pokemon.side.sideConditions)) c.teraPoisonAbsorbsToxicSpikes++;
				if (pokemon.terastallized === 'Steel') c.teraSteelSkipsToxicSpikes++;
				if (pokemon.terastallized === 'Flying') c.teraFlyingSkipsToxicSpikes++;
				if (types.includes('Poison') && pokemon.terastallized !== 'Poison' && pokemon.terastallized !== 'Flying' &&
					pokemon.terastallized !== 'Steel' && pokemon.status) c.teraLostPoisonAbsorb++;
			}
		}
		return r;
	};

	const switchIn = BattleActions.prototype.switchIn;
	BattleActions.prototype.switchIn = function (pokemon, pos, ...rest) {
		const c = t();
		if (c) {
			const old = pokemon.side.active[pos];
			if (old && old !== pokemon && old.terastallized && old.hp) c.teraSwitchedOut++;
			if (pokemon.terastallized) {
				c.teraSwitchedBackIn++;
				if (pokemon.species.id === 'ogerpontealtera') c.ogerponSwitchedBackInAsTealTera++;
			}
		}
		return switchIn.call(this, pokemon, pos, ...rest);
	};

	// Roost under Terastallization
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, ...rest) {
		const r = addVolatile.call(this, status, ...rest);
		const c = t();
		const id = typeof status === 'string' ? status : status.id;
		if (c && id === 'roost') {
			if (this.terastallized) {
				if (this.terastallized === 'Flying') c.roostTeraFlying++; else c.roostTeraNonFlying++;
				if (!r || !this.volatiles.roost) c.roostVolatileFailedUnderTera++;
			} else c.roostNotTera++;
		}
		return r;
	};

	// fields: a Tera Flying Pokemon in a terrain
	const fieldEvent = Battle.prototype.fieldEvent;
	Battle.prototype.fieldEvent = function (eventid, ...rest) {
		const c = t();
		if (c && eventid === 'Residual') {
			for (const side of this.sides) {
				const p = side.active[0];
				if (!p || !p.terastallized || !p.hp) continue;
				if (this.field.terrain === 'grassyterrain') {
					if (p.terastallized === 'Flying') c.teraFlyingInGrassyTerrain++; else c.teraGroundedInGrassyTerrain++;
				}
				if (this.field.terrain === 'mistyterrain' && p.terastallized === 'Flying') c.teraFlyingInMistyTerrain++;
			}
		}
		return fieldEvent.call(this, eventid, ...rest);
	};

	// fainting: a terastallized Pokemon, an Ogerpon in its Tera forme, and what the faint restores
	const resolveFaints = Battle.prototype.resolveFaints;
	Battle.prototype.resolveFaints = function (...args) {
		const c = t();
		const watched = [];
		if (c) {
			for (const entry of this.faintQueue) {
				const p = entry.target;
				if (p.terastallized) c.teraFainted++;
				if (p.species.id === 'ogerpontealtera') { c.ogerponTealTeraFainted++; watched.push(p); }
			}
		}
		const r = resolveFaints.call(this, ...args);
		for (const p of watched) {
			if (p.fainted && p.species.id === 'ogerpon' && !p.terastallized && p.ability === dex.toID(p.set.ability)) c.ogerponFaintedAbilityRestored++;
		}
		return r;
	};

	// Embody Aspect (Teal)'s boost: at the forme change, or on a later switch-in
	const boost = Battle.prototype.boost;
	Battle.prototype.boost = function (boosts, target, source, effect, ...rest) {
		const c = t();
		const eff = effect || this.effect;
		if (c && eff && eff.id === 'embodyaspectteal') {
			if (state.inTera) c.embodyBoostAtTera++;
			else if (state.inSwitch) c.embodyBoostOnSwitchIn++;
		}
		return boost.call(this, boosts, target, source, effect, ...rest);
	};
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice5.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice5-${k}.json`), JSON.stringify(pool[k]));
	}
	const tally = newTally();
	activeTally = tally;
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let trace;
		try { trace = record(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice5-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice5 seeds, ${written} traces written, ${truncated} truncated at the turn limit, ${pool.moves.length} moves, ` +
		`${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error('tally (from Showdown\'s own events):');
	for (const [k, v] of Object.entries(tally)) console.error(`  ${k}: ${JSON.stringify(v)}`);
}

module.exports = { assign };
