#!/usr/bin/env node
'use strict';
// Generates battles for "slice 8f" (suppression and reflection: Mold Breaker, Neutralizing Gas, Magic Bounce) and records each on Showdown.
//
// Mold Breaker (`onModifyMove`: `move.ignoreAbility = true`) makes `battle.suppressingAbility(target)` true for the breakable abilities of the
// Pokemon it hits, but not for its own (`activePokemon !== target`). Neutralizing Gas is `Pokemon#ignoringAbility`: while a gas holder is in
// play every other ability but the `cantsuppress` ones is ignored; when the last gas leaves (a switch-out or a faint) its `onEnd` marks itself
// `ending`, speed-sorts the Pokemon in play (a tie draws) and restarts their abilities. Magic Bounce (`onTryHit`, `onAllyTryHitSide`) sends a
// `reflectable` move back with a nested `useMove` (`hasBounced`: once only).
//
// The teams are slice 5's (as gen-slice7c.js), with the three abilities and the reflectable status moves sprinkled on the sets. About 30% of
// the battles are plain random play; the rest are scripted openings (a pair of choices per decision point; the random policy takes over when
// the script ends, and a script Showdown rejects is dropped and the battle recorded without it):
//   breaker   Mold Breaker (or a control with no ability) attacks Filter, Solid Rock and Prism Armor (the last is not breakable) with a super
//             effective move
//   leaf      Leaf Guard under sun against status moves from a Mold Breaker or an ordinary user
//   coat      Overcoat against powder moves from a Mold Breaker or an ordinary user
//   self      Mold Breaker users with recoil moves and Life Orb against Rocky Helmet (the user's own damage), and Mold Breaker against Mold Breaker
//   gasweather  Neutralizing Gas against weather and terrain setters (on the lead, entering under the gas, entering while the gas is up, the
//             gas leaving by a switch or a faint, a speed tie in the sort)
//   gasparadox  Neutralizing Gas against Protosynthesis, Quark Drive (with Booster Energy) and Unburden
//   gasswitch Regenerator and Natural Cure switching out under the gas, and gas holders on both sides
//   bounce    a reflectable status move into Magic Bounce: hazards, Taunt, Encore, Roar and Whirlwind, status, stat drops, Defog, Parting Shot
//   bouncesub the bounced move meeting a Substitute (on either side) and Mold Breaker attackers
//   bouncetwo double Magic Bounce, and a side with Magic Bounce on the bench
//
// Usage: node gen-slice8f.js <outDir> <count> <poolFile> [firstSeed=1]
// Output: <outDir>/slice8f-<seed>.json and slice8f-{moves,species,abilities,items}.json (copies of the pool). A tally of each branch is
// printed to stderr; it is computed by wrapping Showdown's own methods (observation only).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { BattleActions } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'battle-actions'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');
const { assign: assign5 } = require('./gen-slice5.js');

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

const PIVOTS = ['uturn', 'flipturn', 'voltswitch', 'teleport', 'partingshot', 'chillyreception'];
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];
const NEW_ABILITIES = ['moldbreaker', 'neutralizinggas', 'magicbounce'];
const WEATHER_SETTERS = ['drought', 'drizzle', 'sandstream', 'snowwarning'];
const TERRAIN_SETTERS = ['electricsurge', 'grassysurge', 'mistysurge', 'psychicsurge'];

const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

let lastKind = 'random';

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2891336453 + 1664525);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	const hasAb = (id) => pool.abilities.includes(id);
	for (const id of NEW_ABILITIES) if (!hasAb(id)) throw new Error(`the pool has no ${id}`);
	const short = (sc) => sc.teams[0].length !== 6 || sc.teams[1].length !== 6;
	let scenario = assign5(scenario0, pool, seed);
	for (let k = 1; short(scenario) && k < 30; k++) {
		const s2 = seed + 1000003 * k;
		scenario = assign5(makeScenario(s2, 'gen9ou', pool), pool, s2);
	}
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const mv = (id) => dex.moves.get(id);
	const speciesName = (id) => dex.species.get(id).name;
	const real = pool.species.filter(id => id !== 'shedinja');
	const isReflect = (id) => mv(id).flags.reflectable && mv(id).category === 'Status';
	const REFLECT = pool.moves.filter(isReflect);
	const HAZARDS = REFLECT.filter(id => mv(id).sideCondition);
	const VOLATILES = ['taunt', 'encore'].filter(has);
	const STATUS_R = REFLECT.filter(id => mv(id).status);
	const DROPS = REFLECT.filter(id => mv(id).boosts);
	const inert = (id) => !PIVOTS.includes(id) && !FORCERS.includes(id) && id !== 'encore' && id !== 'taunt' && !REFLECT.includes(id);
	const statuses = pool.moves.filter(id => mv(id).category === 'Status' && inert(id));
	const quiet = statuses.filter(id => ['self', 'allySide'].includes(mv(id).target) && !mv(id).weather && !mv(id).sideCondition && !mv(id).terrain);
	const attacks = pool.moves.filter(id => {
		const m = mv(id);
		return m.category !== 'Status' && m.basePower >= 40 && (m.accuracy === true || m.accuracy >= 90) && m.priority === 0 && inert(id) &&
			!m.flags.charge && !m.flags.recharge && !m.selfdestruct && !m.mindBlownRecoil && !m.multihit && !m.selfSwitch && !m.forceSwitch;
	});
	const inertPool = pool.moves.filter(inert);
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves[int(4)] = id;
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const set = (s, ...ids) => {
		s.moves = [...new Set(ids)];
		while (s.moves.length < 4) { const m = pick(inertPool); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const steady = (s, level, spe) => {
		s.level = level; s.item = ''; s.ability = 'No Ability'; s.nature = 'Hardy';
		s.evs = { hp: 0, atk: 0, def: 0, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
	};
	const ab = (s, id) => { s.ability = abilityName(id); };
	const item = (s, id) => { s.item = itemName(id); };

	// sprinkle (every set, from this generator's stream): the three abilities, and the reflectable status moves
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		const r = int(100);
		if (r < 14) ab(s, 'moldbreaker'); else if (r < 28) ab(s, 'neutralizinggas'); else if (r < 46) ab(s, 'magicbounce');
		if (int(100) < 40 && REFLECT.length) put(s, pick(REFLECT));
		if (int(100) < 15 && HAZARDS.length) put(s, pick(HAZARDS));
		if (int(100) < 10 && VOLATILES.length) put(s, pick(VOLATILES));
	}
	const kinds = [
		['random', 30], ['breaker', 7], ['leaf', 4], ['coat', 4], ['self', 3], ['gasweather', 11], ['gasparadox', 6], ['gasswitch', 7],
		['bounce', 13], ['bouncesub', 7], ['bouncetwo', 8],
	];
	let kind = 'random', acc = 0;
	const roll = int(100);
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	lastKind = kind;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const duo = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const teams = (an, bn) => { const pair = [null, null]; pair[aIdx] = `team ${[1, 2, 3, 4, 5, 6].slice(0, an).join('')}`; pair[bIdx] = `team ${[1, 2, 3, 4, 5, 6].slice(0, bn).join('')}`; return pair; };
	const full = () => teams(6, 6);
	// A against B, A faster or slower (species chosen for the speed gap)
	const pair = (aFaster, levelA, levelB) => {
		for (let k = 0; k < 400; k++) {
			const a = pick(real);
			const sa = speStat(a, levelA, aFaster ? 252 : 0);
			const bs = real.filter(b => (aFaster ? sa > speStat(b, levelB, 0) + 10 : speStat(b, levelB, 0) > sa + 10));
			if (bs.length) { A.species = speciesName(a); B.species = speciesName(pick(bs)); return; }
		}
		throw new Error('no speed pair');
	};
	const duel = (aFaster, levelA, levelB) => { steady(A, levelA || 100, aFaster ? 252 : 0); steady(B, levelB || 100, 0); pair(aFaster, levelA || 100, levelB || 100); };
	const idle = () => pick(quiet.length ? quiet : statuses);
	const teraMaybe = (m) => (int(100) < 25 ? `${m} terastallize` : m);
	const aFast = int(2) === 0;
	const types = (id) => dex.species.get(id).types;
	const speciesIdOf = (s) => dex.species.get(s.species).id;
	// a move of an attacker that is super effective against the species (or any attack)
	const effective = (set) => {
		const ts = types(speciesIdOf(set));
		const se = attacks.filter(id => dex.getEffectiveness(mv(id).type, ts) > 0 && mv(id).basePower >= 60 && dex.getImmunity(mv(id).type, ts));
		return se.length ? pick(se) : pick(attacks);
	};
	const mild = attacks.filter(id => mv(id).basePower <= 80);
	const strike = () => pick(mild);

	if (kind === 'breaker') {
		duel(aFast, 70, 100);
		const variant = pick(['filter', 'solidrock', 'prismarmor'].filter(hasAb));
		ab(A, int(100) < 75 ? 'moldbreaker' : 'noability'); ab(B, variant);
		const atk = effective(B);
		set(A, atk, effective(B), idle(), strike());
		set(B, strike(), idle(), strike(), idle());
		scenario.choices = [full(), duo('move 1', 'move 2'), duo('move 2', 'move 1'), duo('move 1', 'move 2'), duo(teraMaybe('move 1'), 'move 1'), duo('move 1', 'switch 2'), duo('move 1', 'move 1')];
	} else if (kind === 'leaf') {
		duel(aFast, 100, 100);
		ab(A, int(100) < 65 ? 'moldbreaker' : 'noability'); ab(B, 'leafguard');
		const sts = STATUS_R.filter(id => ['brn', 'par', 'psn', 'tox'].includes(mv(id).status));
		set(A, pick(sts), pick(sts), strike(), idle());
		set(B, has('sunnyday') ? 'sunnyday' : idle(), idle(), strike(), idle());
		scenario.choices = [full(), duo('move 4', 'move 1'), duo('move 1', 'move 2'), duo('move 2', 'move 2'), duo('move 1', 'move 3'), duo('move 2', 'move 4')];
	} else if (kind === 'coat') {
		duel(aFast, 100, 100);
		ab(A, int(100) < 65 ? 'moldbreaker' : 'noability'); ab(B, 'overcoat');
		const powders = pool.moves.filter(id => mv(id).flags.powder);
		set(A, pick(powders), pick(powders), strike(), idle());
		set(B, strike(), idle(), strike(), idle());
		scenario.choices = [full(), duo('move 1', 'move 2'), duo('move 2', 'move 2'), duo('move 1', 'move 2'), duo('move 3', 'move 1')];
	} else if (kind === 'self') {
		duel(aFast, 100, 100);
		const recoil = attacks.filter(id => mv(id).recoil);
		const mirror = int(100) < 40;
		ab(A, 'moldbreaker'); ab(B, mirror ? 'moldbreaker' : 'noability');
		item(A, 'lifeorb'); item(B, int(2) === 0 ? 'rockyhelmet' : 'lifeorb');
		set(A, pick(recoil), pick(recoil), strike(), idle());
		set(B, pick(recoil), strike(), idle(), idle());
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 1', 'move 1')];
	} else if (kind === 'gasweather') {
		const variant = int(5);
		const setter = pick(WEATHER_SETTERS.concat(TERRAIN_SETTERS).filter(hasAb));
		duel(aFast, 100, 100);
		const idle1 = idle(), idle2 = idle();
		if (variant === 0) { // the gas on the lead, a setter on the other lead; the gas leaves by a switch
			ab(A, 'neutralizinggas'); ab(B, setter);
			set(A, idle1, idle2, strike(), idle()); set(B, idle1, idle2, strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('switch 2', 'move 2'), duo('move 1', 'move 1'), duo('move 1', 'switch 2')];
		} else if (variant === 1) { // a setter in play, the gas enters from the bench (another setter on the gas side may differ), then leaves
			ab(A, setter); const other = pick(WEATHER_SETTERS.filter(hasAb));
			ab(B, int(100) < 60 ? other : 'noability'); ab(scenario.teams[bIdx][1], 'neutralizinggas'); ab(scenario.teams[bIdx][2], int(2) === 0 ? 'neutralizinggas' : 'noability');
			set(A, idle1, idle2, strike(), idle()); set(B, idle1, idle2, strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'switch 2'), duo('move 1', 'move 1'), duo('move 2', 'switch 3'), duo('move 1', 'move 1')];
		} else if (variant === 2) { // the setter comes in under the gas (no weather), then the gas leaves (the setter starts)
			ab(A, 'neutralizinggas'); ab(B, 'noability'); ab(scenario.teams[bIdx][1], setter);
			set(A, idle1, idle2, strike(), idle()); set(B, idle1, idle2, strike(), idle());
			scenario.choices = [full(), duo('move 1', 'switch 2'), duo('move 1', 'move 1'), duo('switch 2', 'move 1'), duo('move 1', 'move 2')];
		} else if (variant === 3) { // the gas holder faints
			steady(A, 8, 0); steady(B, 100, 0); pair(false, 8, 100);
			ab(A, 'neutralizinggas'); ab(B, setter);
			set(A, idle1, idle2, strike(), idle()); set(B, idle1, strike(), strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 2'), duo('switch 2', null), duo('move 1', 'move 1')];
		} else { // a speed tie: the sort of the gas's onEnd shuffles
			B.species = A.species; steady(A, 100, 0); steady(B, 100, 0);
			ab(A, 'neutralizinggas'); ab(B, setter);
			set(A, idle1, idle2, strike(), idle()); set(B, idle1, idle2, strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('switch 2', 'move 1'), duo('move 1', 'move 1')];
		}
	} else if (kind === 'gasparadox') {
		duel(aFast, 100, 100);
		const which = pick(['protosynthesis', 'quarkdrive', 'unburden'].filter(hasAb));
		ab(A, 'neutralizinggas'); ab(B, which);
		const field = which === 'protosynthesis' ? 'sunnyday' : which === 'quarkdrive' ? 'electricterrain' : null;
		if (which === 'unburden') {
			item(B, pick(['electricseed', 'grassyseed', 'mistyseed', 'psychicseed', 'ejectpack', 'redcard', 'ejectbutton'].filter(id => pool.items.includes(id))));
			set(A, has('knockoff') ? 'knockoff' : strike(), pick(TERRAIN_SETTERS.length ? ['electricterrain', 'grassyterrain', 'mistyterrain', 'psychicterrain'].filter(has) : [idle()]), strike(), idle());
			set(B, idle(), strike(), idle(), strike());
		} else {
			if (pool.items.includes('boosterenergy') && int(100) < 50) item(B, 'boosterenergy');
			set(B, has(field) ? field : idle(), strike(), idle(), strike());
			set(A, strike(), idle(), strike(), idle());
		}
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 2'), duo('switch 2', 'move 2'), duo('move 1', 'move 2'), duo('move 1', 'move 2')];
	} else if (kind === 'gasswitch') {
		duel(aFast, 100, 100);
		const holder = pick(['regenerator', 'naturalcure'].filter(hasAb));
		const control = int(100) < 25;
		ab(A, holder); ab(B, control ? 'noability' : 'neutralizinggas');
		const sts = STATUS_R.filter(id => ['brn', 'par', 'psn', 'tox'].includes(mv(id).status));
		set(A, idle(), strike(), idle(), strike());
		set(B, holder === 'naturalcure' ? pick(sts) : strike(), strike(), strike(), idle());
		if (int(100) < 30) { // both sides hold a gas
			ab(scenario.teams[aIdx][1], 'neutralizinggas'); ab(B, 'neutralizinggas');
		}
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('switch 2', 'move 2'), duo('move 1', 'move 2'), duo('switch 2', 'move 3')];
	} else if (kind === 'bounce') {
		duel(aFast, 100, 100);
		const pool_ = pick([HAZARDS, VOLATILES, ['roar', 'whirlwind'].filter(has), STATUS_R, DROPS, ['defog', 'partingshot', 'strengthsap'].filter(has), REFLECT]);
		const x = pick(pool_.length ? pool_ : REFLECT), y = pick(REFLECT);
		ab(A, int(100) < 20 ? 'moldbreaker' : 'noability'); ab(B, 'magicbounce');
		set(A, x, y, strike(), idle());
		set(B, idle(), strike(), idle(), strike());
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 4'), duo('move 3', 'move 2'), duo('switch 2', 'move 1'), duo('move 1', 'move 1')];
	} else if (kind === 'bouncesub') {
		duel(aFast, 100, 100);
		const x = pick(REFLECT);
		const subSide = int(3); // 0: the bouncer has a Substitute, 1: the user has one, 2: a Mold Breaker user into the bouncer's Substitute
		ab(A, subSide === 2 ? 'moldbreaker' : 'noability'); ab(B, 'magicbounce');
		set(A, x, 'substitute', strike(), idle());
		set(B, 'substitute', strike(), idle(), idle());
		if (subSide === 0) scenario.choices = [full(), duo('move 3', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 3'), duo('move 3', 'move 2')];
		else if (subSide === 1) scenario.choices = [full(), duo('move 2', 'move 3'), duo('move 1', 'move 3'), duo('move 1', 'move 2'), duo('move 1', 'move 3')];
		else scenario.choices = [full(), duo('move 2', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 3'), duo('move 3', 'move 2')];
	} else if (kind === 'bouncetwo') {
		duel(aFast, 100, 100);
		const x = pick(REFLECT), y = pick(HAZARDS.length ? HAZARDS : REFLECT);
		const variant = int(2);
		if (variant === 0) { // both hold Magic Bounce: the bounced move is not bounced again
			ab(A, 'magicbounce'); ab(B, 'magicbounce');
			set(A, x, y, strike(), idle()); set(B, pick(REFLECT), y, strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 2'), duo('move 2', 'move 1')];
		} else { // Magic Bounce on the bench: the hazard lands on the lead, then the bouncer comes in; the reply is bounced
			ab(A, 'noability'); ab(B, 'noability'); ab(scenario.teams[bIdx][1], 'magicbounce');
			set(A, x, y, strike(), idle()); set(B, idle(), strike(), idle(), strike());
			scenario.choices = [full(), duo('move 2', 'move 1'), duo('move 1', 'switch 2'), duo('move 2', 'move 1'), duo('move 1', 'move 2'), duo('move 3', 'move 1')];
		}
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally, from Showdown's own methods (observation only)

let tally = null;
function newTally() {
	return {
		moldBreaker: { suppressedBreakable: 0, filter: 0, solidrock: 0, leafguard: 0, overcoat: 0, magicbounce: 0, selfNotSuppressed: 0, withStatusMove: 0 },
		gas: { entered: 0, leftByEnd: 0, endRestart: 0, endOtherGas: 0, endTie: 0, endAtFaint: 0, setterSuppressed: 0, setterRestarted: 0, paradoxRestarted: 0,
			ignoredOnSwitchOut: 0, ignoredBoostItem: 0, twoGases: 0 },
		bounce: { bounced: 0, hazard: 0, volatile: 0, forcer: 0, status: 0, drop: 0, otherMove: 0, intoSubstitute: 0, userSubstitute: 0, alreadyBounced: 0, bouncerSuppressed: 0,
			bouncedLanded: 0, pivot: 0 },
	};
}
// the user of a Mold Breaker move is never its own breakable-ability target here (its ability is Mold Breaker, which is not breakable)
const STRUCTURAL_ZERO = ['moldBreaker.selfNotSuppressed'];

function install() {
	const t = () => tally;
	// suppressingAbility: Mold Breaker's move in use against a Pokemon's ability
	const suppressingAbility = Battle.prototype.suppressingAbility;
	Battle.prototype.suppressingAbility = function (target) {
		const r = suppressingAbility.call(this, target);
		const c = t();
		if (c && this.activeMove && this.activeMove.ignoreAbility && target) {
			if (r) {
				c.moldBreaker.suppressedBreakable++;
				const id = target.ability;
				if (c.moldBreaker[id] !== undefined) c.moldBreaker[id]++;
				if (id === 'magicbounce') c.bounce.bouncerSuppressed++;
				if (this.activeMove.category === 'Status') c.moldBreaker.withStatusMove++;
			} else if (target === this.activePokemon) c.moldBreaker.selfNotSuppressed++;
		}
		return r;
	};
	const singleEvent = Battle.prototype.singleEvent;
	let inGasEnd = null;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, ...rest) {
		const c = t();
		if (!c) return singleEvent.call(this, eventid, effect, state, target, ...rest);
		if (eventid === 'SwitchIn' && effect.id === 'neutralizinggas') {
			c.gas.entered++;
			if (this.getAllActive().filter(p => p.ability === 'neutralizinggas').length > 1) c.gas.twoGases++;
		}
		if (eventid === 'End' && effect.id === 'neutralizinggas') {
			const prev = inGasEnd;
			inGasEnd = { restarted: [], tie: false };
			const others = this.getAllActive().filter(p => p !== target && p.hasAbility('neutralizinggas'));
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			if (others.length) c.gas.endOtherGas++;
			else if (state && state.ending) {
				c.gas.endRestart++;
				if (inGasEnd.tie) c.gas.endTie++;
				if (target.hp === 0 || target.fainted) c.gas.endAtFaint++;
				for (const id of inGasEnd.restarted) {
					if (WEATHER_SETTERS.includes(id) || TERRAIN_SETTERS.includes(id)) c.gas.setterRestarted++;
					if (['protosynthesis', 'quarkdrive'].includes(id)) c.gas.paradoxRestarted++;
				}
			}
			c.gas.leftByEnd++;
			inGasEnd = prev;
			return r;
		}
		if (inGasEnd && eventid === 'Start' && effect.effectType === 'Ability') inGasEnd.restarted.push(effect.id);
		return singleEvent.call(this, eventid, effect, state, target, ...rest);
	};
	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, comparator) {
		if (inGasEnd && list.length >= 2 && list[0] && list[0].speed !== undefined && list.every(p => p.speed === list[0].speed)) inGasEnd.tie = true;
		return speedSort.call(this, list, comparator);
	};
	// abilities ignored under the gas: a setter that entered, a switch-out ability
	const ignoringAbility = Pokemon.prototype.ignoringAbility;
	Pokemon.prototype.ignoringAbility = function () {
		const r = ignoringAbility.call(this);
		const c = t();
		if (c && r && this.isActive && this.ability !== 'neutralizinggas' && this.battle.getAllActive().some(p => p.ability === 'neutralizinggas')) {
			if (WEATHER_SETTERS.includes(this.ability) || TERRAIN_SETTERS.includes(this.ability)) c.gas.setterSuppressed++;
			if (['regenerator', 'naturalcure'].includes(this.ability) && this.battle.event && this.battle.event.id === 'SwitchOut') c.gas.ignoredOnSwitchOut++;
			if (this.item === 'boosterenergy') c.gas.ignoredBoostItem++;
		}
		return r;
	};
	// Magic Bounce: the nested use
	const useMove = BattleActions.prototype.useMove;
	BattleActions.prototype.useMove = function (move, pokemon, options) {
		const c = t();
		const src = options && options.sourceEffect;
		if (c && move && move.hasBounced) {
			const m = this.dex.moves.get(move.id || move);
			c.bounce.bounced++;
			if (m.sideCondition) c.bounce.hazard++;
			else if (['taunt', 'encore'].includes(m.id)) c.bounce.volatile++;
			else if (m.forceSwitch) c.bounce.forcer++;
			else if (m.status) c.bounce.status++;
			else if (m.boosts) c.bounce.drop++;
			else c.bounce.otherMove++;
			if (m.selfSwitch) c.bounce.pivot++;
			const tgt = options && options.target;
			if (pokemon.volatiles.substitute) c.bounce.intoSubstitute++;
			if (tgt && tgt.volatiles.substitute) c.bounce.userSubstitute++;
		}
		return useMove.call(this, move, pokemon, options);
	};
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, ...rest) {
		const c = t();
		if (c && (eventid === 'TryHit') && sourceEffect && sourceEffect.hasBounced && sourceEffect.flags && sourceEffect.flags.reflectable &&
			(Array.isArray(target) ? target[0] : target) && (Array.isArray(target) ? target[0] : target).ability === 'magicbounce' &&
			!(Array.isArray(target) ? target[0] : target).ignoringAbility()) c.bounce.alreadyBounced++;
		return runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
	};
	// a bounced move that landed on its user's side or on the user
	const addSideCondition = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, source, sourceEffect, linked) {
		const r = addSideCondition.call(this, status, source, sourceEffect, linked);
		const c = t();
		if (c && r && this.battle.activeMove && this.battle.activeMove.hasBounced) c.bounce.bouncedLanded++;
		return r;
	};
}

function recordScenario(scenario) {
	const before = JSON.stringify(tally);
	try {
		return { trace: record(scenario), scripted: !!scenario.choices };
	} catch (e) {
		if (!scenario.choices) throw e;
		tally = JSON.parse(before);
		const plain = Object.assign({}, scenario);
		delete plain.choices;
		return { trace: record(plain), scripted: false, dropped: e.message };
	}
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice8f.js <outDir> <count> <poolFile> [firstSeed=1]');
		process.exit(2);
	}
	const count = parseInt(countArg, 10), first = parseInt(firstArg || '1', 10);
	const pool = JSON.parse(fs.readFileSync(poolFile, 'utf8'));
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		if (!Array.isArray(pool[k])) throw new Error(`bad pool file ${poolFile}: no ${k}`);
	}
	fs.mkdirSync(outDir, { recursive: true });
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		fs.writeFileSync(path.join(outDir, `slice8f-${k}.json`), JSON.stringify(pool[k]));
	}
	tally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scripted = 0, dropped = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scripted++;
		if (result.dropped) { dropped++; if (process.env.SLICE8F_VERBOSE) console.error(`seed ${seed} (${lastKind}): script dropped: ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice8f-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice8f seeds, ${written} traces written (${scripted} scripted, ${dropped} scripts dropped), ${truncated} truncated, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${turns} turns, ${draws} draws`);
	const zeros = [];
	const walk = (prefix, v) => {
		if (typeof v === 'number') { if (v === 0 && !STRUCTURAL_ZERO.includes(prefix)) zeros.push(prefix); return; }
		for (const [k, x] of Object.entries(v)) walk(prefix ? `${prefix}.${k}` : k, x);
	};
	for (const [k, v] of Object.entries(tally)) { console.error(`  ${k}: ${JSON.stringify(v)}`); walk(k, v); }
	console.error(zeros.length ? `ZERO COUNTERS: ${zeros.join(', ')}` : 'no counter is zero');
}

module.exports = { assign };
