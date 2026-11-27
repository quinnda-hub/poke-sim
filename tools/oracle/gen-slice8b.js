#!/usr/bin/env node
'use strict';
// Generates battles for "slice 8b" (damage and stats abilities) and records each on Showdown.
//
// The abilities: Multiscale, Sharpness, Tinted Lens, Guts, Transistor, Sniper, Heatproof, Purifying Salt, Water Bubble, Flash Fire,
// Supreme Overlord, Vessel of Ruin and Unaware. The teams are slice 6's (gen-slice6.js `assign`), with these abilities sprinkled on the
// sets. About 30% of the battles are plain random play over those teams; the rest are scripted (a pair of choices per decision point; the
// random policy takes over when the script ends, and a script that Showdown rejects is dropped and the battle recorded without it):
//   multiscale  a Multiscale holder hit at full HP, below it, after it heals back, and by a multi-hit move (only the first hit is halved)
//   sharpness   a Sharpness holder with slicing and non-slicing moves
//   tinted      a Tinted Lens holder against a target that resists its move, and against one that does not
//   guts        a Guts holder burned (by Flame Orb or Will-O-Wisp), poisoned or paralysed, with Physical, Special and Facade moves, and before the status
//   transistor  a Transistor holder with Physical and Special Electric moves and a move of another type
//   sniper      a Sniper holder with moves that always crit (and ones that mostly do not)
//   heatproof   a Heatproof holder against Fire moves and burn damage (Will-O-Wisp, Flame Orb), at a level where the burn does 1 HP
//   salt        a Purifying Salt holder against Ghost moves, Will-O-Wisp, Toxic, Thunder Wave, Toxic Orb and Flame Orb, and Toxic Spikes on a switch-in
//   bubble      a Water Bubble holder against Fire moves, burns, Flame Orb, and with Water moves of its own
//   flashfire   a Flash Fire holder that absorbs Fire moves, uses its own boosted, absorbs again with the volatile in place, switches out and returns
//   overlord    a Supreme Overlord holder entering after 0 to 5 faints of its side (and again after one more, once benched)
//   vessel      Vessel of Ruin on one side or both, with Special and Physical moves from both sides (and mirror teams, whose holders tie in speed)
//   unaware     an Unaware holder against boosted attackers and boosted defenders (stages, evasion, accuracy, critical hits, drops), and two holders
//
// Usage:
//   node gen-slice8b.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice8b-<seed>.json, plus slice8b-{moves,species,abilities,items}.json (copies of the pool). A tally of the outcomes that
// distinguish each branch is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers pass
// every argument and result through and call nothing the battle would not call).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');
const { assign: assign6 } = require('./gen-slice6.js');

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

const ABILITIES = ['multiscale', 'sharpness', 'tintedlens', 'guts', 'transistor', 'sniper', 'heatproof', 'purifyingsalt', 'waterbubble',
	'flashfire', 'supremeoverlord', 'vesselofruin', 'unaware'];
const IDLE = ['recover', 'swordsdance', 'calmmind', 'roost'];
const STATUS_MOVES = ['willowisp', 'toxic', 'thunderwave'];
const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const speciesName = (id) => dex.species.get(id).name;
const typesOf = (id) => dex.species.get(id).types;

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2246822519 + 3266489917);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const hasMove = (id) => pool.moves.includes(id);
	for (const id of [...ABILITIES]) if (!pool.abilities.includes(id)) throw new Error(`the pool has no ${id}`);
	for (const id of [...IDLE, 'willowisp', 'toxic', 'thunderwave', 'toxicspikes']) if (!hasMove(id)) throw new Error(`the pool has no ${id}`);
	for (const id of ['flameorb', 'toxicorb']) if (!pool.items.includes(id)) throw new Error(`the pool has no ${id}`);
	let scenario = assign6(scenario0, pool, seed);
	const short = (sc) => sc.teams[0].length !== 6 || sc.teams[1].length !== 6;
	for (let k = 1; short(scenario) && k < 30; k++) {
		const s2 = seed + 1000003 * k;
		scenario = assign6(makeScenario(s2, 'gen9ou', pool), pool, s2);
	}
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;

	const mv = (id) => dex.moves.get(id);
	const real = pool.species.filter(id => id !== 'shedinja');
	// the sprinkle: the 8b abilities on many sets, with the orbs that burn and poison their holders, so that unscripted play meets them
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 45) s.ability = abilityName(pick(ABILITIES));
		if (int(100) < 7) s.item = itemName(pick(['flameorb', 'toxicorb']));
	}
	const kinds = [
		['random', 30], ['multiscale', 6], ['sharpness', 4], ['tinted', 4], ['guts', 6], ['transistor', 3], ['sniper', 3], ['heatproof', 5],
		['salt', 5], ['bubble', 5], ['flashfire', 7], ['overlord', 8], ['vessel', 6], ['unaware', 8],
	];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	scenario.kind = kind;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const duo = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const full = ['team 123456', 'team 123456'];

	// ---- the move pools
	const plain = (id) => {
		const m = mv(id);
		return m.category !== 'Status' && m.priority === 0 && !m.multihit && !m.selfSwitch && !m.forceSwitch && !m.selfdestruct && !m.mindBlownRecoil &&
			!m.flags.charge && !m.flags.recharge && !m.self && !m.recoil && !m.drain && !m.ohko && !m.damage && !m.willCrit &&
			['normal', 'allAdjacent', 'allAdjacentFoes'].includes(m.target) && (m.accuracy === true || m.accuracy >= 90) &&
			m.basePower >= 40 && m.basePower <= 70;
	};
	const attacks = pool.moves.filter(plain);
	const physical = attacks.filter(id => mv(id).category === 'Physical');
	const special = attacks.filter(id => mv(id).category === 'Special');
	const ofType = (type, cat) => attacks.filter(id => mv(id).type === type && (!cat || mv(id).category === cat));
	const slicing = pool.moves.filter(id => mv(id).flags.slicing && plain(id));
	const sure100 = (id) => mv(id).accuracy === true || mv(id).accuracy === 100;
	const multi = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.multihit && (m.accuracy === true || m.accuracy >= 90) && m.priority === 0 && !m.selfdestruct && !m.selfSwitch && !m.flags.charge && !m.flags.recharge; });
	// (no move that always crits is accepted: the pool has only the moves with a raised crit stage, which crit one time in eight)
	const highCrit = attacks.filter(id => mv(id).critRatio >= 2);
	const selfBoosts = (pred) => pool.moves.filter(id => {
		const m = mv(id);
		return m.category === 'Status' && m.target === 'self' && m.boosts && Object.values(m.boosts).every(v => v > 0) && Object.keys(m.boosts).some(pred) && !m.volatileStatus && !m.slotCondition && !m.onHit && !m.onTry;
	});
	const atkBoosters = selfBoosts(k => ['atk', 'spa'].includes(k));
	const defBoosters = selfBoosts(k => ['def', 'spd'].includes(k));
	const evaBoosters = selfBoosts(k => k === 'evasion');
	const drops = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.self && m.self.boosts && Object.values(m.self.boosts).every(v => v < 0) && m.priority === 0 && !m.multihit && !m.selfSwitch && !m.flags.charge && !m.flags.recharge && (m.accuracy === true || m.accuracy >= 90); });
	const inaccurate = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && typeof m.accuracy === 'number' && m.accuracy >= 50 && m.accuracy <= 80 && m.priority === 0 && !m.multihit && !m.selfSwitch && !m.flags.charge && !m.flags.recharge && !m.selfdestruct && !m.ohko && !m.self; });
	const pickOr = (xs, fallback) => (xs.length ? pick(xs) : fallback());
	const fail = (what) => { throw new Error(`the pool has no ${what}`); };
	const need = (xs, what) => { if (!xs.length) fail(what); return xs; };
	need(attacks, 'plain attack'); need(slicing, 'slicing move'); need(ofType('Fire', 'Physical'), 'physical Fire move'); need(ofType('Fire', 'Special'), 'special Fire move');
	need(highCrit, 'high-crit move'); need(atkBoosters, 'Attack booster'); need(defBoosters, 'Defense booster');

	// ---- species with a property
	const canHit = (type, id) => dex.getImmunity(type, typesOf(id));
	const resists = (type, id) => canHit(type, id) && dex.getEffectiveness(type, typesOf(id)) < 0;
	const without = (...types) => real.filter(id => !typesOf(id).some(t => types.includes(t)));
	const noStatusImmunity = without('Fire', 'Steel', 'Poison', 'Electric', 'Ground');

	// ---- a set built from scratch: a species, a level, the two bulk or offence spreads, no item, the given ability
	const build = (s, o) => {
		s.species = speciesName(o.species || pick(real));
		s.level = o.level || (o.evs === offence ? 70 : 100);
		s.ability = abilityName(o.ability || 'noability');
		s.nature = 'Hardy';
		s.item = itemName(o.item || '');
		s.evs = o.evs || { hp: 252, atk: 0, def: 4, spa: 0, spd: 0, spe: 0 };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
		s.gender = undefined; delete s.gender;
	};
	// (the spreads sum past 510: Showdown does not check them. An attacker is level 70 and a holder level 100, so that the scripted turns end with both alive)
	const bulky = { hp: 252, atk: 0, def: 252, spa: 0, spd: 252, spe: 0 };
	const offence = { hp: 252, atk: 252, def: 252, spa: 252, spd: 252, spe: 0 };
	const moves = (s, ...ids) => {
		const list = [...new Set([...ids, ...IDLE.filter(hasMove)])].slice(0, 4);
		s.moves = list;
	};
	const attackerFor = (target, ...pools) => pool0(pools.flat().filter(id => canHit(mv(id).type, target)));
	const pool0 = (xs) => (xs.length ? pick(xs) : pick(attacks));
	const hitting = (xs, speciesId) => { const ok = xs.filter(id => canHit(mv(id).type, speciesId)); return ok.length ? pick(ok) : pick(xs); };
	const repeat = (n, ...pair) => Array.from({ length: n }, () => duo(...pair));

	if (kind === 'multiscale') {
		// B holds Multiscale and recovers (so that it is back at full HP, or not, when A hits); A hits at full HP, below it, and with a multi-hit move
		const v = int(3);
		build(B, { species: pick(real), ability: 'multiscale', evs: bulky });
		build(A, { species: pick(real), evs: offence });
		const strike = v === 2 && multi.length ? hitting(multi, B.species && dex.species.get(B.species).id) : hitting(attacks, dex.species.get(B.species).id);
		const other = hitting(attacks, dex.species.get(B.species).id);
		moves(A, strike, other); moves(B, 'recover', hitting(attacks, dex.species.get(A.species).id));
		scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 1', 'move 1'), duo('move 1', int(2) ? 'move 1' : 'move 2')];
	} else if (kind === 'sharpness') {
		// A holds Sharpness: slicing moves (boosted) and the others (not), against a target that is bulky and idle
		build(A, { species: pick(real), ability: 'sharpness', evs: offence });
		build(B, { species: pick(real), evs: bulky });
		const picks = [pick(slicing), hitting(attacks.filter(id => !mv(id).flags.slicing), dex.species.get(B.species).id), pick(slicing), pick(slicing)];
		moves(A, ...picks);
		moves(B, 'recover');
		scenario.choices = [full, ...[0, 1, 2, 3].map(i => duo(`move ${[1, 2, 3, 4][i] <= A.moves.length ? i + 1 : 1}`, 'move 1'))];
	} else if (kind === 'tinted') {
		// A holds Tinted Lens: one move the target resists (doubled), one it does not (not), alternately
		const pairs = [];
		for (const id of attacks) for (const sp of real) if (resists(mv(id).type, sp)) pairs.push([id, sp]);
		const [resisted, sp] = pick(pairs);
		build(A, { species: pick(real), ability: 'tintedlens', evs: offence });
		build(B, { species: sp, evs: bulky });
		const neutral = hitting(attacks.filter(id => dex.getEffectiveness(mv(id).type, typesOf(sp)) >= 0 && canHit(mv(id).type, sp)), sp);
		moves(A, resisted, neutral, hitting(attacks.filter(id => resists(mv(id).type, sp)), sp));
		moves(B, 'recover');
		scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 3', 'move 1'), duo('move 1', 'move 1')];
	} else if (kind === 'guts') {
		const v = int(3);
		const gutsSpecies = pick(noStatusImmunity);
		const facade = hasMove('facade') ? ['facade'] : [];
		build(B, { species: pick(real), evs: bulky });
		const phys1 = hitting(physical, dex.species.get(B.species).id), spec1 = hitting(special, dex.species.get(B.species).id);
		if (v === 0) {
			// Flame Orb burns the holder at the end of turn 1: Physical (not halved, boosted), Special (unchanged), Facade
			build(A, { species: gutsSpecies, ability: 'guts', item: 'flameorb', evs: offence });
			moves(A, phys1, spec1, ...facade, hitting(physical, dex.species.get(B.species).id));
			moves(B, 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo(`move ${facade.length ? 3 : 4}`, 'move 1'), duo('move 1', 'move 1')];
		} else {
			// B inflicts a status (burn, poison or paralysis) with a move; A attacks every turn
			const inflict = pick(STATUS_MOVES);
			build(A, { species: gutsSpecies, ability: 'guts', evs: offence });
			moves(A, phys1, spec1, ...facade);
			moves(B, inflict, hitting(attacks, A.species && dex.species.get(A.species).id), 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 1', 'move 3'), duo('move 2', 'move 2'), duo(`move ${facade.length ? 3 : 1}`, 'move 3'), duo('move 1', 'move 3')];
		}
	} else if (kind === 'transistor') {
		const v = int(2);
		const grounded = without('Ground');
		build(A, { species: pick(real), ability: 'transistor', evs: offence });
		build(B, { species: pick(grounded), evs: bulky });
		const elecP = ofType('Electric', 'Physical'), elecS = ofType('Electric', 'Special');
		moves(A, pickOr(elecP, () => pick(elecS)), pickOr(elecS, () => pick(elecP)), hitting(attacks.filter(id => mv(id).type !== 'Electric'), dex.species.get(B.species).id), pickOr(elecS, () => pick(elecP)));
		moves(B, 'recover');
		scenario.choices = v === 0 ? [full, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 3', 'move 1'), duo('move 4', 'move 1')] : [full, duo('move 2', 'move 1'), duo('move 1', 'move 1'), duo('move 2', 'move 1')];
	} else if (kind === 'sniper') {
		// moves with a raised crit stage (Sniper's 1.5x on each crit), repeated over many turns
		build(A, { species: pick(real), ability: 'sniper', evs: offence });
		build(B, { species: pick(real), evs: bulky });
		moves(A, pick(highCrit), pick(highCrit), pick(highCrit), hitting(attacks, dex.species.get(B.species).id));
		moves(B, 'recover', 'swordsdance');
		scenario.choices = [full, ...[1, 2, 3, 1, 2, 3, 1, 2, 3, 4].map((i, turn) => duo(`move ${i}`, turn < 6 ? 'move 1' : 'move 2'))];
	} else if (kind === 'heatproof') {
		const v = int(3);
		const heat = pick(without('Fire'));
		const fireP = pick(ofType('Fire', 'Physical')), fireS = pick(ofType('Fire', 'Special'));
		if (v === 0) {
			// Will-O-Wisp, then Fire moves of both categories at the burned holder: burn damage is halved, the Fire moves too
			build(B, { species: heat, ability: 'heatproof', evs: bulky });
			build(A, { species: pick(real), evs: offence });
			moves(A, 'willowisp', fireP, fireS, hitting(attacks, heat));
			moves(B, 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 3', 'move 1'), duo('move 4', 'move 1'), duo('move 2', 'move 1')];
		} else if (v === 1) {
			// Flame Orb on the holder, and a level or spread that gives an odd burn damage
			build(B, { species: heat, ability: 'heatproof', item: 'flameorb', level: 50 + int(51), evs: { hp: int(64) * 4, atk: 0, def: 100, spa: 0, spd: 100, spe: 0 } });
			build(A, { species: pick(real), evs: offence });
			moves(A, fireP, fireS, hitting(attacks, heat));
			moves(B, 'recover');
			scenario.choices = [full, ...repeat(4, 'move 1', 'move 1')];
		} else {
			// a level 1 to 8 holder (a maximum HP of a few tens: the burn is 1 HP, which half rounds up to 1 again), burned and idle
			build(B, { species: heat, ability: 'heatproof', level: 1 + int(8), evs: { hp: 0, atk: 0, def: 0, spa: 0, spd: 0, spe: 0 } });
			build(A, { species: pick(real), evs: offence });
			B.item = itemName('flameorb');
			moves(A, pick(IDLE.filter(hasMove)));
			moves(B, 'recover');
			scenario.choices = [full, ...repeat(3, 'move 1', 'move 1')];
		}
	} else if (kind === 'salt') {
		const v = int(3);
		const salt = pick(without('Normal', 'Fire', 'Steel', 'Poison', 'Electric', 'Ground'));
		build(B, { species: salt, ability: 'purifyingsalt', evs: bulky });
		build(A, { species: pick(real), evs: offence });
		const ghostP = ofType('Ghost', 'Physical'), ghostS = ofType('Ghost', 'Special');
		if (v === 0) {
			// Ghost moves of both categories, then the three statuses, which it refuses
			moves(A, pickOr(ghostP, () => pick(physical)), pickOr(ghostS, () => pick(special)), 'willowisp', pick(['toxic', 'thunderwave']));
			moves(B, 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 3', 'move 1'), duo('move 4', 'move 1'), duo('move 1', 'move 1')];
		} else if (v === 1) {
			// Toxic Orb or Flame Orb on the holder: the item's status is refused at the end of turn
			B.item = itemName(pick(['toxicorb', 'flameorb']));
			moves(A, pickOr(ghostP, () => pick(physical)), 'willowisp', pick(['toxic', 'thunderwave']));
			moves(B, 'recover');
			scenario.choices = [full, ...repeat(3, 'move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 3', 'move 1')];
		} else {
			// Toxic Spikes on B's side, then B switches out and back: its replacement is poisoned, the holder is not
			moves(A, 'toxicspikes', pickOr(ghostS, () => pick(special)));
			moves(B, 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'switch 2'), duo('move 2', 'switch 2'), duo('move 2', 'move 1')];
		}
	} else if (kind === 'bubble') {
		const v = int(3);
		const bubble = pick(without('Fire', 'Water'));
		const fireP = pick(ofType('Fire', 'Physical')), fireS = pick(ofType('Fire', 'Special'));
		if (v === 0) {
			// a holder against Fire moves (halved), Will-O-Wisp (refused) and a Flame Orb of its own (refused)
			build(B, { species: bubble, ability: 'waterbubble', item: int(2) ? 'flameorb' : '', evs: bulky });
			build(A, { species: pick(real), evs: offence });
			moves(A, fireP, fireS, 'willowisp', hitting(attacks, bubble));
			moves(B, 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo('move 3', 'move 1'), duo('move 4', 'move 1'), duo('move 1', 'move 1')];
		} else {
			// a holder that attacks: Water moves of both categories (doubled) and another type (not), and a foe that tries to burn it
			build(A, { species: bubble, ability: 'waterbubble', item: v === 2 ? 'flameorb' : '', evs: offence });
			build(B, { species: pick(real), evs: bulky });
			const waterP = ofType('Water', 'Physical'), waterS = ofType('Water', 'Special');
			moves(A, pickOr(waterP, () => pick(waterS)), pickOr(waterS, () => pick(waterP)), hitting(attacks.filter(id => mv(id).type !== 'Water'), dex.species.get(B.species).id));
			moves(B, 'willowisp', 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 3', 'move 1'), duo('move 1', 'move 1')];
		}
	} else if (kind === 'flashfire') {
		// B holds Flash Fire: absorbs (and is boosted), absorbs again, is hit by a Will-O-Wisp, switches out (the volatile goes) and returns
		const v = int(3);
		const holder = pick(real);
		build(B, { species: holder, ability: 'flashfire', evs: bulky });
		// (its replacement takes a hit too, and must outlast it)
		build(scenario.teams[bIdx][1], { species: pick(real), evs: bulky });
		build(A, { species: pick(real), evs: offence });
		const fireP = pick(ofType('Fire', 'Physical')), fireS = pick(ofType('Fire', 'Special'));
		moves(A, v === 2 ? fireP : fireS, 'willowisp', hitting(attacks, holder), fireP);
		moves(B, fireP, fireS, hitting(attacks, dex.species.get(A.species).id), 'recover');
		const first = duo('move 1', int(2) ? 'move 1' : 'move 2');
		scenario.choices = v === 0
			? [full, first, duo('move 1', 'move 2'), duo('move 3', 'switch 2'), duo('move 1', 'switch 2'), duo('move 1', 'move 1'), duo('move 4', 'move 2')]
			: v === 1 ? [full, duo('move 2', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 1'), duo('move 3', 'switch 2'), duo('move 4', 'switch 2'), duo('move 1', 'move 1')]
				: [full, duo('move 1', 'move 4'), duo('move 3', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 1'), duo('move 3', 'switch 2'), duo('move 1', 'switch 2'), duo('move 1', 'move 1')];
	} else if (kind === 'overlord') {
		// A's five other members are level 1, and B (level 100) knocks each out with an accurate Water move: the holder (original member 6) comes
		// in after k faints, k in 0..5; with the variant it is switched out for a member that is then knocked out, and returns with one more
		const k = [0, 1, 2, 3, 4, 5][int(6)];
		const again = int(3) === 0 && k >= 1 && k <= 4;
		const waters = attacks.filter(id => mv(id).type === 'Water' && sure100(id) && mv(id).basePower >= 60);
		need(waters, 'accurate Water move');
		for (let i = 0; i < 5; i++) { build(scenario.teams[aIdx][i], { species: pick(real), level: 1, evs: { hp: 0, atk: 0, def: 0, spa: 0, spd: 0, spe: 0 } }); moves(scenario.teams[aIdx][i], pick(IDLE.filter(hasMove))); }
		const H = scenario.teams[aIdx][5];
		// (a holder that Water does not hit super effectively, at level 100, so that it outlasts B's attacks)
		build(H, { species: pick(real.filter(id => dex.getEffectiveness('Water', typesOf(id)) <= 0)), ability: 'supremeoverlord', level: 100, evs: offence });
		build(B, { species: pick(real), evs: bulky });
		const strike = hitting(attacks, dex.species.get(B.species).id);
		moves(H, strike, hitting(attacks, dex.species.get(B.species).id));
		moves(B, pick(waters), 'recover');
		// A's side.pokemon order and who is out (a switch swaps the active member, at position 1, and the one chosen)
		const order = k === 0 ? [5, 1, 2, 3, 4, 0] : [0, 1, 2, 3, 4, 5];
		const down = new Set();
		const switchTo = (orig) => { const i = order.indexOf(orig); const text = `switch ${i + 1}`; [order[0], order[i]] = [order[i], order[0]]; return text; };
		const forced = (text) => { const pair = [null, null]; pair[aIdx] = text; return pair; };
		const preview = ['team 123456', 'team 123456'];
		if (k === 0) preview[aIdx] = 'team 612345';
		const script = [preview];
		// the k faints: a turn (the active weak member is knocked out), then the forced switch to the next weak member, or to the holder
		let active = order[0];
		for (let j = 1; j <= k; j++) {
			script.push(duo('move 1', 'move 1'));
			down.add(active);
			const next = j < k ? order.slice(1).find(o => o !== 5 && !down.has(o)) : 5;
			script.push(forced(switchTo(next)));
			active = next;
		}
		script.push(duo('move 1', 'move 1'));
		const weak = order.slice(1).find(o => o !== 5 && !down.has(o));
		if (again && weak !== undefined) {
			// the holder switches to a weak member, which B knocks out; the holder comes back with one more fallen
			script.push(duo(switchTo(weak), 'move 1'));
			down.add(weak);
			script.push(forced(switchTo(5)));
			script.push(duo('move 1', 'move 1'), duo('move 2', 'move 1'));
		} else {
			script.push(duo('move 2', 'move 1'));
		}
		scenario.choices = script;
	} else if (kind === 'vessel') {
		const v = int(4);
		const spA = (against) => hitting(special, against), phA = (against) => hitting(physical, against);
		const aSp = pick(real), bSp = v === 3 ? aSp : pick(real);
		build(A, { species: aSp, ability: v === 1 ? 'noability' : 'vesselofruin', evs: bulky });
		build(B, { species: bSp, ability: v === 2 ? 'noability' : 'vesselofruin', evs: bulky });
		if (v === 3) { for (const k of Object.keys(A)) if (k !== 'moves') B[k] = JSON.parse(JSON.stringify(A[k])); A.ability = abilityName('vesselofruin'); B.ability = abilityName('vesselofruin'); }
		moves(A, spA(dex.species.get(B.species).id), phA(dex.species.get(B.species).id), spA(dex.species.get(B.species).id));
		moves(B, spA(dex.species.get(A.species).id), phA(dex.species.get(A.species).id), spA(dex.species.get(A.species).id));
		if (v === 3) B.moves = [...A.moves];
		scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 3'), duo('move 3', 'move 1'), duo('move 1', 'move 1')];
	} else if (kind === 'unaware') {
		const v = int(6);
		const U = A, F = B;
		const uSp = pick(real), fSp = pick(real);
		const uId = dex.species.get(uSp).id, fId = dex.species.get(fSp).id;
		build(U, { species: uSp, ability: 'unaware', evs: bulky });
		build(F, { species: fSp, evs: offence });
		if (v === 0) {
			// the foe boosts (twice), then attacks: Unaware's defender ignores the boosts
			moves(F, pick(atkBoosters), pick(atkBoosters), hitting(attacks, uId), hitting(attacks, uId));
			moves(U, hitting(attacks, fId), 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 2', 'move 3'), duo('move 2', 'move 4'), duo('move 2', 'move 3')];
		} else if (v === 1) {
			// the foe boosts and attacks with moves that always crit (a crit ignores the target's Defense boosts only; here the attacker's Attack is ignored)
			moves(F, pick(atkBoosters), pick(highCrit), hitting(attacks, uId));
			moves(U, hitting(attacks, fId), 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 2'), duo('move 1', 'move 3')];
		} else if (v === 2) {
			// the foe boosts its Defense and Special Defense, and Unaware attacks into them (and with an always-crit move)
			build(U, { species: uSp, ability: 'unaware', evs: offence });
			build(F, { species: fSp, evs: bulky });
			moves(F, pick(defBoosters), pick(defBoosters), 'recover');
			moves(U, hitting(physical, fId), hitting(special, fId), pick(highCrit));
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 3', 'move 1'), duo('move 1', 'move 3'), duo('move 2', 'move 1')];
		} else if (v === 3) {
			// evasion and accuracy: the foe raises its evasion, and Unaware attacks with moves that can miss (and the foe, moves that can miss)
			build(U, { species: uSp, ability: 'unaware', evs: offence });
			build(F, { species: fSp, evs: bulky });
			const evasion = evaBoosters.length ? pick(evaBoosters) : pick(defBoosters);
			moves(F, evasion, pickOr(inaccurate, () => pick(attacks)), 'recover');
			moves(U, pickOr(inaccurate, () => pick(attacks)), pickOr(inaccurate, () => pick(attacks)), hitting(attacks, fId));
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 2', 'move 2'), duo('move 3', 'move 2'), duo('move 1', 'move 2')];
		} else if (v === 4) {
			// stage interactions: the foe lowers its own Attack (a move that drops it) and attacks into Unaware, with critical hits, after boosting
			build(U, { species: uSp, ability: 'unaware', evs: bulky });
			const drop = drops.length ? pick(drops) : pick(attacks);
			moves(F, pick(atkBoosters), drop, pick(highCrit), hitting(attacks, uId));
			moves(U, hitting(attacks, fId), 'recover');
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 3'), duo('move 1', 'move 2'), duo('move 1', 'move 4')];
		} else {
			// two Unaware holders (mirror teams: the same speed, so that the handlers tie in every sort), boosting and attacking each other
			for (const k of Object.keys(U)) if (k !== 'moves') F[k] = JSON.parse(JSON.stringify(U[k]));
			U.ability = abilityName('unaware'); F.ability = abilityName('unaware');
			moves(U, pick(atkBoosters), pick(defBoosters), hitting(attacks, uId), hitting(attacks, uId));
			F.moves = [...U.moves];
			scenario.choices = [full, duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 3', 'move 3'), duo('move 4', 'move 4'), duo('move 3', 'move 3')];
		}
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// The tally. Every wrapper passes its arguments and result through.

function newTally() {
	return {
		multiscale: { full: 0, below: 0, multihitFirstHit: 0, multihitLaterHit: 0 },
		sharpness: { slicing: 0, notSlicing: 0 },
		tintedLens: { resisted: 0, notResisted: 0 },
		guts: { physicalWithStatus: 0, specialWithStatus: 0, noStatus: 0, burnedPhysicalNotHalved: 0, burn: 0, poison: 0, paralysis: 0, facadeBurned: 0 },
		transistor: { electricPhysical: 0, electricSpecial: 0, notElectric: 0 },
		sniper: { crit: 0, noCrit: 0 },
		heatproof: { fireAttack: 0, fireSpecial: 0, notFire: 0, burnDamage: 0, burnDamageOdd: 0, burnDamageOne: 0 },
		purifyingSalt: { ghostAttack: 0, ghostSpecial: 0, notGhost: 0, refusedByMove: 0, refusedByItem: 0, refusedByHazard: 0 },
		waterBubble: { fireHalved: 0, waterDoubled: 0, burnRefusedByMove: 0, burnRefusedByItem: 0 },
		flashFire: { absorbed: 0, absorbedAgain: 0, absorbedStatusMove: 0, boostedPhysical: 0, boostedSpecial: 0, fireWithoutVolatile: 0, lostOnSwitch: 0 },
		supremeOverlord: { start0: 0, start1: 0, start2: 0, start3: 0, start4: 0, start5: 0, power0: 0, power1: 0, power2: 0, power3: 0, power4: 0, power5: 0 },
		vesselOfRuin: { lowered: 0, physicalUnchanged: 0, attackerHolds: 0, bothHold: 0, bothHoldTied: 0 },
		unaware: {
			attackIgnored: 0, specialAttackIgnored: 0, defenseIgnored: 0, specialDefenseIgnored: 0, evasionIgnored: 0, accuracyIgnored: 0,
			critIntoBoostedDefense: 0, critBoostedAttackIgnored: 0, negativeStageIgnored: 0, bothHold: 0,
		},
	};
}
let activeTally = null;
const t = () => activeTally;
const hasVolatile = (p, id) => !!(p && p.volatiles && p.volatiles[id]);

function install() {
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, relayVar, ...rest) {
		const c = t();
		if (!c) return runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
		const move = sourceEffect && sourceEffect.effectType === 'Move' ? sourceEffect : null;
		switch (eventid) {
		case 'ModifyDamage': {
			// target: the attacker; source: the defender (the target of the move)
			if (move && target && source) {
				const hit = source.getMoveHitData(move);
				if (source.ability === 'multiscale') {
					if (source.hp >= source.maxhp) { c.multiscale.full++; if (move.multihit && move.hit === 1) c.multiscale.multihitFirstHit++; }
					else { c.multiscale.below++; if (move.multihit && move.hit > 1) c.multiscale.multihitLaterHit++; }
				}
				if (target.ability === 'tintedlens') { if (hit.typeMod < 0) c.tintedLens.resisted++; else c.tintedLens.notResisted++; }
				if (target.ability === 'sniper') { if (hit.crit) c.sniper.crit++; else c.sniper.noCrit++; }
				if (hit.crit && source.ability === 'unaware' && source.side !== target.side && target.boosts[move.category === 'Physical' ? 'atk' : 'spa'] > 0) c.unaware.critBoostedAttackIgnored++;
				if (hit.crit && target.ability === 'unaware' && source.boosts[move.overrideDefensiveStat || (move.category === 'Physical' ? 'def' : 'spd')] > 0) c.unaware.critIntoBoostedDefense++;
			}
			break;
		}
		case 'BasePower': {
			// target: the attacker
			if (move && target && target.ability === 'sharpness') { if (move.flags.slicing) c.sharpness.slicing++; else c.sharpness.notSlicing++; }
			if (move && target && target.ability === 'supremeoverlord') {
				const fallen = target.abilityState.fallen || 0;
				c.supremeOverlord['power' + fallen]++;
			}
			break;
		}
		case 'ModifyAtk': case 'ModifySpA': {
			// target: the attacker; source: the defender
			if (move && target && source) {
				const phys = eventid === 'ModifyAtk';
				if (target.ability === 'guts') {
					if (target.status) {
						if (phys) c.guts.physicalWithStatus++; else c.guts.specialWithStatus++;
						if (phys && target.status === 'brn') c.guts.burnedPhysicalNotHalved++;
						if (target.status === 'brn') c.guts.burn++;
						if (target.status === 'psn' || target.status === 'tox') c.guts.poison++;
						if (target.status === 'par') c.guts.paralysis++;
						if (phys && target.status === 'brn' && move.id === 'facade') c.guts.facadeBurned++;
					} else c.guts.noStatus++;
				}
				if (target.ability === 'transistor') {
					if (move.type === 'Electric') { if (phys) c.transistor.electricPhysical++; else c.transistor.electricSpecial++; } else c.transistor.notElectric++;
				}
				if (source.ability === 'heatproof') { if (move.type === 'Fire') { if (phys) c.heatproof.fireAttack++; else c.heatproof.fireSpecial++; } else c.heatproof.notFire++; }
				if (source.ability === 'purifyingsalt') { if (move.type === 'Ghost') { if (phys) c.purifyingSalt.ghostAttack++; else c.purifyingSalt.ghostSpecial++; } else c.purifyingSalt.notGhost++; }
				if (source.ability === 'waterbubble' && move.type === 'Fire') c.waterBubble.fireHalved++;
				if (target.ability === 'waterbubble' && move.type === 'Water') c.waterBubble.waterDoubled++;
				if (move.type === 'Fire' && hasVolatile(target, 'flashfire') && target.ability === 'flashfire') { if (phys) c.flashFire.boostedPhysical++; else c.flashFire.boostedSpecial++; }
				if (move.type === 'Fire' && target.ability === 'flashfire' && !hasVolatile(target, 'flashfire')) c.flashFire.fireWithoutVolatile++;
				if (eventid === 'ModifySpA') {
					const holders = [target, source].filter(p => p.ability === 'vesselofruin');
					if (holders.length === 2) { c.vesselOfRuin.bothHold++; if (target.speed === source.speed) c.vesselOfRuin.bothHoldTied++; }
					else if (target.ability === 'vesselofruin') c.vesselOfRuin.attackerHolds++;
					else if (source.ability === 'vesselofruin') c.vesselOfRuin.lowered++;
				} else if (source.ability === 'vesselofruin' && target.ability !== 'vesselofruin') c.vesselOfRuin.physicalUnchanged++;
			}
			break;
		}
		case 'Damage': {
			// target: the damaged Pokemon; sourceEffect: the effect
			if (target && target.ability === 'heatproof' && sourceEffect && sourceEffect.id === 'brn') {
				c.heatproof.burnDamage++;
				if (relayVar % 2 === 1) c.heatproof.burnDamageOdd++;
				if (relayVar === 1) c.heatproof.burnDamageOne++;
			}
			break;
		}
		case 'SetStatus': {
			// target: the Pokemon, sourceEffect: the cause, relayVar: the status
			if (target && (target.ability === 'purifyingsalt' || (target.ability === 'waterbubble' && relayVar && relayVar.id === 'brn'))) {
				const r = runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
				if (!r) {
					const kind = sourceEffect && sourceEffect.effectType === 'Move' ? 'ByMove' : sourceEffect && sourceEffect.effectType === 'Item' ? 'ByItem' : 'ByHazard';
					if (target.ability === 'purifyingsalt') c.purifyingSalt['refused' + kind]++;
					else if (kind !== 'ByHazard') c.waterBubble['burnRefused' + kind]++;
				}
				return r;
			}
			break;
		}
		case 'TryHit': {
			// target: the targets (an array); source: the attacker
			target = Array.isArray(target) ? target[0] : target;
			if (target && target.ability === 'flashfire' && sourceEffect && sourceEffect.effectType === 'Move' && sourceEffect.type === 'Fire' && target !== source) {
				const had = hasVolatile(target, 'flashfire');
				if (had) c.flashFire.absorbedAgain++; else c.flashFire.absorbed++;
				if (sourceEffect.category === 'Status') c.flashFire.absorbedStatusMove++;
			}
			return runEvent.call(this, eventid, Array.isArray(arguments[1]) ? arguments[1] : target, source, sourceEffect, relayVar, ...rest);
		}
		case 'ModifyBoost': {
			if (target) {
				const before = Object.assign({}, relayVar);
				const r = runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
				const changed = (k) => before[k] !== undefined && before[k] !== 0 && r[k] === 0;
				const am = this.activeMove;
				if (this.getAllActive().filter(p => p.ability === 'unaware').length === 2 && target.ability === 'unaware') c.unaware.bothHold++;
				if (changed('atk')) c.unaware.attackIgnored++;
				if (changed('spa')) c.unaware.specialAttackIgnored++;
				if (changed('def')) c.unaware.defenseIgnored++;
				if (changed('spd')) c.unaware.specialDefenseIgnored++;
				if (changed('evasion')) c.unaware.evasionIgnored++;
				if (changed('accuracy')) c.unaware.accuracyIgnored++;
				if ((['atk', 'spa', 'def', 'spd', 'accuracy', 'evasion'].some(k => before[k] < 0 && r[k] === 0)) && am) c.unaware.negativeStageIgnored++;
				return r;
			}
			break;
		}
		default: break;
		}
		return runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
	};
	const removeVolatile = Pokemon.prototype.removeVolatile;
	Pokemon.prototype.removeVolatile = function (status, ...rest) {
		const c = t();
		const id = typeof status === 'string' ? status : status.id;
		// the ability's onEnd (a switch-out or a faint) takes the volatile away
		if (c && id === 'flashfire' && this.volatiles.flashfire && this.battle.event && this.battle.event.id === 'End') c.flashFire.lostOnSwitch++;
		return removeVolatile.call(this, status, ...rest);
	};
	// a holder switching in, at the count of its side's faints (Supreme Overlord's onStart reads it)
	const fieldEvent = Battle.prototype.fieldEvent;
	Battle.prototype.fieldEvent = function (eventid, targets, ...rest) {
		const c = t();
		if (c && eventid === 'SwitchIn' && targets) {
			for (const p of targets) if (p.ability === 'supremeoverlord' && p.hp) c.supremeOverlord['start' + Math.min(p.side.totalFainted, 5)]++;
		}
		return fieldEvent.call(this, eventid, targets, ...rest);
	};
}

// counters that cannot be hit by construction, or are rare by nature
// - guts.facadeBurned: Facade is scripted for Guts only when the pool has it
// - heatproof.burnDamageOne: needs a holder whose burn is 1 HP (a level 1 to 8 holder), scripted
// - heatproof.fireSpecial / fireAttack and the like are scripted
const STRUCTURAL_ZERO = [];

function recordScenario(scenario) {
	const before = JSON.stringify(activeTally);
	try {
		return { trace: record(scenario), scripted: !!scenario.choices };
	} catch (e) {
		if (!scenario.choices) throw e;
		activeTally = JSON.parse(before);
		const plain = Object.assign({}, scenario);
		delete plain.choices;
		return { trace: record(plain), scripted: false, dropped: e.message };
	}
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice8b.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice8b-${k}.json`), JSON.stringify(pool[k]));
	}
	activeTally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scriptedCount = 0, dropped = 0;
	const byKind = {};
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scriptedCount++;
		byKind[scenario.kind] = byKind[scenario.kind] || { n: 0, dropped: 0 };
		byKind[scenario.kind].n++;
		if (result.dropped) {
			dropped++; byKind[scenario.kind].dropped++;
			if (process.env.SLICE8B_VERBOSE) console.error(`seed ${seed}: script dropped (${scenario.kind}, ${(scenario.choices || []).length} steps): ${result.dropped}`);
		}
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice8b-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice8b seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error(`kinds: ${JSON.stringify(byKind)}`);
	console.error('tally (from Showdown\'s own events):');
	const zeros = [];
	const walk = (prefix, v) => {
		if (typeof v === 'number') { if (v === 0 && !STRUCTURAL_ZERO.includes(prefix)) zeros.push(prefix); return; }
		for (const [k, x] of Object.entries(v)) walk(prefix ? `${prefix}.${k}` : k, x);
	};
	for (const [k, v] of Object.entries(activeTally)) {
		console.error(`  ${k}: ${JSON.stringify(v)}`);
		walk(k, v);
	}
	if (zeros.length) console.error(`ZERO COUNTERS (not structural): ${zeros.join(', ')}`);
	else console.error('no counter is zero except the structural ones');
}

module.exports = { assign, install, newTally, setTally: (x) => { activeTally = x; } };
