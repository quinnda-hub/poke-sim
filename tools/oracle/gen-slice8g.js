#!/usr/bin/env node
'use strict';
// Generates battles for "slice 8g" (the combined set for the slice 8 abilities) and records each on Showdown.
//
// Slice 8 built 58 abilities in six phases (8a stat stages and switch-in, 8b damage and stats, 8c immunities and damage guards, 8d contact
// and item abilities, 8e the move pipeline, 8f suppression and reflection). Each phase's set holds mostly its own abilities. This set records
// their interactions, and every ability the slice adds is on some team. About 30% of the battles are plain random play over teams with the
// slice's abilities sprinkled on the sets (and the moves and items that meet them); the rest are scripted openings (a pair of choices per
// decision point; the random policy takes over when the script ends, and a script Showdown rejects is dropped and the battle recorded
// without it):
//   breaker    Mold Breaker (or a control with no ability) against every breakable ability a move can meet: Clear Body, Big Pecks, Keen Eye,
//              Inner Focus, Guard Dog, Contrary, Mirror Armor, Multiscale, Heatproof, Purifying Salt, Water Bubble, Flash Fire, Unaware, Good
//              as Gold, Water Absorb, Levitate, Sturdy, Sap Sipper, Soundproof, Bulletproof, Shell Armor, Shield Dust, Sweet Veil
//   gasenter   Neutralizing Gas entering (from the bench, or on the lead) beside on-switch abilities (Intimidate, Dauntless Shield, Supreme
//              Overlord), damage guards (Magic Guard, Multiscale), residuals (Speed Boost, Poison Heal) and Pressure, and leaving (a switch)
//   gasfaint   the gas holder fainting while the foe's abilities restart (Intimidate, Supreme Overlord), and the foe's abilities entering
//              under the gas
//   gastwo     two gas holders
//   gasintim   Intimidate meeting Contrary, Defiant, Mirror Armor, Guard Dog (and others) with Neutralizing Gas on either side: entering,
//              suppressed, restarted when the gas leaves
//   bounce     Magic Bounce against a Pressure user (a move both know), a Prankster user's status move (into a Dark and a non-Dark type),
//              Libero and Protean, Mirror Armor (Growl, Screech), Synchronize (Stun Spore, Toxic, Will-O-Wisp) and Mold Breaker
//   guarddog   Guard Dog at Attack -6 and just above it against Intimidate (Charm and Feather Dance, or Parting Shot, repeated, then Intimidate
//              switched in again and again)
//   unaware    Unaware on either side against Intimidate, Defiant, Contrary, Speed Boost and Weak Armor stages
//   guardhit   Magic Guard against Rough Skin and Rocky Helmet (and Life Orb recoil)
//   sturdy     Sturdy against multi-hit moves, and against Berserk crossing half HP (a Rocky Helmet hit, a multi-hit move into Berserk)
//   flashfire  Flash Fire against Protect and Substitute (the order of the TryHit handlers)
//   moxie      Moxie and Supreme Overlord counts (a double knockout by Explosion, then knockouts by the replacements)
//   contrary   Contrary with Defiant drops (stat-lowering moves, self drops and Intimidate on both sides)
//   faintend   a Pokemon dragged onto hazards after the last foe fainted ends the battle at 0 HP and not fainted (Shedinja, Rocky Helmet,
//              Dragon Tail or Circle Throw over Stealth Rock or Spikes)
//
// Usage: node gen-slice8g.js <outDir> <count> <poolFile> [firstSeed=1]
// Output: <outDir>/slice8g-<seed>.json and slice8g-{moves,species,abilities,items}.json (copies of the pool). A tally of each branch is
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

// the 58 abilities of slice 8 (the pool of 8g minus the pool of 7f)
const NEW_ABILITIES = [
	'baddreams', 'berserk', 'bigpecks', 'bulletproof', 'clearbody', 'contrary', 'dauntlessshield', 'defiant', 'flamebody', 'flashfire',
	'goodasgold', 'gooey', 'guarddog', 'guts', 'heatproof', 'infiltrator', 'innerfocus', 'intimidate', 'justified', 'keeneye', 'levitate',
	'libero', 'liquidvoice', 'magicbounce', 'magicguard', 'magician', 'mirrorarmor', 'moldbreaker', 'moxie', 'multiscale', 'neutralizinggas',
	'pickpocket', 'poisonheal', 'prankster', 'pressure', 'protean', 'purifyingsalt', 'roughskin', 'sapsipper', 'sharpness', 'shellarmor',
	'shielddust', 'sniper', 'soundproof', 'speedboost', 'static', 'sturdy', 'supremeoverlord', 'sweetveil', 'synchronize', 'tintedlens',
	'toxicchain', 'transistor', 'unaware', 'vesselofruin', 'waterabsorb', 'waterbubble', 'weakarmor',
];
const PIVOTS = ['uturn', 'flipturn', 'voltswitch', 'teleport', 'partingshot', 'chillyreception'];
const FORCERS = ['roar', 'whirlwind', 'dragontail', 'circlethrow'];
const BREAKABLE = [
	'clearbody', 'bigpecks', 'keeneye', 'innerfocus', 'guarddog', 'contrary', 'mirrorarmor', 'multiscale', 'heatproof', 'purifyingsalt', 'waterbubble',
	'flashfire', 'unaware', 'goodasgold', 'waterabsorb', 'levitate', 'sturdy', 'sapsipper', 'soundproof', 'bulletproof', 'shellarmor', 'shielddust', 'sweetveil',
];

const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

let lastKind = 'random';
const teamAbilities = new Set();

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2654435761 + 40503);
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
	const REFL = REFLECT.filter(id => !mv(id).selfSwitch && id !== 'memento');
	const inert = (id) => !PIVOTS.includes(id) && !FORCERS.includes(id) && id !== 'encore' && id !== 'taunt' && !REFLECT.includes(id);
	const statuses = pool.moves.filter(id => mv(id).category === 'Status' && inert(id));
	const quiet = statuses.filter(id => ['self', 'allySide'].includes(mv(id).target) && !mv(id).weather && !mv(id).sideCondition && !mv(id).terrain);
	const plainAttack = (m) => m.category !== 'Status' && m.basePower >= 40 && (m.accuracy === true || m.accuracy >= 90) && m.priority === 0 &&
		!m.flags.charge && !m.flags.recharge && !m.selfdestruct && !m.mindBlownRecoil && !m.multihit && !m.selfSwitch && !m.forceSwitch;
	const attacks = pool.moves.filter(id => inert(id) && plainAttack(mv(id)));
	const inertPool = pool.moves.filter(inert);
	const filt = (pred) => pool.moves.filter(id => { const m = mv(id); return inert(id) || REFLECT.includes(id) ? pred(m) : false; });
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves[int(4)] = id;
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const set = (s, ...ids) => {
		s.moves = [...new Set(ids.filter(Boolean))].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(inertPool); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const steady = (s, level, spe) => {
		s.level = level; s.item = ''; s.ability = 'No Ability'; s.nature = 'Hardy';
		s.evs = { hp: 252, atk: 0, def: 0, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
	};
	const ab = (s, id) => { s.ability = abilityName(id); };
	const item = (s, id) => { s.item = itemName(id); };

	// sprinkle: the slice's abilities on the sets, and the moves and items that meet them
	const SPRINKLE_MOVES = [...REFLECT, 'protect', 'substitute', 'willowisp', 'toxic', 'partingshot', 'charm', 'swordsdance', 'dragontail', 'uturn', 'stealthrock', 'spikes']
		.filter(has);
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 55) ab(s, pick(NEW_ABILITIES));
		if (int(100) < 45) put(s, pick(SPRINKLE_MOVES));
		if (int(100) < 25) put(s, pick(SPRINKLE_MOVES));
		const r = int(100);
		if (r < 10) item(s, 'rockyhelmet'); else if (r < 16) item(s, 'lifeorb'); else if (r < 20) item(s, 'toxicorb'); else if (r < 24) item(s, 'flameorb');
	}
	const kinds = [
		['random', 30], ['breaker', 12], ['gasenter', 10], ['gasfaint', 3], ['gastwo', 2], ['gasintim', 6], ['bounce', 12], ['guarddog', 4], ['unaware', 5],
		['guardhit', 3], ['sturdy', 3], ['flashfire', 3], ['moxie', 2], ['contrary', 3], ['faintend', 2],
	];
	let kind = 'random', acc = 0;
	const roll = int(100);
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	lastKind = kind;
	for (const side of [0, 1]) for (const s of scenario.teams[side]) teamAbilities.add(s.ability);
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	let A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const duo = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const teams = (an, bn) => { const pair = [null, null]; pair[aIdx] = `team ${[1, 2, 3, 4, 5, 6].slice(0, an).join('')}`; pair[bIdx] = `team ${[1, 2, 3, 4, 5, 6].slice(0, bn).join('')}`; return pair; };
	const full = () => teams(6, 6);
	const benchA = (i) => scenario.teams[aIdx][i], benchB = (i) => scenario.teams[bIdx][i];
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
	const teraMaybe = (m) => (int(100) < 15 ? `${m} terastallize` : m);
	const aFast = int(2) === 0;
	const types = (id) => dex.species.get(id).types;
	const speciesIdOf = (s) => dex.species.get(s.species).id;
	const effective = (s) => {
		const ts = types(speciesIdOf(s));
		const se = attacks.filter(id => dex.getEffectiveness(mv(id).type, ts) > 0 && mv(id).basePower >= 60 && dex.getImmunity(mv(id).type, ts));
		return se.length ? pick(se) : pick(attacks);
	};
	const mild = attacks.filter(id => mv(id).basePower <= 80);
	const strike = () => pick(mild);
	// a move of a kind (or the fallback) that the target species can take
	const hits = (id, s) => dex.getImmunity(mv(id).type, types(speciesIdOf(s)));
	const kindOf = (pred, fallback) => { const l = filt(pred); return l.length ? l : (fallback || attacks); };
	const pickHit = (l, s) => { const ok = l.filter(id => hits(id, s)); return pick(ok.length ? ok : l); };
	// stat-lowering moves aimed at the foe, by stat
	const drops = (stat) => REFL.filter(id => { const b = mv(id).boosts; return b && b[stat] < 0 && mv(id).target === 'normal'; });
	const contact = mild.filter(id => mv(id).flags.contact);
	const multi = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.multihit && (m.accuracy === true || m.accuracy >= 90) && m.priority === 0 && !m.selfdestruct; });
	const burnOrPsn = REFLECT.filter(id => ['brn', 'psn', 'tox'].includes(mv(id).status));

	if (kind === 'breaker') {
		// A (Mold Breaker, or the control) attacks B, which holds one breakable ability, with the kind of move that ability stops
		const X = pick(BREAKABLE);
		duel(aFast, 100, 100);
		ab(A, int(100) < 78 ? 'moldbreaker' : 'noability'); ab(B, X);
		const strongA = () => strike();
		let m1, m2, m3;
		const first = () => kindOf(m => m.category !== 'Status' && m.basePower >= 60);
		switch (X) {
			case 'clearbody': m1 = pick(drops('atk').concat(drops('def'), drops('spe'))); m2 = pick(drops('spa').concat(drops('spd'))); break;
			case 'bigpecks': m1 = pick(drops('def')); m2 = pick(drops('def')); break;
			case 'keeneye': m1 = pick(drops('accuracy')); m2 = pick(drops('accuracy')); break;
			case 'innerfocus': m1 = has('fakeout') ? 'fakeout' : pick(drops('atk')); m2 = pick(kindOf(m => m.category !== 'Status' && m.secondary && m.secondary.volatileStatus === 'flinch')); break;
			case 'guarddog': m1 = pick(['roar', 'whirlwind', 'dragontail', 'circlethrow'].filter(has)); m2 = pick(['roar', 'dragontail', 'circlethrow', 'whirlwind'].filter(has)); break;
			case 'contrary': m1 = pick(drops('atk').concat(drops('spa'), drops('def'))); m2 = pick(drops('spe').concat(drops('spd'))); break;
			case 'mirrorarmor': m1 = pick(drops('atk').concat(drops('def'))); m2 = pick(drops('spd').concat(drops('spa'), drops('spe'))); break;
			case 'multiscale': m1 = strongA(B); m2 = strongA(B); break;
			case 'heatproof': m1 = pickHit(kindOf(m => m.type === 'Fire' && m.category !== 'Status'), B); m2 = pick(['willowisp'].filter(has).concat([strongA(B)])); break;
			case 'purifyingsalt': m1 = pick(burnOrPsn); m2 = pickHit(kindOf(m => m.type === 'Ghost' && m.category !== 'Status'), B); break;
			case 'waterbubble': m1 = pickHit(kindOf(m => m.type === 'Fire' && m.category !== 'Status'), B); m2 = pick(['willowisp'].filter(has).concat([m1])); break;
			case 'flashfire': m1 = pickHit(kindOf(m => m.type === 'Fire' && m.category !== 'Status'), B); m2 = pick(['willowisp'].filter(has).concat([m1])); break;
			case 'unaware': m1 = pick(['swordsdance', 'nastyplot', 'bulkup', 'calmmind', 'dragondance'].filter(has)); m2 = strongA(B); break;
			case 'goodasgold': m1 = pick(statuses.concat(REFL)); m2 = pick(REFL); break;
			case 'waterabsorb': m1 = pickHit(kindOf(m => m.type === 'Water' && m.category !== 'Status'), B); m2 = m1; break;
			case 'levitate': m1 = pickHit(kindOf(m => m.type === 'Ground' && m.category !== 'Status'), B); m2 = pick(['spikes', 'toxicspikes'].filter(has).concat([m1])); break;
			case 'sturdy': m1 = pick(multi.concat(attacks.filter(id => mv(id).basePower >= 90))); m2 = pick(['fissure', 'guillotine', 'sheercold', 'horndrill'].filter(has).concat([strongA(B)])); break;
			case 'sapsipper': m1 = pickHit(kindOf(m => m.type === 'Grass' && m.category !== 'Status'), B); m2 = pick(['leechseed', 'spore', 'sleeppowder', 'stunspore'].filter(has).concat([m1])); break;
			case 'soundproof': m1 = pickHit(kindOf(m => m.flags.sound && m.category !== 'Status'), B); m2 = pick(kindOf(m => m.flags.sound, [m1])); break;
			case 'bulletproof': m1 = pickHit(kindOf(m => m.flags.bullet && m.category !== 'Status'), B); m2 = pick(kindOf(m => m.flags.bullet, [m1])); break;
			case 'shellarmor': m1 = pickHit(kindOf(m => m.willCrit && m.category !== 'Status'), B); m2 = m1; break;
			case 'sweetveil': m1 = pick(['spore', 'sleeppowder', 'hypnosis', 'yawn', 'lovelykiss', 'darkvoid', 'grasswhistle'].filter(has).concat(['toxic'].filter(has))); m2 = m1; break;
			case 'shielddust': m1 = pickHit(kindOf(m => m.category !== 'Status' && m.secondary && m.secondary.chance >= 30 && m.secondary.status), B); m2 = pickHit(kindOf(m => m.category !== 'Status' && m.secondary && m.secondary.boosts), B); break;
		}
		m3 = strike();
		set(A, m1, m2, m3, idle());
		set(B, strike(), idle(), strike(), idle());
		const x = teraMaybe;
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 3'), duo(x('move 3'), 'move 1'), duo('move 2', 'move 1'), duo('move 1', 'move 3')];
	} else if (kind === 'gasenter' || kind === 'gasfaint') {
		// A holds the gas; B's lead holds K (B's bench too, for the on-switch kinds)
		const K = pick(['intimidate', 'dauntlessshield', 'supremeoverlord', 'magicguard', 'multiscale', 'speedboost', 'poisonheal', 'pressure']);
		const variant = kind === 'gasfaint' ? 'faint' : pick(['lead', 'lead', 'glead', 'in']);
		const guardK = ['magicguard', 'multiscale', 'speedboost', 'poisonheal', 'pressure'].includes(K);
		const stA = K === 'magicguard' ? pick(burnOrPsn) : K === 'poisonheal' ? pick(['toxic'].filter(has).concat(burnOrPsn)) : strike();
		if (variant === 'faint') {
			duel(false, 8, 100);
		} else duel(aFast, 100, 100);
		const gas = benchA(1);
		steady(gas, 100, 0); ab(gas, 'neutralizinggas');
		if (variant === 'glead') { ab(A, 'neutralizinggas'); ab(gas, 'noability'); }
		else if (variant !== 'faint') ab(A, 'noability');
		if (variant === 'faint') ab(A, 'neutralizinggas');
		if (K === 'poisonheal' && pool.items.includes('toxicorb') && int(100) < 50) item(B, 'toxicorb');
		if (K === 'poisonheal' && !burnOrPsn.length) throw new Error('no status moves');
		const bk = (s) => { steady(s, 100, 0); ab(s, K); };
		if (variant === 'in') {
			bk(benchB(1)); bk(benchB(2)); ab(B, 'noability');
		} else { ab(B, K); bk(benchB(1)); }
		set(A, stA, strike(), strike(), idle());
		set(B, strike(), K === 'pressure' ? stA : idle(), strike(), idle());
		for (const k of [1, 2]) set(benchB(k), strike(), idle(), strike(), idle());
		set(benchA(2), strike(), idle(), strike(), idle());
		if (variant === 'lead') scenario.choices = [full(), duo('move 1', 'move 1'), duo('switch 2', 'move 2'), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('switch 2', 'move 1'), duo('move 1', 'move 2')];
		else if (variant === 'glead') scenario.choices = [full(), duo('move 1', 'move 1'), duo('switch 2', 'move 2'), duo('move 1', 'move 1'), duo('switch 2', 'move 2'), duo('move 1', 'move 1')];
		else if (variant === 'in') scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 1', 'switch 2'), duo('move 2', 'move 1'), duo('move 1', 'switch 3'), duo('move 1', 'move 2'), duo('switch 2', 'move 2')];
		else scenario.choices = [full(), duo('move 1', 'move 1'), duo('switch 2', null), duo('move 1', 'move 2'), duo('move 1', 'move 1')];
		if (variant === 'faint' && !guardK) { for (const k of [1, 2]) bk(benchB(k)); }
	} else if (kind === 'gastwo') {
		duel(aFast, 100, 100);
		ab(A, 'neutralizinggas'); ab(B, 'neutralizinggas');
		const K = pick(['intimidate', 'supremeoverlord', 'dauntlessshield', 'speedboost', 'pressure']);
		for (const s of [benchA(1), benchB(1)]) { steady(s, 100, 0); ab(s, K); }
		ab(benchA(2), 'neutralizinggas');
		if (int(2) === 0) { B.species = A.species; steady(B, 100, 0); steady(A, 100, 0); ab(A, 'neutralizinggas'); ab(B, 'neutralizinggas'); }
		set(A, strike(), idle(), strike(), idle()); set(B, strike(), idle(), strike(), idle());
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('switch 2', 'move 2'), duo('move 1', 'switch 2'), duo('switch 2', 'move 1'), duo('move 2', 'switch 2'), duo('move 1', 'move 1')];
	} else if (kind === 'gasintim') {
		const X = pick(['contrary', 'defiant', 'mirrorarmor', 'guarddog', 'innerfocus', 'clearbody', 'bigpecks', 'unaware', 'justified']);
		const variant = int(3);
		duel(aFast, 100, 100);
		for (const k of [1, 2, 3]) { steady(benchA(k), 100, 0); steady(benchB(k), 100, 0); }
		const holders = [B, benchB(1), benchB(2)];
		if (variant === 0) { // A: gas lead, X on the bench; B's lead holds Intimidate: suppressed at the start, restarted when the gas leaves
			ab(A, 'neutralizinggas'); ab(benchA(1), X); ab(B, 'intimidate'); ab(benchB(1), 'intimidate'); ab(benchB(2), 'noability');
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('switch 2', 'move 1'), duo('move 1', 'switch 2'), duo('switch 2', 'move 1'), duo('move 1', 'switch 2'), duo('move 1', 'move 1')];
		} else if (variant === 1) { // A: X lead (hit by Intimidate at the start), the gas on the bench: in, then out (the restart hits X)
			ab(A, X); ab(benchA(1), 'neutralizinggas'); ab(B, 'intimidate'); ab(benchB(1), 'noability'); ab(benchB(2), 'intimidate');
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('switch 2', 'move 1'), duo('move 1', 'switch 2'), duo('switch 2', 'move 1'), duo('move 1', 'switch 3'), duo('move 1', 'move 1')];
		} else { // Intimidate comes in under A's gas, then the gas leaves
			ab(A, X); ab(benchA(1), 'neutralizinggas'); ab(B, 'noability'); ab(benchB(1), 'intimidate'); ab(benchB(2), 'intimidate');
			scenario.choices = [full(), duo('switch 2', 'move 1'), duo('move 1', 'switch 2'), duo('switch 2', 'move 1'), duo('move 1', 'switch 2'), duo('move 1', 'move 1')];
		}
		for (const s of [A, B, benchA(1), benchA(2), benchB(1), benchB(2)]) set(s, strike(), idle(), strike(), idle());
		void holders;
	} else if (kind === 'bounce') {
		const variant = pick(['pressure', 'pressure', 'prankster', 'prankster', 'libero', 'mirror', 'mirror', 'sync', 'sync', 'breaker', 'goodasgold', 'parting']);
		duel(aFast, 100, 100);
		ab(B, 'magicbounce');
		let x, y;
		if (variant === 'pressure') {
			ab(A, 'pressure'); x = pick(REFL); y = pick(REFL);
			set(A, x, y, strike(), idle()); set(B, x, y, strike(), idle());
		} else if (variant === 'prankster') {
			ab(A, 'prankster');
			const dark = int(2) === 0;
			const cand = real.filter(id => types(id).includes('Dark') === dark);
			A.species = speciesName(pick(cand));
			x = pick(REFL); y = pick(['toxic', 'willowisp', 'thunderwave', 'stealthrock', 'spikes'].filter(has).concat(REFL));
			set(A, x, y, strike(), idle()); set(B, strike(), idle(), strike(), idle());
		} else if (variant === 'libero') {
			ab(A, pick(['libero', 'protean'])); x = pick(REFL); y = pick(REFL);
			set(A, x, y, strike(), idle()); set(B, strike(), idle(), strike(), idle());
		} else if (variant === 'mirror') {
			ab(A, 'mirrorarmor'); x = pick(['growl', 'screech', 'charm', 'leer', 'tailwhip', 'featherdance', 'sandattack'].filter(has).concat(drops('atk')));
			y = pick(['screech', 'growl', 'charm'].filter(has).concat(drops('def')));
			set(A, x, y, strike(), idle()); set(B, strike(), x, strike(), idle());
		} else if (variant === 'sync') {
			ab(A, 'synchronize'); x = pick(['stunspore', 'toxic', 'willowisp', 'thunderwave', 'poisonpowder', 'sleeppowder'].filter(has)); y = pick(burnOrPsn);
			set(A, x, y, strike(), idle()); set(B, strike(), idle(), strike(), idle());
		} else if (variant === 'goodasgold') {
			ab(A, 'goodasgold'); x = pick(REFL); y = pick(REFL);
			set(A, x, y, strike(), idle()); set(B, strike(), x, strike(), idle());
		} else if (variant === 'parting') {
			// Parting Shot into Magic Bounce: the bounced move's drops and its switch run on the original user's side
			ab(A, pick(['noability', 'noability', 'contrary', 'clearbody', 'defiant']));
			x = 'partingshot'; y = pick(REFL);
			set(A, x, y, strike(), idle()); set(B, strike(), y, strike(), idle());
		} else {
			ab(A, 'moldbreaker'); x = pick(REFL); y = pick(REFL);
			set(A, x, y, strike(), idle()); set(B, strike(), x, strike(), idle());
		}
		scenario.choices = variant === 'parting'
			? [full(), duo('move 1', 'move 1'), duo(null, 'switch 2'), duo('move 2', 'move 2'), duo('move 1', 'move 1'), duo('move 2', 'move 1')]
			: [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 3'), duo('move 2', 'move 1')];
	} else if (kind === 'guarddog') {
		// B lowers A's Attack with Charm and Feather Dance (or Parting Shot, each pivot bringing an Intimidate holder in), then switches
		// Intimidate holders in repeatedly; A holds Guard Dog (and at -6 Intimidate has no Attack stage to lower)
		const v = int(2);
		duel(aFast, 100, 100);
		ab(A, 'guarddog'); ab(B, 'noability');
		for (const k of [1, 2, 3]) { steady(benchB(k), 100, 0); ab(benchB(k), 'intimidate'); }
		const low = pick(['charm', 'featherdance', 'charm'].filter(has));
		set(A, strike(), idle(), strike(), idle());
		if (v === 0) {
			set(B, low, 'partingshot', strike(), idle());
			for (const k of [1, 2, 3]) set(benchB(k), low, strike(), idle(), strike());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 1', 'move 1'), duo('move 1', 'switch 2'), duo('move 1', 'switch 3'), duo('move 1', 'switch 2'), duo('move 1', 'switch 3')];
		} else if (v === 1) {
			set(B, 'partingshot', low, strike(), idle());
			for (const k of [1, 2, 3]) set(benchB(k), 'partingshot', low, strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 1', null), duo(null, 'switch 2'), duo('move 1', 'move 1'), duo(null, 'switch 3'), duo('move 1', 'move 1'), duo(null, 'switch 2'), duo('move 1', 'move 1')];
		}
	} else if (kind === 'unaware') {
		// U holds Unaware, H the ability whose stages Unaware ignores; each side attacks and lowers or raises the other
		const H = pick(['intimidate', 'defiant', 'contrary', 'speedboost', 'weakarmor']);
		const uSide = int(2) === 0;
		duel(aFast, 100, 100);
		const [U, Hs] = uSide ? [A, B] : [B, A];
		const benchH = uSide ? benchB(1) : benchA(1);
		ab(U, 'unaware'); ab(Hs, H === 'intimidate' ? 'noability' : H);
		if (H === 'intimidate') { steady(benchH, 100, 0); ab(benchH, 'intimidate'); }
		const phys = contact.length ? contact : attacks;
		const boost = pick(['swordsdance', 'bulkup', 'dragondance'].filter(has));
		set(U, pick(drops('atk')), pick(phys), boost, idle());
		set(Hs, pick(phys), boost, pick(phys), idle());
		const [su, sh] = uSide ? [duo, duo] : [duo, duo]; void su; void sh;
		const pairc = (u, h) => (uSide ? duo(u, h) : duo(h, u));
		if (H === 'intimidate') scenario.choices = [full(), pairc('move 2', 'move 1'), pairc('move 1', 'switch 2'), pairc('move 2', 'move 1'), pairc('move 3', 'move 3'), pairc('move 2', 'move 1')];
		else scenario.choices = [full(), pairc('move 1', 'move 3'), pairc('move 2', 'move 1'), pairc('move 1', 'move 2'), pairc('move 2', 'move 1'), pairc('move 3', 'move 3'), pairc('move 2', 'move 1')];
	} else if (kind === 'guardhit') {
		const R = pick(['roughskin', 'rockyhelmet', 'both']);
		duel(aFast, 100, 100);
		ab(A, 'magicguard'); ab(B, R === 'rockyhelmet' ? 'noability' : 'roughskin');
		if (R !== 'roughskin') item(B, 'rockyhelmet');
		if (int(3) === 0) item(A, 'lifeorb');
		const cmove = () => pick(contact);
		set(A, cmove(), cmove(), pick(burnOrPsn), idle());
		set(B, strike(), pick(burnOrPsn), cmove(), idle());
		if (int(100) < 40) { ab(B, 'magicguard'); ab(A, 'roughskin'); item(A, R === 'roughskin' ? 'rockyhelmet' : 'rockyhelmet'); item(B, ''); }
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 3', 'move 3'), duo('move 1', 'move 1'), duo('move 2', 'move 1')];
	} else if (kind === 'sturdy') {
		const v = int(3);
		duel(aFast, 100, 100);
		const mm = pick(multi);
		if (v === 0) { // Sturdy holder (low level, so a hit would kill it) against multi-hit moves
			steady(A, 100, aFast ? 252 : 0); steady(B, 100, 0);
			ab(A, 'sturdy'); ab(B, 'noability');
			set(A, idle(), strike(), idle(), strike()); set(B, mm, pick(multi), strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 2', 'move 1'), duo('move 2', 'move 1')];
		} else if (v === 1) { // a Sturdy holder with Rocky Helmet hit by a Berserk holder's contact move (the Berserk holder crosses half)
			ab(A, 'sturdy'); ab(B, 'berserk'); item(A, 'rockyhelmet');
			set(A, strike(), idle(), strike(), idle()); set(B, pick(contact), pick(contact), strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 1'), duo('move 3', 'move 1')];
		} else { // a Sturdy user's multi-hit move into Berserk
			ab(A, 'sturdy'); ab(B, 'berserk');
			set(A, mm, pick(multi), strike(), idle()); set(B, strike(), idle(), strike(), idle());
			scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 1'), duo('move 2', 'move 3')];
		}
	} else if (kind === 'flashfire') {
		duel(aFast, 100, 100);
		ab(B, 'flashfire'); ab(A, int(100) < 20 ? 'moldbreaker' : 'noability');
		const fire = pickHit(kindOf(m => m.type === 'Fire' && m.category !== 'Status'), B);
		const prot = pick(['protect', 'spikyshield', 'banefulbunker', 'kingsshield', 'detect'].filter(has));
		set(A, fire, has('willowisp') ? 'willowisp' : fire, strike(), idle());
		set(B, prot, has('substitute') ? 'substitute' : idle(), strike(), prot);
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 1', 'move 2'), duo('move 2', 'move 1'), duo('move 1', 'move 2'), duo('move 1', 'move 3'), duo('move 2', 'move 3')];
	} else if (kind === 'moxie') {
		// B's lead explodes, taking A's fodder with it; the replacements are Moxie and Supreme Overlord holders against level 5 foes
		duel(false, 1, 100);
		const second = int(2) === 0;
		const [mo, so] = second ? [benchA(1), benchA(2)] : [benchA(2), benchA(1)];
		steady(mo, 100, 0); steady(so, 100, 0); ab(mo, 'moxie'); ab(so, 'supremeoverlord');
		ab(A, 'noability');
		set(A, idle(), idle(), idle(), idle());
		set(B, has('explosion') ? 'explosion' : strike(), strike(), strike(), strike());
		for (const k of [1, 2, 3]) { steady(benchB(k), 5, 0); }
		for (const s of [mo, so]) set(s, effective(benchB(1)), strike(), strike(), strike());
		scenario.choices = [full(), duo('move 1', 'move 1'), duo(second ? 'switch 2' : 'switch 3', 'switch 2'), duo('move 1', 'move 1'), duo(null, 'switch 2'), duo('move 1', 'move 1'), duo(null, 'switch 2'), duo('move 1', 'move 1')];
	} else if (kind === 'contrary') {
		duel(aFast, 100, 100);
		ab(A, 'defiant'); ab(B, 'contrary');
		const swap = int(2) === 0;
		if (swap) { ab(A, 'contrary'); ab(B, 'defiant'); }
		steady(benchA(1), 100, 0); steady(benchB(1), 100, 0); ab(benchA(1), 'intimidate'); ab(benchB(1), 'intimidate');
		const selfDrop = ['closecombat', 'superpower', 'overheat', 'dracometeor', 'leafstorm', 'vcreate'].filter(has);
		const lower = drops('atk').concat(drops('spa'), drops('def'));
		set(A, pick(lower), pick(selfDrop), strike(), idle()); set(B, pick(lower), pick(selfDrop), strike(), idle());
		scenario.choices = [full(), duo('move 1', 'move 1'), duo('move 2', 'move 2'), duo('move 1', 'move 2'), duo('switch 2', 'move 1'), duo('move 2', 'switch 2'), duo('move 1', 'move 2')];
	} else if (kind === 'faintend') {
		// Showdown's resolveFaints returns at once when the battle is over, but the phazing loop still drags a Pokemon in (battle.ts 2563,
		// 2866-2873): a Shedinja dragged onto hazards after the last foe fainted ends at 0 HP and not fainted. P1: Breloom (Rocky Helmet)
		// and a level 1 Shedinja; P2: a level 1 Alomomola (a hazard, Close Combat) and a Shedinja that phazes.
		const hazard = pick(['stealthrock', 'stealthrock', 'spikes'].filter(has));
		const forcer = pick(['dragontail', 'circlethrow', 'dragontail'].filter(has));
		const sp = (species, level, moves, itm) => ({ name: '', species, ability: 'No Ability', item: itm, moves, nature: 'Hardy',
			evs: { hp: 0, atk: 0, def: 0, spa: 0, spd: 0, spe: 0 }, ivs: { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 }, level, gender: '' });
		const seedBomb = pick(['seedbomb', 'seedbomb', 'energyball'].filter(has));
		const first = sp('Breloom', int(2) ? 100 : 50, ['endure', seedBomb, 'bulkup'], 'Rocky Helmet');
		scenario.teams[aIdx] = [first, sp('Shedinja', 1, ['endure'], '')];
		scenario.teams[bIdx] = [sp('Alomomola', 1, [hazard, 'closecombat'], ''), sp('Shedinja', 1, [forcer], '')];
		scenario.choices = [teams(2, 2), duo('move 1', 'move 1'), duo('move 2', 'move 1'), duo(null, 'switch 2'), duo('move 3', 'move 1')];
	}
	return scenario;
}

// ---------------------------------------------------------------------------
// Tally, from Showdown's own methods (observation only: every wrapper passes its arguments and result through)

let tally = null;
function newTally() {
	const breaker = {};
	for (const id of BREAKABLE) breaker[id] = 0;
	return {
		moldBreaker: breaker,
		gas: {
			entered: 0, twoGases: 0, restarted: 0, restartedIntimidate: 0, restartedOverlord: 0, endAtFaint: 0, endTie: 0, endOtherGas: 0,
			ignored_intimidate: 0, ignored_dauntlessshield: 0, ignored_supremeoverlord: 0, ignored_magicguard: 0, ignored_multiscale: 0, ignored_speedboost: 0,
			ignored_poisonheal: 0, ignored_pressure: 0,
			restartIntimidateHitsGasHolder: 0, afterGas_intoContrary: 0, afterGas_intoDefiant: 0, afterGas_intoMirrorArmor: 0, afterGas_intoGuardDog: 0,
		},
		bounce: { bounced: 0, byPressure: 0, byPressureKnown: 0, pranksterToDark: 0, pranksterToOther: 0, byLibero: 0, byMirrorArmor: 0, bySynchronize: 0, breakerBypass: 0, byGoodAsGold: 0, partingShot: 0 },
		guardDog: { intimidateAtMinus6: 0, intimidateAtMinus5: 0, intimidateAtOther: 0 },
		unaware: { intimidate: 0, defiant: 0, contrary: 0, speedboost: 0, weakarmor: 0 },
		magicGuard: { roughskin: 0, rockyhelmet: 0, lifeorb: 0 },
		sturdy: { multihit: 0, savedFromFull: 0, berserkCrossed: 0, berserkCrossedByMulti: 0 },
		flashFire: { intoProtect: 0, intoSubstitute: 0, plain: 0 },
		moxie: { boosts: 0, overlordWithFallen: 0, overlordAfterMoxie: 0 },
		contrary: { dropReversed: 0, defiantTriggered: 0, selfDropContrary: 0 },
		faintEnd: { draggedAfterEnd: 0, hp0NotFainted: 0 },
	};
}
const STRUCTURAL_ZERO = [];
const FALLBACK = new Set();

function install() {
	const t = () => tally;
	const suppressingAbility = Battle.prototype.suppressingAbility;
	Battle.prototype.suppressingAbility = function (target) {
		const r = suppressingAbility.call(this, target);
		const c = t();
		if (c && r && this.activeMove && this.activeMove.ignoreAbility && target) {
			const id = target.ability;
			if (c.moldBreaker[id] !== undefined) c.moldBreaker[id]++;
			if (id === 'magicbounce') c.bounce.breakerBypass++;
		}
		return r;
	};
	let inGasEnd = null;
	const gasSeen = new WeakSet();
	const anyGas = (b) => b.getAllActive().some(p => p.ability === 'neutralizinggas' && !p.volatiles.gastroacid);
	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, ...rest) {
		const c = t();
		if (!c) return singleEvent.call(this, eventid, effect, state, target, ...rest);
		if (eventid === 'SwitchIn' && effect.id === 'neutralizinggas') {
			c.gas.entered++; gasSeen.add(this);
			if (this.getAllActive().filter(p => p.ability === 'neutralizinggas').length > 1) c.gas.twoGases++;
		}
		if (eventid === 'End' && effect.id === 'neutralizinggas') {
			const prev = inGasEnd;
			inGasEnd = { restarted: [], tie: false };
			const others = this.getAllActive().filter(p => p !== target && p.hasAbility('neutralizinggas'));
			const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
			if (others.length) c.gas.endOtherGas++;
			else if (state && state.ending) {
				if (inGasEnd.tie) c.gas.endTie++;
				if (target.hp === 0 || target.fainted) c.gas.endAtFaint++;
				for (const id of inGasEnd.restarted) {
					c.gas.restarted++;
					if (id === 'intimidate') c.gas.restartedIntimidate++;
					if (id === 'supremeoverlord') c.gas.restartedOverlord++;
				}
			}
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
	// abilities ignored under the gas (the handlers of a guard, a residual, Pressure)
	const ignoringAbility = Pokemon.prototype.ignoringAbility;
	Pokemon.prototype.ignoringAbility = function () {
		const r = ignoringAbility.call(this);
		const c = t();
		if (c && r && this.isActive && this.ability !== 'neutralizinggas' && anyGas(this.battle)) {
			const key = `ignored_${this.ability}`;
			if (c.gas[key] !== undefined) c.gas[key]++;
		}
		return r;
	};
	const boost = Battle.prototype.boost;
	Battle.prototype.boost = function (boosts, target, source, effect, ...rest) {
		const c = t();
		const eff = effect || this.effect;
		if (c && eff && eff.name === 'Intimidate' && target) {
			const id = target.ability;
			const before = target.boosts.atk;
			// (a restarted Intimidate meets the gas holder that is leaving, which is still the foe's active Pokemon; one that is not restarted
			// meets X only with no gas in play, so these count the Intimidate that hits X in a battle that has had a gas)
			if (inGasEnd && id === 'neutralizinggas') c.gas.restartIntimidateHitsGasHolder++;
			if (gasSeen.has(this) && !inGasEnd) {
				const key = { contrary: 'afterGas_intoContrary', defiant: 'afterGas_intoDefiant', mirrorarmor: 'afterGas_intoMirrorArmor', guarddog: 'afterGas_intoGuardDog' }[id];
				if (key) c.gas[key]++;
			}
			if (id === 'guarddog') {
				if (before === -6) c.guardDog.intimidateAtMinus6++;
				else if (before === -5) c.guardDog.intimidateAtMinus5++;
				else c.guardDog.intimidateAtOther++;
			}
			if (id === 'unaware') c.unaware.intimidate++;
		}
		const r = boost.call(this, boosts, target, source, effect, ...rest);
		if (c && eff && eff.id === 'moxie') c.moxie.boosts++;
		if (c && eff && eff.id === 'defiant') c.contrary.defiantTriggered++;
		return r;
	};
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, relayVar, ...rest) {
		const c = t();
		if (c) {
			const tgt = Array.isArray(target) ? target[0] : target;
			if (eventid === 'ChangeBoost' && tgt && tgt.ability === 'contrary' && relayVar && Object.values(relayVar).some(v => v < 0)) {
				c.contrary.dropReversed++;
				if (!source || source === tgt) c.contrary.selfDropContrary++;
			}
			if (eventid === 'ModifyBoost' && tgt) {
				// Unaware ignoring a stage that one of the abilities set
				if (this.getAllActive().some(p => p.ability === 'unaware' && p !== tgt)) {
					const b = tgt.boosts;
					if (b && (b.atk !== 0 || b.def !== 0 || b.spa !== 0 || b.spd !== 0 || b.spe !== 0)) {
						if (['defiant', 'contrary', 'speedboost', 'weakarmor'].includes(tgt.ability)) c.unaware[tgt.ability]++;
					}
				}
			}
			if (eventid === 'TryBoost' && tgt && tgt.ability === 'guarddog') { /* counted in boost() */ }
			if (eventid === 'TryHit' && tgt && tgt.ability === 'flashfire' && sourceEffect && sourceEffect.type === 'Fire') {
				if (tgt.volatiles.protect || tgt.volatiles.spikyshield || tgt.volatiles.banefulbunker || tgt.volatiles.kingsshield || tgt.volatiles.detect) c.flashFire.intoProtect++;
				else if (tgt.volatiles.substitute) c.flashFire.intoSubstitute++;
				else c.flashFire.plain++;
			}
			if (eventid === 'Damage' && tgt && effect_ok(sourceEffect)) {
				if (tgt.ability === 'magicguard' && !tgt.ignoringAbility()) {
					if (sourceEffect.id === 'roughskin') c.magicGuard.roughskin++;
					if (sourceEffect.id === 'rockyhelmet') c.magicGuard.rockyhelmet++;
					if (sourceEffect.id === 'lifeorb') c.magicGuard.lifeorb++;
				}
				if (tgt.ability === 'sturdy' && this.activeMove && this.activeMove.multihit) c.sturdy.multihit++;
				if (tgt.ability === 'sturdy' && relayVar >= tgt.hp && tgt.hp === tgt.maxhp) c.sturdy.savedFromFull++;
			}
			if (eventid === 'AfterMoveSecondary' && tgt && tgt.ability === 'berserk' && source && source !== tgt && tgt.hp && sourceEffect && sourceEffect.totalDamage) {
				if (tgt.hp <= tgt.maxhp / 2 && tgt.hp + sourceEffect.totalDamage > tgt.maxhp / 2) {
					c.sturdy.berserkCrossed++;
					if (sourceEffect.multihit) c.sturdy.berserkCrossedByMulti++;
				}
			}
		}
		return runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
	};
	const effect_ok = (e) => !!(e && e.id);
	// Magic Bounce: the nested use, by who bounced and the user's ability
	const useMove = BattleActions.prototype.useMove;
	BattleActions.prototype.useMove = function (move, pokemon, options) {
		const c = t();
		if (c && move && move.hasBounced) {
			c.bounce.bounced++;
			const user = options && options.target; // the bounced move's original user is the nested use's target
			const id = user && user.ability;
			const mid = this.dex.moves.get(move.id || move).id;
			if (id === 'pressure') {
				c.bounce.byPressure++;
				if (pokemon.moves.includes(mid)) c.bounce.byPressureKnown++;
			}
			if (id === 'libero' || id === 'protean') c.bounce.byLibero++;
			if (id === 'goodasgold') c.bounce.byGoodAsGold++;
			if (mid === 'partingshot') c.bounce.partingShot++;
			if (id === 'mirrorarmor') c.bounce.byMirrorArmor++;
			if (id === 'synchronize') c.bounce.bySynchronize++;
			if (id === 'prankster') {
				if (user.hasType('Dark')) c.bounce.pranksterToDark++; else c.bounce.pranksterToOther++;
			}
		}
		const id = typeof move === 'string' ? move : move && move.id;
		if (c && pokemon && pokemon.ability === 'supremeoverlord' && id && this.dex.moves.get(id).category !== 'Status' && pokemon.side.totalFainted > 0) {
			c.moxie.overlordWithFallen++;
			if (pokemon.side.pokemon.some(p => p.ability === 'moxie')) c.moxie.overlordAfterMoxie++;
		}
		return useMove.call(this, move, pokemon, options);
	};
	// a Pokemon dragged in after the battle ended, and a Pokemon at 0 HP that is not fainted when it did
	const dragIn = BattleActions.prototype.dragIn;
	BattleActions.prototype.dragIn = function (side, pos) {
		const r = dragIn.call(this, side, pos);
		const c = t();
		if (c && this.battle.ended) {
			c.faintEnd.draggedAfterEnd++;
			if (this.battle.sides.some(s => s.pokemon.some(p => p.hp === 0 && !p.fainted))) c.faintEnd.hp0NotFainted++;
		}
		return r;
	};
	void FALLBACK;
}

function recordScenario(scenario) {
	const before = JSON.stringify(tally);
	try {
		return { trace: record(scenario), scripted: !!scenario.choices };
	} catch (e) {
		if (!scenario.choices) throw e;
		// the script ran into a request it did not expect (a faint, a pivot): keep its longest prefix that Showdown accepts
		for (let n = scenario.choices.length - 1; n >= 1; n--) {
			tally = JSON.parse(before);
			try {
				return { trace: record(Object.assign({}, scenario, { choices: scenario.choices.slice(0, n) })), scripted: true, cut: scenario.choices.length - n };
			} catch (e2) { /* a shorter prefix */ }
		}
		tally = JSON.parse(before);
		const plain = Object.assign({}, scenario);
		delete plain.choices;
		return { trace: record(plain), scripted: false, dropped: e.message };
	}
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice8g.js <outDir> <count> <poolFile> [firstSeed=1]');
		process.exit(2);
	}
	const count = parseInt(countArg, 10), first = parseInt(firstArg || '1', 10);
	const pool = JSON.parse(fs.readFileSync(poolFile, 'utf8'));
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		if (!Array.isArray(pool[k])) throw new Error(`bad pool file ${poolFile}: no ${k}`);
	}
	fs.mkdirSync(outDir, { recursive: true });
	for (const k of ['moves', 'species', 'abilities', 'items']) {
		fs.writeFileSync(path.join(outDir, `slice8g-${k}.json`), JSON.stringify(pool[k]));
	}
	tally = newTally();
	install();
	const kindCount = {}, kindDropped = {}, kindCut = {};
	let cutCount = 0;
	let turns = 0, draws = 0, truncated = 0, written = 0, scripted = 0, dropped = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		const kind = lastKind;
		kindCount[kind] = (kindCount[kind] || 0) + 1;
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scripted++;
		if (result.cut) { cutCount++; kindCut[kind] = (kindCut[kind] || 0) + 1; }
		if (result.dropped) {
			dropped++; kindDropped[kind] = (kindDropped[kind] || 0) + 1;
			if (process.env.SLICE8G_VERBOSE) console.error(`seed ${seed} (${kind}): script dropped: ${result.dropped}`);
		}
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice8g-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice8g seeds, ${written} traces written (${scripted} scripted, ${dropped} scripts dropped), ${truncated} truncated, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${turns} turns, ${draws} draws`);
	console.error(`  kinds: ${JSON.stringify(kindCount)}`);
	console.error(`  ${cutCount} scripts cut short at an unexpected request, by kind: ${JSON.stringify(kindCut)}`);
	console.error(`  dropped by kind: ${JSON.stringify(kindDropped)}`);
	const missing = NEW_ABILITIES.filter(id => !teamAbilities.has(abilityName(id)));
	console.error(missing.length ? `ABILITIES ON NO TEAM: ${missing.join(', ')}` : `all ${NEW_ABILITIES.length} slice 8 abilities are on some team`);
	const zeros = [];
	const walk = (prefix, v) => {
		if (typeof v === 'number') { if (v === 0 && !STRUCTURAL_ZERO.includes(prefix)) zeros.push(prefix); return; }
		for (const [k, x] of Object.entries(v)) walk(prefix ? `${prefix}.${k}` : k, x);
	};
	for (const [k, v] of Object.entries(tally)) { console.error(`  ${k}: ${JSON.stringify(v)}`); walk(k, v); }
	console.error(zeros.length ? `ZERO COUNTERS: ${zeros.join(', ')}` : 'no counter is zero');
}

module.exports = { assign };
