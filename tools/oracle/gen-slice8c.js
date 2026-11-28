#!/usr/bin/env node
'use strict';
// Generates battles for "slice 8c" (immunities and damage guards) and records each on Showdown.
//
// Slice 8c lets in eleven abilities: Good as Gold, Water Absorb, Sap Sipper, Soundproof, Bulletproof (each an `onTryHit` that blocks a hit
// before the type immunity and the accuracy roll), Levitate (no handler: `Pokemon.isGrounded`, which the hazards and the terrains read),
// Sturdy and Magic Guard and Poison Heal (`onDamage`, run by `spreadDamage`), Shell Armor (`onCriticalHit: false`) and Sweet Veil (sleep
// and Yawn, neither of which exists in this engine: it only has to be accepted). The teams are slice 6's (gen-slice6.js `assign`) under the
// kits of slice 7 (gen-slice7f.js), with the 8c abilities sprinkled on the sets. About 30% of the battles are plain random play over those
// teams; the rest are scripted openings (a pair of choices per decision point; the random policy takes over when the script ends, and a
// script that Showdown rejects is dropped and the battle recorded without it):
//   gold      a Good as Gold holder against status moves that target it (Thunder Wave, Toxic, Will-O-Wisp, Taunt, Encore, Trick, Switcheroo,
//             Parting Shot) and against the ones that do not (hazards, Haze, Defog) and Knock Off, which is not a status move
//   absorb    a Water Absorb holder at full HP, then hurt, then hit by Water moves again (healed or immune)
//   levground a Levitate holder against Ground moves
//   levhazard Spikes, Toxic Spikes, Stealth Rock and Sticky Web on the holder's side, then the holder switches in, then out again
//   levterr   a terrain set against a Levitate holder (Grassy Terrain's heal, Terrain Pulse, Rising Voltage, Grassy Glide, Expanding Force)
//   sturdy    a Sturdy holder (level 8, full HP) against a strong attack, with Recover between (it survives at 1 HP again and again when
//             it is the faster), against a multi-hit move (the first hit leaves 1 HP, the second takes it), behind a Substitute, and with Endure
//   mgstatus  a Magic Guard holder given burn, poison and toxic; or holding a Flame Orb, a Toxic Orb or a Black Sludge
//   mghazard  Stealth Rock, Spikes and Toxic Spikes, then a Magic Guard holder switches in
//   mgweather a sandstorm against a Magic Guard holder
//   mgitem    Life Orb, recoil moves and a Rocky Helmet on the foe, with a Magic Guard user (the damage Magic Guard blocks)
//   mgdirect  the damage Magic Guard does not block: a Substitute's cost and Struggle's recoil (Taunt leaves only status moves)
//   pheal     a Poison Heal holder poisoned (Toxic, Toxic Spikes, Toxic Orb) at full HP and hurt, or burned (Will-O-Wisp, Flame Orb: no heal)
//   sap       a Sap Sipper holder against Grass moves until its Attack is at +6 (the last ones are immune with no boost)
//   sound     a Soundproof holder against sound moves (also behind a Substitute: sound moves ignore it)
//   bullet    a Bulletproof holder against bullet moves
//   shell     a Shell Armor holder against high-crit moves, over many turns (the crit roll is made and then cancelled)
//
// Usage:
//   node gen-slice8c.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice8c-<seed>.json, plus slice8c-{moves,species,abilities,items}.json (copies of the pool). A tally of the outcomes that
// distinguish each branch is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers pass
// every argument and result through and call nothing the battle would not call).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { BattleActions } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim', 'battle-actions'));
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

const ABILITIES = ['goodasgold', 'waterabsorb', 'levitate', 'sturdy', 'magicguard', 'poisonheal', 'sapsipper', 'soundproof', 'bulletproof', 'sweetveil', 'shellarmor'];
const IDLE = ['recover', 'swordsdance', 'calmmind', 'roost'];
const HAZARDS = ['stealthrock', 'spikes', 'toxicspikes', 'stickyweb'];
const TERRAINS = ['grassyterrain', 'electricterrain', 'mistyterrain', 'psychicterrain'];
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2246822519 + 668265263);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const mv = (id) => dex.moves.get(id);
	const has = (id) => pool.moves.includes(id);
	for (const id of ABILITIES) if (!pool.abilities.includes(id)) throw new Error(`the pool has no ability ${id}`);
	for (const id of [...HAZARDS, ...TERRAINS, 'toxic', 'willowisp', 'thunderwave', 'taunt', 'encore', 'trick', 'switcheroo', 'partingshot', 'haze', 'defog',
		'knockoff', 'substitute', 'endure', 'sandstorm', 'terrainpulse', 'risingvoltage', 'grassyglide', 'expandingforce', ...IDLE]) {
		if (!has(id)) throw new Error(`the pool has no ${id}`);
	}
	for (const id of ['toxicorb', 'flameorb', 'blacksludge', 'rockyhelmet', 'leftovers', 'lifeorb']) {
		if (!pool.items.includes(id)) throw new Error(`the pool has no item ${id}`);
	}
	let scenario = assign6(scenario0, pool, seed);
	const short = (sc) => sc.teams[0].length !== 6 || sc.teams[1].length !== 6;
	for (let k = 1; short(scenario) && k < 30; k++) {
		const s2 = seed + 1000003 * k;
		scenario = assign6(makeScenario(s2, 'gen9ou', pool), pool, s2);
	}
	delete scenario.choices;
	scenario.tera = true;
	scenario.teraChance = Math.round((0.1 + 0.1 * int(5)) * 10) / 10;
	const real = pool.species.filter(id => id !== 'shedinja');
	const speciesName = (id) => dex.species.get(id).name;
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves.splice(int(4), 1, id);
		s.moves = [...new Set(s.moves)].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const setItem = (s, id) => { s.item = itemName(id); };
	const setAbility = (s, id) => { s.ability = abilityName(id); };

	// move lists of the kits (only the pool's moves)
	const damaging = (id) => mv(id).category !== 'Status';
	const plainHit = (m) => m.category !== 'Status' && !m.selfdestruct && !m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge && !m.selfSwitch &&
		!m.forceSwitch && m.priority === 0 && !m.self;
	const ofType = (type, pred) => pool.moves.filter(id => mv(id).type === type && pred(mv(id)));
	const waterMoves = ofType('Water', m => plainHit(m));
	const grassMoves = ofType('Grass', m => plainHit(m) && (m.accuracy === true || m.accuracy >= 90));
	const groundMoves = pool.moves.filter(id => { const m = mv(id); return m.type === 'Ground' && plainHit(m) && !m.multihit; });
	const soundMoves = pool.moves.filter(id => mv(id).flags.sound);
	const bulletMoves = pool.moves.filter(id => mv(id).flags.bullet && plainHit(mv(id)));
	const critMoves = pool.moves.filter(id => { const m = mv(id); return plainHit(m) && m.critRatio >= 2 && !m.multihit && m.basePower >= 40 && m.basePower <= 100; });
	const status = pool.moves.filter(id => mv(id).category === 'Status' && mv(id).target === 'normal');
	const attacks = pool.moves.filter(id => {
		const m = mv(id);
		return plainHit(m) && m.basePower >= 40 && m.basePower <= 65 && (m.accuracy === true || m.accuracy === 100) && !m.recoil && !m.drain && !m.multihit && m.flags.protect;
	});
	const strong = pool.moves.filter(id => { const m = mv(id); return plainHit(m) && m.basePower >= 80 && m.accuracy === 100 && !m.multihit && !m.selfBoost && !m.recoil && !m.drain; });
	const multi = pool.moves.filter(id => { const m = mv(id); return plainHit(m) && m.multihit && (m.accuracy === true || m.accuracy >= 90) && m.basePower >= 20; });
	const recoilMoves = pool.moves.filter(id => { const m = mv(id); return plainHit(m) && m.recoil && m.basePower >= 80 && m.accuracy >= 90; });
	const contactAttacks = attacks.filter(id => mv(id).flags.contact);
	for (const [n, xs] of [['water', waterMoves], ['grass', grassMoves], ['ground', groundMoves], ['sound', soundMoves], ['bullet', bulletMoves], ['crit', critMoves],
		['status', status], ['attacks', attacks], ['strong', strong], ['multi', multi], ['recoil', recoilMoves], ['contact', contactAttacks]]) {
		if (!xs.length) throw new Error(`the pool has no ${n} move`);
	}

	// the sprinkle: the 8c abilities on many sets, so that unscripted play meets them (with the kit moves that wake them)
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 34) setAbility(s, pick(ABILITIES));
		if (int(100) < 10) put(s, pick(waterMoves));
		if (int(100) < 8) put(s, pick(grassMoves));
		if (int(100) < 8) put(s, pick(groundMoves));
		if (int(100) < 8) put(s, pick(soundMoves));
		if (int(100) < 8) put(s, pick(bulletMoves));
		if (int(100) < 6) put(s, pick(critMoves));
		if (int(100) < 10) put(s, pick(['toxic', 'willowisp', 'thunderwave', 'taunt', 'encore', 'trick']));
		if (int(100) < 6) put(s, pick(HAZARDS));
		if (int(100) < 6) put(s, 'sandstorm');
		if (int(100) < 10) setItem(s, pick(['toxicorb', 'flameorb', 'blacksludge', 'rockyhelmet', 'lifeorb', 'leftovers']));
	}
	const kinds = [
		['random', 30], ['gold', 7], ['absorb', 4], ['levground', 3], ['levhazard', 4], ['levterr', 4], ['sturdy', 10], ['mgstatus', 5], ['mghazard', 4],
		['mgweather', 2], ['mgitem', 5], ['mgdirect', 3], ['pheal', 6], ['sap', 3], ['sound', 4], ['bullet', 2], ['shell', 4],
	];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	scenario.kind = kind;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const A2 = scenario.teams[aIdx][1];
	const duoC = (a, b) => { const pair = [null, null]; pair[aIdx] = a; pair[bIdx] = b; return pair; };
	const full = [`team ${[1, 2, 3, 4, 5, 6].join('')}`, `team ${[1, 2, 3, 4, 5, 6].join('')}`];
	const types = (s) => dex.species.get(s.species).types;
	const canHit = (id, s) => dex.getImmunity(mv(id).type, types(s));
	const pickFor = (ids, s) => { const ok = ids.filter(id => canHit(id, s)); return pick(ok.length ? ok : ids); };
	// a bulky holder at level 100 with no item and the given ability; its foe the same with a plain set
	const steady = (s, ability, spe) => {
		s.level = 100; s.ability = abilityName(ability); s.nature = 'Hardy'; s.item = '';
		s.evs = { hp: 252, atk: 0, def: 4, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
	};
	// A and B of these levels (A with 252 Speed EVs when it is to be the faster, B with none; the other way round otherwise)
	const speeds = (aFaster, la = 100, lb = 100) => {
		for (let k = 0; k < 400; k++) {
			const a = pick(real);
			const sa = speStat(a, la, aFaster ? 252 : 0);
			const bs = real.filter(b => (aFaster ? sa > speStat(b, lb, 0) + 10 : speStat(b, lb, 0) > sa + 10));
			if (bs.length) { A.species = speciesName(a); B.species = speciesName(pick(bs)); return; }
		}
		throw new Error('no speed pair');
	};
	// the moves in these slots, a repeated id replaced by an idle move that is not used (so that the slots the script names stay put)
	const exact = (s, ...ids) => {
		const out = [];
		for (const id of ids.slice(0, 4)) out.push(out.includes(id) ? IDLE.find(i => !out.includes(i) && !ids.includes(i)) : id);
		for (const i of IDLE) if (out.length < 4 && !out.includes(i)) out.push(i);
		s.moves = out;
	};
	const tm = (m) => (int(100) < 15 ? `${m} terastallize` : m);
	const aFaster = int(2) === 0;
	const turns = (n, a, b) => Array.from({ length: n }, () => duoC(a, b));
	const cycle = (a, bs) => bs.map(b => duoC(a, b));
	// a species the holder may be (not Flying, Ground or immune to the kit's types), so that the kit's hits are not natural immunities
	const plainSpecies = (s, banned) => {
		for (let k = 0; k < 200; k++) {
			const id = pick(real);
			const ts = dex.species.get(id).types;
			if (!ts.some(t => banned.includes(t))) { s.species = speciesName(id); return; }
		}
	};

	// A's idle slots: 1 Recover and 2 Roost (a heal), 3 Swords Dance and 4 Calm Mind (nothing happens). Recover and Roost have 8 PP each,
	// so a script that heals every turn alternates the two.
	const idleA = () => exact(A, 'recover', 'roost', 'swordsdance', 'calmmind');
	const heal = (i) => (i % 2 === 0 ? 'move 1' : 'move 2');
	const distinct = (list, n, ...avoid) => {
		const out = [];
		for (let k = 0; k < 60 && out.length < n; k++) { const x = pick(list); if (!out.includes(x) && !avoid.includes(x)) out.push(x); }
		return out;
	};

	if (kind === 'gold') {
		// B throws status moves at A; every one that targets A is blocked before the accuracy roll; the side and field moves are not
		speeds(aFaster); steady(A, 'goodasgold', aFaster ? 252 : 0); steady(B, 'noability', 0);
		const blockable = ['thunderwave', 'toxic', 'willowisp', 'taunt', 'encore', 'trick', 'switcheroo', 'partingshot'];
		const v = int(3);
		if (v === 0) exact(B, ...distinct(blockable, 4));
		else if (v === 1) exact(B, pick(blockable), 'haze', pick(blockable), 'defog');
		else exact(B, pick(HAZARDS), 'knockoff', pick(blockable), pick(HAZARDS));
		idleA();
		scenario.choices = [full, ...[0, 1, 2, 3, 0, 2].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
		if (int(3) === 0) setItem(B, pick(['leftovers', 'rockyhelmet', 'lifeorb']));
	} else if (kind === 'absorb') {
		// full HP (immune, nothing to heal), then hurt by a plain attack, then Water moves again (healed)
		speeds(aFaster); steady(A, 'waterabsorb', aFaster ? 252 : 0); steady(B, 'noability', 0);
		const [w1, w2, w3] = distinct(waterMoves, 3);
		exact(B, w1, pickFor(attacks, A), w2 || w1, w3 || w1);
		idleA();
		scenario.choices = [full, ...[0, 1, 2, 0, 3, 2].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
	} else if (kind === 'levground') {
		speeds(aFaster); steady(A, 'levitate', aFaster ? 252 : 0); steady(B, 'noability', 0);
		plainSpecies(A, ['Flying']);
		exact(B, ...distinct(groundMoves, 3), pickFor(attacks, A));
		idleA();
		scenario.choices = [full, ...[0, 1, 2, 0, 1, 3].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
	} else if (kind === 'levhazard') {
		// B sets hazards on A's side (A idles), then A switches to the Levitate holder (slot 2), then back to the lead, then out again.
		// (Showdown numbers the switch targets by the current party order, so slot 2 is always the other one.)
		speeds(false); steady(A, 'noability', 0); steady(B, 'noability', 252);
		steady(A2, 'levitate', 0);
		plainSpecies(A2, ['Flying']);
		exact(B, ...[...HAZARDS].sort(() => rnd() - 0.5));
		idleA();
		exact(A2, 'recover', 'roost', 'swordsdance', 'calmmind');
		scenario.choices = [full, duoC('move 3', 'move 1'), duoC('move 3', 'move 2'), duoC('move 3', 'move 3'), duoC('move 3', 'move 4'), duoC('switch 2', 'move 1'), duoC('move 3', 'move 1'), duoC('switch 2', 'move 1'), duoC('switch 2', 'move 1')];
	} else if (kind === 'levterr') {
		speeds(aFaster); steady(A, 'levitate', aFaster ? 252 : 0); steady(B, 'noability', 0);
		plainSpecies(A, ['Flying']);
		const users = ['terrainpulse', 'risingvoltage', 'grassyglide', 'expandingforce'];
		exact(B, pick(TERRAINS), ...distinct(users, 2), pickFor(attacks, A));
		idleA();
		// the terrain goes up, then its users and an attack hit A (the Grassy Terrain heal shows when A is hurt)
		scenario.choices = [full, ...[0, 3, 1, 2, 3, 1].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
	} else if (kind === 'sturdy') {
		const v = int(5);
		const fast = v === 2 || int(10) < 6;
		speeds(fast, 20, 40); steady(A, 'sturdy', fast ? 252 : 0); steady(B, 'noability', 0);
		A.level = 20; A.evs.hp = 0; B.level = 40;
		B.evs.atk = 252; B.evs.spa = 252;
		idleA();
		if (v === 0) {
			// a strong hit from full HP; the faster holder recovers to full between hits and survives it again
			exact(B, pickFor(strong, A), pickFor(strong, A), pickFor(attacks, A), 'recover');
			scenario.choices = fast ? [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 1'), duoC('move 1', 'move 2')] : [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 2')];
		} else if (v === 1) {
			// a multi-hit move: the first hit leaves 1 HP, the next takes it
			exact(B, ...distinct(multi.filter(id => canHit(id, A)).length ? multi.filter(id => canHit(id, A)) : multi, 2), pickFor(attacks, A), 'recover');
			scenario.choices = [full, duoC('move 1', 'move 1')];
		} else if (v === 2) {
			// a Substitute first: A is not at full HP any more, and the hit lands on the Substitute
			exact(B, pickFor(strong, A), pickFor(strong, A), pickFor(attacks, A), 'recover');
			A.evs.hp = 252;
			exact(A, 'substitute', 'recover', 'roost', 'swordsdance');
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 1')];
		} else if (v === 3) {
			// Endure: it runs before Sturdy (priority -10 and -30), and leaves 1 HP, so Sturdy has nothing to do
			exact(B, pickFor(strong, A), pickFor(strong, A), pickFor(attacks, A), 'recover');
			exact(A, 'endure', 'recover', 'roost', 'swordsdance');
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 1')];
		} else {
			// a recoil move, or a Life Orb, a Rocky Helmet: the damage that follows the saved hit
			exact(B, pickFor(recoilMoves, A), pickFor(strong, A), pickFor(attacks, A), 'recover');
			setItem(B, pick(['lifeorb', 'rockyhelmet', 'leftovers', 'blacksludge']));
			scenario.choices = [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 2')];
		}
	} else if (kind === 'mgstatus') {
		speeds(aFaster); steady(A, 'magicguard', aFaster ? 252 : 0); steady(B, 'noability', 0);
		const v = int(3);
		idleA();
		if (v === 0) {
			// status from B, in turn
			exact(B, 'toxic', 'willowisp', pickFor(attacks, A), 'thunderwave');
			const first = int(2);
			scenario.choices = [full, ...[first, 2, 2, 2, 3].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
		} else if (v === 1) {
			// an orb of its own, a Black Sludge (a non-Poison holder takes damage from it), beside a foe that only idles
			setItem(A, pick(['toxicorb', 'flameorb', 'blacksludge']));
			exact(B, 'recover', 'roost', 'swordsdance', 'calmmind');
			scenario.choices = [full, ...turns(5, 'move 3', 'move 3')];
		} else {
			// hurt first, then given the status (the damage that Magic Guard blocks happens at the end of every turn)
			exact(B, pickFor(attacks, A), pick(['toxic', 'willowisp']), pickFor(attacks, A), 'recover');
			scenario.choices = [full, ...[0, 1, 2, 2, 2].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
		}
	} else if (kind === 'mghazard') {
		// the hazards go up while A (lead) idles, then the Magic Guard holder switches in, out and in again
		speeds(false); steady(A, 'noability', 0); steady(B, 'noability', 252);
		steady(A2, 'magicguard', 0);
		plainSpecies(A2, ['Flying']);
		exact(B, 'stealthrock', 'spikes', 'toxicspikes', pick(['spikes', 'sandstorm']));
		idleA();
		exact(A2, 'recover', 'roost', 'swordsdance', 'calmmind');
		scenario.choices = [full, duoC('move 3', 'move 1'), duoC('move 3', 'move 2'), duoC('move 3', 'move 3'), duoC('move 3', 'move 4'), duoC('switch 2', 'move 1'), duoC('move 3', 'move 1'), duoC('switch 2', 'move 1'), duoC('switch 2', 'move 1')];
	} else if (kind === 'mgweather') {
		speeds(aFaster); steady(A, 'magicguard', aFaster ? 252 : 0); steady(B, 'noability', 0);
		exact(B, 'sandstorm', pickFor(attacks, A), 'recover', 'swordsdance');
		idleA();
		scenario.choices = [full, ...[0, 1, 2, 2, 2].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
	} else if (kind === 'mgitem') {
		const v = int(3);
		speeds(aFaster); steady(A, 'magicguard', aFaster ? 252 : 0); steady(B, 'noability', 0);
		exact(B, 'recover', 'roost', 'swordsdance', 'calmmind');
		if (v === 0) {
			// Life Orb on the Magic Guard user
			setItem(A, 'lifeorb');
			exact(A, pickFor(attacks, B), pickFor(attacks, B), 'recover', 'roost');
		} else if (v === 1) {
			// a Rocky Helmet on the foe against a contact move
			setItem(B, 'rockyhelmet');
			exact(A, pickFor(contactAttacks, B), pickFor(contactAttacks, B), 'recover', 'roost');
		} else {
			// recoil
			exact(A, pickFor(recoilMoves, B), pickFor(recoilMoves, B), 'recover', 'roost');
		}
		scenario.choices = [full, ...[0, 1, 0, 2, 0, 3].map(k => duoC(`move ${k + 1}`, 'move 3'))];
	} else if (kind === 'mgdirect') {
		// the holder (the faster) makes a Substitute (a cost Magic Guard does not block), then the foe taunts it: its moves are all
		// status, so it uses Struggle, whose recoil is directDamage (not blocked either)
		speeds(true); steady(A, 'magicguard', 252); steady(B, 'noability', 0);
		exact(A, 'substitute', 'recover', 'toxic', 'swordsdance');
		exact(B, 'taunt', pickFor(attacks, A), 'recover', 'swordsdance');
		scenario.choices = int(2) === 0 ? [full, duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2')]
			: [full, duoC('move 1', 'move 1'), duoC('move 2', 'move 3'), duoC('move 1', 'move 1'), duoC('move 1', 'move 2'), duoC('move 1', 'move 2')];
	} else if (kind === 'pheal') {
		const v = int(4);
		speeds(aFaster); steady(A, 'poisonheal', aFaster ? 252 : 0); steady(B, 'noability', 0);
		idleA();
		if (v === 0) {
			// Toxic, then a hit: toxic at full HP (no heal), then hurt (heals an eighth each turn)
			exact(B, 'toxic', pickFor(attacks, A), pickFor(attacks, A), 'recover');
			scenario.choices = [full, ...[0, 1, 2, 3, 3].map(k => duoC('move 3', `move ${k + 1}`))];
		} else if (v === 1) {
			// a Toxic Orb or a Flame Orb of its own (a burn damages: its effect id is brn)
			setItem(A, pick(['toxicorb', 'toxicorb', 'flameorb']));
			exact(B, pickFor(attacks, A), pickFor(attacks, A), 'recover', 'swordsdance');
			scenario.choices = [full, ...[0, 1, 2, 2, 2].map(k => duoC('move 3', `move ${k + 1}`))];
		} else if (v === 2) {
			// Will-O-Wisp: a burn takes its damage
			exact(B, 'willowisp', pickFor(attacks, A), 'recover', 'swordsdance');
			scenario.choices = [full, ...[0, 1, 1, 2].map(k => duoC('move 3', `move ${k + 1}`))];
		} else {
			// Toxic Spikes on A's side while the lead idles, the holder switches in (poisoned), and B hits it
			steady(A2, 'poisonheal', 0); steady(A, 'noability', 0);
			plainSpecies(A2, ['Flying', 'Poison', 'Steel']);
			exact(B, 'toxicspikes', pickFor(attacks, A2), pickFor(attacks, A2), 'toxic');
			exact(A2, 'recover', 'roost', 'swordsdance', 'calmmind');
			idleA();
			scenario.choices = [full, duoC('move 3', 'move 1'), duoC('switch 2', 'move 2'), duoC('move 3', 'move 2'), duoC('move 3', 'move 3'), duoC('move 3', 'move 3')];
		}
	} else if (kind === 'sap') {
		speeds(aFaster); steady(A, 'sapsipper', aFaster ? 252 : 0); steady(B, 'noability', 0);
		exact(B, ...distinct(grassMoves, 2), pickFor(attacks, A), pick(grassMoves));
		idleA();
		scenario.choices = [full, ...[0, 1, 0, 3, 0, 1, 3, 0].map((k, i) => duoC(i % 2 === 0 ? 'move 1' : 'move 2', `move ${k + 1}`))];
	} else if (kind === 'sound') {
		const sub = int(2) === 0;
		speeds(sub || aFaster); steady(A, 'soundproof', (sub || aFaster) ? 252 : 0); steady(B, 'noability', 0);
		const ss = distinct(soundMoves, 4);
		exact(B, ...ss);
		if (sub) { exact(A, 'substitute', 'recover', 'roost', 'swordsdance'); scenario.choices = [full, duoC('move 1', 'move 1'), ...ss.map((_, i) => duoC('move 2', `move ${i + 1}`))]; }
		else { idleA(); scenario.choices = [full, ...ss.map((_, i) => duoC(heal(i), `move ${i + 1}`))]; }
	} else if (kind === 'bullet') {
		speeds(aFaster); steady(A, 'bulletproof', aFaster ? 252 : 0); steady(B, 'noability', 0);
		const bs = [...distinct(bulletMoves, 3), pickFor(attacks, A)];
		exact(B, ...bs);
		idleA();
		scenario.choices = [full, ...[0, 1, 2, 3, 0, 1].map((k, i) => duoC(heal(i), `move ${k + 1}`))];
	} else if (kind === 'shell') {
		// a bulky holder that recovers against high-crit attackers, over many turns
		speeds(aFaster); steady(A, 'shellarmor', aFaster ? 252 : 0); steady(B, 'noability', 0);
		B.evs.atk = 252; B.evs.spa = 252;
		const weak = critMoves.filter(id => mv(id).basePower <= 85);
		const cs = distinct(weak.length ? weak : critMoves, 2).map(id => id);
		exact(B, ...cs.map(id => pickFor([id], A)));
		idleA();
		scenario.choices = [full, ...Array.from({ length: 12 }, (_, i) => duoC(heal(i), `move ${1 + (i % cs.length)}`))];
	}
	if (scenario.choices && int(100) < 12) scenario.choices = scenario.choices.map((p, i) => (i === 1 ? p.map(c => (c && c.startsWith('move') ? tm(c) : c)) : p));
	return scenario;
}

// ---------------------------------------------------------------------------
// The tally. Every wrapper passes its arguments and result through.

function newTally() {
	return {
		gold: { blockedStatus: 0, hazardThrough: 0, fieldThrough: 0, knockOffThrough: 0, blockedTrick: 0, blockedTaunt: 0 },
		absorb: { immuneAtFull: 0, healed: 0 },
		levitate: { groundMoveImmune: 0, spikes: 0, toxicspikes: 0, stickyweb: 0, stealthrockGrounded: 0, terrain: 0 },
		sturdy: { saved: 0, savedMulti: 0, savedAgain: 0, hitSubstitute: 0, endure: 0, notFull: 0 },
		magicguard: { brn: 0, psn: 0, tox: 0, lifeorb: 0, rockyhelmet: 0, blacksludge: 0, recoil: 0, stealthrock: 0, spikes: 0, sandstorm: 0, struggleRecoil: 0, substituteCost: 0, moveDamage: 0 },
		poisonheal: { toxicHealed: 0, psnHealed: 0, atFullHp: 0, burnDamage: 0 },
		sapsipper: { boosted: 0, immuneCapped: 0 },
		soundproof: { blocked: 0, behindSubstitute: 0 },
		bulletproof: { blocked: 0 },
		shellarmor: { cancelled: 0, otherCrits: 0 },
	};
}
let activeTally = null;
const t = () => activeTally;
const holds = (p, id) => p && p.ability === id;

function install() {
	const add = Battle.prototype.add;
	Battle.prototype.add = function (...args) {
		const c = t();
		if (c && args[0] === '-immune' && typeof args[2] === 'string') {
			const tgt = args[1];
			const mvId = this.activeMove && this.activeMove.id;
			if (args[2] === '[from] ability: Good as Gold') {
				c.gold.blockedStatus++;
				if (mvId === 'trick' || mvId === 'switcheroo') c.gold.blockedTrick++;
				if (mvId === 'taunt') c.gold.blockedTaunt++;
			}
			if (args[2] === '[from] ability: Water Absorb' && tgt && tgt.hp === tgt.maxhp) c.absorb.immuneAtFull++;
			if (args[2] === '[from] ability: Levitate') c.levitate.groundMoveImmune++;
			if (args[2] === '[from] ability: Sap Sipper') c.sapsipper.immuneCapped++;
			if (args[2] === '[from] ability: Soundproof') {
				c.soundproof.blocked++;
				if (tgt && tgt.volatiles && tgt.volatiles.substitute) c.soundproof.behindSubstitute++;
			}
			if (args[2] === '[from] ability: Bulletproof') c.bulletproof.blocked++;
		}
		if (c && args[0] === '-ability' && args[2] === 'Sturdy') {
			const mv = this.activeMove;
			c.sturdy.saved++;
			if (mv && mv.multihit) c.sturdy.savedMulti++;
			if (args[1] && args[1].sturdyBefore) c.sturdy.savedAgain++;
			if (args[1]) args[1].sturdyBefore = true;
		}
		return add.call(this, ...args);
	};
	const heal = Battle.prototype.heal;
	Battle.prototype.heal = function (damage, target, source, effect) {
		const c = t();
		const r = heal.call(this, damage, target, source, effect);
		const eff = this.effect;
		if (c && eff && eff.id === 'waterabsorb' && r) c.absorb.healed++;
		if (c && eff && eff.id === 'poisonheal') {
			const tgt = target || (this.event && this.event.target);
			if (!r) c.poisonheal.atFullHp++;
			else if (tgt && tgt.status === 'tox') c.poisonheal.toxicHealed++;
			else if (tgt) c.poisonheal.psnHealed++;
		}
		return r;
	};
	const boost = Battle.prototype.boost;
	Battle.prototype.boost = function (...args) {
		const c = t();
		const r = boost.apply(this, args);
		if (c && this.effect && this.effect.id === 'sapsipper' && r) c.sapsipper.boosted++;
		return r;
	};
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, relayVar, ...rest) {
		const c = t();
		if (c && eventid === 'Damage' && target && target.ability) {
			const eff = sourceEffect;
			if (target.ability === 'magicguard' && eff) {
				const k = c.magicguard;
				if (eff.effectType === 'Move') k.moveDamage++;
				else if (eff.id === 'brn') k.brn++;
				else if (eff.id === 'psn') k.psn++;
				else if (eff.id === 'tox') k.tox++;
				else if (eff.id === 'lifeorb') k.lifeorb++;
				else if (eff.id === 'rockyhelmet') k.rockyhelmet++;
				else if (eff.id === 'blacksludge') k.blacksludge++;
				else if (eff.id === 'recoil') k.recoil++;
				else if (eff.id === 'stealthrock') k.stealthrock++;
				else if (eff.id === 'spikes') k.spikes++;
				else if (eff.id === 'sandstorm') k.sandstorm++;
			}
			if (target.ability === 'poisonheal' && eff && eff.id === 'brn') c.poisonheal.burnDamage++;
			if (target.ability === 'sturdy' && eff && eff.effectType === 'Move') {
				if (target.hp !== target.maxhp) c.sturdy.notFull++;
				if (target.volatiles.endure) c.sturdy.endure++;
			}
		}
		if (c && eventid === 'CriticalHit' && target) {
			const r = runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
			if (target.ability === 'shellarmor' && !r) c.shellarmor.cancelled++;
			if (target.ability !== 'shellarmor' && r) c.shellarmor.otherCrits++;
			return r;
		}
		return runEvent.call(this, eventid, target, source, sourceEffect, relayVar, ...rest);
	};
	const directDamage = Battle.prototype.directDamage;
	Battle.prototype.directDamage = function (damage, target, source, effect) {
		const c = t();
		const eff = effect || this.effect;
		const tgt = target || (this.event && this.event.target);
		if (c && tgt && tgt.ability === 'magicguard' && eff) {
			if (eff.id === 'strugglerecoil') c.magicguard.struggleRecoil++;
			if (eff.id === 'substitute') c.magicguard.substituteCost++;
		}
		return directDamage.call(this, damage, target, source, effect);
	};
	const isGrounded = Pokemon.prototype.isGrounded;
	Pokemon.prototype.isGrounded = function (...args) {
		const r = isGrounded.apply(this, args);
		const c = t();
		if (c && this.ability === 'levitate' && r === null) {
			const id = this.battle.effect && this.battle.effect.id;
			if (id === 'spikes') c.levitate.spikes++;
			else if (id === 'toxicspikes') c.levitate.toxicspikes++;
			else if (id === 'stickyweb') c.levitate.stickyweb++;
			else if (TERRAINS.includes(id)) c.levitate.terrain++;
			else if (this.battle.activeMove && ['terrainpulse', 'risingvoltage', 'grassyglide', 'expandingforce'].includes(this.battle.activeMove.id)) c.levitate.terrain++;
		}
		return r;
	};
	const runMove = BattleActions.prototype.runMove;
	BattleActions.prototype.runMove = function (moveOrMoveName, pokemon, targetLoc, options) {
		const c = t();
		const id = typeof moveOrMoveName === 'string' ? moveOrMoveName : moveOrMoveName && moveOrMoveName.id;
		const foe = pokemon && pokemon.side.foe.active[0];
		if (c && foe && foe.ability === 'goodasgold' && id) {
			const m = dex.moves.get(id);
			if (m.target === 'foeSide' && HAZARDS.includes(id)) c.gold.hazardThrough++;
			if (id === 'haze' || id === 'defog') c.gold.fieldThrough++;
			if (id === 'knockoff') c.gold.knockOffThrough++;
		}
		if (c && foe && foe.ability === 'sturdy' && foe.volatiles.substitute && id && dex.moves.get(id).category !== 'Status') c.sturdy.hitSubstitute++;
		return runMove.call(this, moveOrMoveName, pokemon, targetLoc, options);
	};
	// a Pokemon that comes in on Stealth Rock grounded (the Levitate holder takes no hazard: only Stealth Rock counts as a control)
	const damage = Battle.prototype.damage;
	Battle.prototype.damage = function (dmg, target, source, effect, ...rest) {
		const c = t();
		const eff = effect || this.effect;
		const tgt = target || (this.event && this.event.target);
		if (c && tgt && tgt.ability === 'levitate' && eff && eff.id === 'stealthrock') c.levitate.stealthrockGrounded++;
		return damage.call(this, dmg, target, source, effect, ...rest);
	};
}

// counters that cannot be hit by construction or are rare by nature
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
		console.error('usage: node gen-slice8c.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice8c-${k}.json`), JSON.stringify(pool[k]));
	}
	activeTally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scriptedCount = 0, dropped = 0;
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		let result;
		try { result = recordScenario(scenario); } catch (e) { console.error(`seed ${seed}: ${e.message}`, JSON.stringify(scenario.choices)); throw e; }
		const trace = result.trace;
		if (result.scripted) scriptedCount++;
		if (result.dropped) { dropped++; if (process.env.SLICE8C_VERBOSE) console.error(`seed ${seed}: script dropped (${scenario.kind}, turn ${(scenario.choices || []).length}): ${result.dropped}`); }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice8c-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice8c seeds, ${written} traces written (${scriptedCount} scripted, ${dropped} scripts dropped), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
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
