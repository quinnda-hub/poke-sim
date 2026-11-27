#!/usr/bin/env node
'use strict';
// Generates battles for "slice 8d" (the on-hit and after-move abilities) and records each on Showdown.
//
// Slice 8d lets in eleven abilities:
//   Flame Body, Static      onDamagingHit: a contact hit and `randomChance(3, 10)` (the draw is made for every contact hit, whatever the
//                           attacker's state) -> the attacker is burned or paralysed
//   Rough Skin              onDamagingHit (order 1): a contact hit costs the attacker a eighth of its HP
//   Toxic Chain             onSourceDamagingHit: any damaging hit, `randomChance(3, 10)` -> the target is badly poisoned, except through
//                           Shield Dust or Covert Cloak (no draw)
//   Bad Dreams              onResidual against sleeping foes (sleep is not modelled: the handler is in the Residual sort and nothing else)
//   Synchronize             onAfterSetStatus: the status goes back to its source (not Toxic Spikes, not sleep or freeze, not self-inflicted)
//   Magician                onAfterMoveSecondarySelf: a user with an empty hand takes the item of the Pokemon it hit
//   Pickpocket              onAfterMoveSecondary: a holder with an empty hand takes the item of a contact attacker
//   Berserk                 onDamage / onAfterMoveSecondary: a hit (the whole of a multi-hit move) that takes the holder across half its HP
//                           raises its Special Attack
//   Moxie                   onSourceAfterFaint: a Pokemon that a move knocked out raises the Attack of the user, once the faint is resolved
//   Shield Dust             onModifySecondaries: the secondaries without a `self` effect are dropped (and so are their draws)
//
// The teams are slice 6's (gen-slice6.js `assign`) with an ability from this slice on about half of the sets. About 30% of the battles are
// plain random play over those teams; the rest are scripted openings. A script here is a program, not a list: at every decision point it
// reads the battle (through the Showdown Battle that `record` plays) and gives each side's choice from the state, then the random policy
// takes over. A forced switch after a knockout is answered by the script too (the first Pokemon that can come in), so a script that ends in
// knockouts stays a script. The kinds:
//   contactbody   Flame Body or Static against contact and non-contact moves, multi-hit contact moves, an attacker that is already
//                 statused, a Fire or Electric attacker (type immunity), a Rocky Helmet on the holder (the order of the two handlers),
//                 a Shedinja attacker that the helmet faints, and a Synchronize attacker (the status goes back)
//   roughskin     Rough Skin against contact, non-contact and multi-hit contact moves, with a Rocky Helmet, with a Shedinja attacker
//   toxicchain    Toxic Chain against Shield Dust, a Poison or Steel type, a Synchronize holder, an already statused target, multi-hit moves
//   sync          Synchronize against a status move, a secondary status, Toxic Spikes on switch-in, Flame Orb or Toxic Orb, a type-immune
//                 source and an already statused one
//   magician      Magician with an empty and with a full hand, against a target with and without an item, with Knock Off, Thief, Trick
//                 and a Choice item, a multi-hit move, and a target that a Shedinja user cannot outlive
//   pickpocket    Pickpocket with an empty and with a full hand, against contact and non-contact moves, Knock Off, a pivot and a Thief
//   berserk       Berserk with a recovery move (the hit crosses half HP again and again), a multi-hit move, and a poisoned or Leftovers holder
//                 whose HP crosses half at the residual
//   moxie         Moxie on a knockout (a chain of them), a double knockout (a Rocky Helmet that faints a Shedinja user), a residual knockout
//                 (Toxic on a Shedinja: it does not count), recoil after the knockout, and a hazard knockout
//   dust          Shield Dust against secondary chances: status, flinch and drops on the target, and a move whose secondary is on the user
//   mirror        two Pokemon of one species and speed, one with Bad Dreams: Residual handlers that tie
//
// Usage:
//   node gen-slice8d.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice8d-<seed>.json, plus slice8d-{moves,species,abilities,items}.json (copies of the pool). A tally of the branches
// of each ability is printed to stderr; it is computed by wrapping Showdown's own methods (observation only: the wrappers pass every
// argument and result through and call nothing the battle would not call).

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

const NEW_ABILITIES = ['flamebody', 'static', 'roughskin', 'toxicchain', 'baddreams', 'synchronize', 'magician', 'pickpocket', 'berserk', 'moxie', 'shielddust'];
const CHOICE = ['choiceband', 'choicespecs', 'choicescarf'];
const HOLD = ['leftovers', 'lifeorb', 'assaultvest', 'rockyhelmet', 'expertbelt', 'blacksludge', 'wiseglasses', ...CHOICE];
const RECOVERY = ['recover', 'roost', 'slackoff', 'softboiled', 'milkdrink', 'moonlight', 'synthesis', 'shoreup'];
const IDLE = ['recover', 'swordsdance', 'calmmind', 'roost'];
const STATUS_MOVES = ['toxic', 'willowisp', 'thunderwave', 'glare', 'stunspore', 'poisonpowder', 'poisongas'];
const itemName = (id) => id === '' ? '' : dex.items.get(id).name;
const abilityName = (id) => id === 'noability' ? 'No Ability' : dex.abilities.get(id).name;
const speStat = (id, level, ev) => Math.floor(Math.floor((2 * dex.species.get(id).baseStats.spe + 31 + Math.floor(ev / 4)) * level / 100) + 5);

// the Battle that `record` is playing (record builds its own; the scripts read it)
let currentBattle = null;

function assign(scenario0, pool, seed) {
	const rnd = mulberry32(seed * 2246822519 + 3266489917);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const has = (id) => pool.moves.includes(id);
	const hasItem = (id) => pool.items.includes(id);
	for (const id of NEW_ABILITIES) if (!pool.abilities.includes(id)) throw new Error(`the pool has no ability ${id}`);
	for (const id of ['toxic', 'willowisp', 'thunderwave', 'knockoff', 'thief', 'trick', 'toxicspikes', 'recover', 'uturn', 'substitute']) {
		if (!has(id)) throw new Error(`the pool has no ${id}`);
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
	const mv = (id) => dex.moves.get(id);
	const sp = (id) => dex.species.get(id);
	const speciesName = (id) => sp(id).name;
	const real = pool.species.filter(id => id !== 'shedinja');
	const hasShed = pool.species.includes('shedinja');
	const byType = (t) => real.filter(id => sp(id).types.includes(t));
	const notTypes = (...ts) => real.filter(id => ts.every(t => !sp(id).types.includes(t)));
	const put = (s, id) => {
		if (s.moves.includes(id)) return;
		s.moves.splice(int(4), 1, id);
		s.moves = [...new Set(s.moves)].slice(0, 4);
		while (s.moves.length < 4) { const m = pick(pool.moves); if (!s.moves.includes(m)) s.moves.push(m); }
	};
	const setItem = (s, id) => { s.item = itemName(id); };
	const setAbility = (s, id) => { s.ability = abilityName(id); };

	// the sprinkle: this slice's abilities on about half of the sets (the bench and the leads of the random battles), with the items and moves
	// they meet. Abilities that no script depends on only: a scripted kind overwrites its two leads below.
	const itemKit = ['rockyhelmet', 'rockyhelmet', 'leftovers', 'lifeorb', 'blacksludge', ...CHOICE, 'flameorb', 'toxicorb'].filter(hasItem);
	const moveKit = ['knockoff', 'thief', 'trick', 'switcheroo', 'toxic', 'willowisp', 'thunderwave', 'toxicspikes', 'uturn', 'doublehit', 'armthrust', 'furyswipes', 'bulletseed', 'scald', 'nuzzle', 'sandstorm'].filter(has);
	for (const side of [0, 1]) for (const s of scenario.teams[side]) {
		if (int(100) < 50) setAbility(s, pick(NEW_ABILITIES));
		if (int(100) < 22) setItem(s, pick(itemKit));
		if (int(100) < 30) put(s, pick(moveKit));
	}
	const kinds = [
		['random', 30], ['contactbody', 11], ['roughskin', 7], ['toxicchain', 7], ['sync', 9], ['magician', 7], ['pickpocket', 7], ['berserk', 8],
		['moxie', 8], ['dust', 6],
	];
	const roll = int(100);
	let kind = 'random', acc = 0;
	for (const [k, w] of kinds) { acc += w; if (roll < acc) { kind = k; break; } }
	// two in a hundred of the scripted share are the mirror kind (it replaces a random battle)
	if (kind === 'random' && int(100) < 4) kind = 'mirror';
	scenario.kind = kind;
	if (kind === 'random') return scenario;

	const aIdx = int(2), bIdx = 1 - aIdx;
	const A = scenario.teams[aIdx][0], B = scenario.teams[bIdx][0];
	const ctrl = { A: aIdx, B: bIdx, program: [[], []], turns: 0 };

	// ---- move catalogues (every move is in the pool)
	const plain = (id) => { const m = mv(id); return m.category !== 'Status' && m.priority === 0 && !m.selfSwitch && !m.forceSwitch && !m.selfdestruct && !m.mindBlownRecoil && !m.flags.charge && !m.flags.recharge && !m.self && !m.recoil && !m.drain && !m.ohko && m.flags.protect && (m.accuracy === true || m.accuracy === 100); };
	const weak = (id) => mv(id).basePower >= 35 && mv(id).basePower <= 70;
	const attacks = pool.moves.filter(id => plain(id) && weak(id) && !mv(id).multihit && !(mv(id).secondaries || []).length);
	const contact = attacks.filter(id => mv(id).flags.contact);
	const noncontact = attacks.filter(id => !mv(id).flags.contact);
	const multiAll = pool.moves.filter(id => plain(id) && mv(id).multihit);
	const multiContact = multiAll.filter(id => mv(id).flags.contact);
	const multiNon = multiAll.filter(id => !mv(id).flags.contact);
	const strong = pool.moves.filter(id => plain(id) && mv(id).basePower >= 85 && !mv(id).multihit);
	const strongContact = strong.filter(id => mv(id).flags.contact);
	const secOf = (id) => (mv(id).secondaries || []);
	const secStatus = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.priority === 0 && !m.selfSwitch && !m.forceSwitch && !m.flags.charge && !m.flags.recharge && !m.selfdestruct && secOf(id).some(x => ['brn', 'par', 'psn', 'tox'].includes(x.status) && x.chance >= 10) && (m.accuracy === true || m.accuracy >= 85); });
	const secTarget = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.priority === 0 && !m.selfSwitch && !m.forceSwitch && !m.flags.charge && !m.flags.recharge && !m.selfdestruct && secOf(id).some(x => !x.self && (x.status || x.volatileStatus === 'flinch' || (x.boosts && Object.values(x.boosts).some(b => b < 0)))) && (m.accuracy === true || m.accuracy >= 85); });
	const secSelf = pool.moves.filter(id => { const m = mv(id); return m.category !== 'Status' && m.priority === 0 && !m.selfSwitch && !m.forceSwitch && !m.flags.charge && !m.flags.recharge && !m.selfdestruct && secOf(id).some(x => x.self) && (m.accuracy === true || m.accuracy >= 85); });
	const types = (s) => sp(s.species.toLowerCase().replace(/[^a-z0-9]/g, '')).types;
	const canHit = (id, s) => dex.getImmunity(mv(id).type, types(s));
	const pickFor = (ids, s) => { const ok = ids.filter(id => canHit(id, s)); return pick(ok.length ? ok : ids); };
	const statusUsable = STATUS_MOVES.filter(has);

	// ---- speeds: A faster or slower than B, by more than a margin, and bulk for the holder
	const steady = (s, spe) => {
		s.level = 100; s.nature = 'Hardy';
		s.evs = { hp: 252, atk: 0, def: 0, spa: 0, spd: 0, spe };
		s.ivs = { hp: 31, atk: 31, def: 31, spa: 31, spd: 31, spe: 31 };
		if (s.item === 'Assault Vest' || s.item === 'Heavy-Duty Boots') s.item = '';
	};
	const speeds = (aFaster) => {
		steady(A, aFaster ? 252 : 0); steady(B, 0);
		for (let k = 0; k < 400; k++) {
			const a = pick(real);
			const sa = speStat(a, 100, aFaster ? 252 : 0);
			const bs = real.filter(b => (aFaster ? sa > speStat(b, 100, 0) + 10 : speStat(b, 100, 0) > sa + 10));
			if (bs.length) { A.species = speciesName(a); B.species = speciesName(pick(bs)); return; }
		}
		throw new Error('no speed pair');
	};
	const bulky = (s) => { s.nature = 'Bold'; s.evs = Object.assign({}, s.evs, { hp: 252, def: 252, spd: 4 }); };
	const setSpecies = (s, id) => { s.species = speciesName(id); };
	const moves = (s, ...ids) => { s.moves = [...new Set([...ids, ...IDLE])].slice(0, 4); };
	const clean = (s) => { s.item = ''; s.ability = 'No Ability'; };
	// every set but the two leads keeps what the sprinkle gave it; the leads are built here
	clean(A); clean(B);
	const aFaster = int(2) === 0;
	const turns = (n) => { ctrl.turns = n; };
	// a program: for each side, a function (battle, side, turn) -> a list of move ids to try in order (the first usable is chosen)
	const prog = (who, f) => { ctrl.program[who === 'A' ? aIdx : bIdx] = f; };
	const always = (...ids) => () => ids;
	const hpFrac = (p) => p.hp / p.maxhp;
	const foeOf = (b, sideIdx) => b.sides[1 - sideIdx].active[0];
	const mine = (b, sideIdx) => b.sides[sideIdx].active[0];

	if (kind === 'contactbody') {
		const v = int(8);
		const flame = int(2) === 0;
		const ab = flame ? 'flamebody' : 'static';
		const immuneType = flame ? 'Fire' : 'Electric';
		speeds(aFaster); bulky(B);
		setAbility(B, ab);
		if (v === 0) {
			// a contact move every turn: the draw is made each time, whatever happened before (the attacker is statused after the first success)
			const m = pickFor(contact, B); moves(A, m); moves(B, pickFor(noncontact, A)); setSpecies(B, pick(real));
			prog('A', always(m)); prog('B', always(pick(RECOVERY.filter(has)) || 'recover'));
			turns(9);
		} else if (v === 1) {
			// a non-contact move: no draw
			const m = pickFor(noncontact, B); moves(A, m, pickFor(contact, B)); moves(B, pickFor(noncontact, A));
			prog('A', always(m)); prog('B', always('recover'));
			turns(8);
		} else if (v === 2) {
			// a multi-hit contact move: a draw per hit
			const m = multiContact.length ? pickFor(multiContact, B) : pickFor(contact, B);
			moves(A, m); moves(B, pickFor(noncontact, A));
			prog('A', always(m)); prog('B', always('recover')); turns(9);
		} else if (v === 3) {
			// an attacker that is already statused: B (or the foe's own move) statuses A first, then A attacks every turn
			const m = pickFor(contact, B);
			const st = pick(statusUsable);
			moves(A, m); moves(B, st);
			prog('A', always(m)); prog('B', (b, s, t) => t === 1 ? [st] : ['recover']); turns(9);
		} else if (v === 4) {
			// the type that cannot be burned (Fire) or paralysed (Electric): the draw is made, the status fails
			const imm = byType(immuneType);
			setSpecies(A, pick(imm)); steady(A, aFaster ? 252 : 0);
			const m = pickFor(contact, B); moves(A, m); moves(B, 'recover');
			prog('A', always(m)); prog('B', always('recover')); turns(8);
		} else if (v === 5) {
			// a Rocky Helmet next to the ability: both are DamagingHit handlers, sorted left to right
			setItem(B, 'rockyhelmet');
			const m = pickFor(contact, B); moves(A, m); moves(B, 'recover');
			prog('A', always(m)); prog('B', always('recover')); turns(9);
		} else if (v === 6) {
			// a Shedinja attacker (1 HP): the helmet or Rough Skin... here the helmet faints it, after the ability's draw
			if (hasShed) { A.species = 'Shedinja'; A.level = 100; } else setSpecies(A, pick(real));
			setItem(B, 'rockyhelmet');
			const m = pickFor(contact, B); moves(A, m); moves(B, 'recover');
			prog('A', always(m)); prog('B', always('recover')); turns(3);
		} else {
			// a Synchronize attacker: the burn or paralysis goes back to the holder
			setAbility(A, 'synchronize');
			const m = pickFor(contact, B); moves(A, m); moves(B, 'recover');
			prog('A', always(m)); prog('B', always('recover')); turns(9);
		}
	} else if (kind === 'roughskin') {
		const v = int(6);
		speeds(aFaster); bulky(B);
		setAbility(B, 'roughskin');
		if (v === 0) { const m = pickFor(contact, B); moves(A, m); prog('A', always(m)); }
		else if (v === 1) { const m = pickFor(noncontact, B); moves(A, m); prog('A', always(m)); }
		else if (v === 2) { const m = multiContact.length ? pickFor(multiContact, B) : pickFor(contact, B); moves(A, m); prog('A', always(m)); }
		else if (v === 3) { setItem(B, 'rockyhelmet'); const m = pickFor(contact, B); moves(A, m); prog('A', always(m)); }
		else if (v === 4) {
			// a Shedinja attacker faints on the first contact hit, and so on until the team is gone or the script ends
			if (hasShed) { A.species = 'Shedinja'; A.level = 100; }
			if (int(2)) setItem(B, 'rockyhelmet');
			const m = pickFor(contact, B); moves(A, m); prog('A', always(m));
		} else {
			// a low-level attacker: Rough Skin takes a large share of its HP, and may faint it after a few turns
			A.level = 8 + int(15);
			const m = pickFor(contact, B); moves(A, m); prog('A', always(m));
		}
		moves(B, pickFor(noncontact, A)); prog('B', always('recover')); turns(9);
	} else if (kind === 'toxicchain') {
		const v = int(8);
		speeds(aFaster); bulky(B);
		setAbility(A, 'toxicchain');
		const m = v === 3 && multiNon.length ? pickFor(multiNon, B) : v === 6 && multiContact.length ? pickFor(multiContact, B) : pickFor(int(2) ? attacks : strong, B);
		moves(A, m); moves(B, 'recover');
		prog('A', always(m)); prog('B', always('recover'));
		if (v === 0) setAbility(B, 'shielddust');                       // no draw
		else if (v === 1) { setSpecies(B, pick(byType(int(2) ? 'Poison' : 'Steel'))); bulky(B); }   // immune: the draw is made, the status fails
		else if (v === 2) setAbility(B, 'synchronize');                  // the toxic goes back
		else if (v === 4) { moves(B, 'toxic'); prog('B', (b, s, t) => t === 1 ? ['toxic'] : ['recover']); } // already statused target (A is poisoned first)
		else if (v === 5) { setAbility(B, 'shielddust'); setItem(B, 'rockyhelmet'); }
		else if (v === 7) { setItem(B, pick(['rockyhelmet', 'leftovers', 'lifeorb'])); }
		turns(9);
	} else if (kind === 'sync') {
		const v = int(8);
		speeds(aFaster); bulky(B);
		setAbility(B, 'synchronize');
		if (v === 0) { const st = pick(statusUsable); moves(A, st, pickFor(attacks, B)); prog('A', (b, s, t) => t === 1 ? [st] : t === 2 ? [st] : [pickFor(attacks, B)]); }
		else if (v === 1) { const m = pick(secStatus); moves(A, m); prog('A', always(m)); }
		else if (v === 2) {
			// Toxic Spikes, then B (with Synchronize) switches in: the hazard's source is the foe, and the effect Toxic Spikes: no answer
			setSpecies(B, pick(real)); setAbility(scenario.teams[bIdx][1], 'synchronize'); clean(scenario.teams[bIdx][0]);
			moves(A, 'toxicspikes', pickFor(attacks, B)); prog('A', always('toxicspikes'));
			prog('B', (b, s, t) => t === 1 ? ['recover'] : t === 2 ? ['switch 2'] : ['recover']);
		}
		else if (v === 3) { setItem(B, pick(['flameorb', 'toxicorb'])); const m = pickFor(attacks, B); moves(A, m); prog('A', always(m)); }
		else if (v === 4) {
			// a source that cannot take the status: Fire (burn), Electric (paralysis), Poison or Steel (poison)
			const st = pick(['willowisp', 'thunderwave', 'toxic']);
			setSpecies(A, pick(byType({ willowisp: 'Fire', thunderwave: 'Electric', toxic: int(2) ? 'Poison' : 'Steel' }[st])));
			steady(A, aFaster ? 252 : 0); moves(A, st); prog('A', always(st));
		}
		else if (v === 5) { const st = pick(statusUsable); moves(A, st, pickFor(attacks, B)); setAbility(A, 'synchronize'); prog('A', always(st)); }  // both Synchronize
		else if (v === 6) { const st = pick(['toxic', 'willowisp', 'thunderwave']); moves(A, st, pickFor(attacks, B)); prog('A', (b, s, t) => t === 1 ? [st] : [pickFor(attacks, B), st]); prog('B', (b, s, t) => t === 1 ? [pick(statusUsable)] : ['recover']); } // the source is already statused
		else { const m = pick(secStatus); moves(A, m); setAbility(A, 'toxicchain'); prog('A', always(m)); }
		if (!ctrl.program[bIdx].length && typeof ctrl.program[bIdx] !== 'function') prog('B', always('recover'));
		moves(B, 'recover');
		turns(9);
	} else if (kind === 'magician') {
		const v = int(8);
		speeds(aFaster); bulky(B); setAbility(A, 'magician');
		const itemB = pick(['leftovers', 'lifeorb', 'blacksludge', 'rockyhelmet', 'assaultvest', ...CHOICE].filter(hasItem));
		setItem(B, int(5) === 0 ? '' : itemB);
		setItem(A, v === 0 ? pick(['leftovers', 'lifeorb']) : '');
		const m = pickFor(attacks, B);
		if (v === 1) { moves(A, 'knockoff'); prog('A', always('knockoff')); }
		else if (v === 2) { moves(A, 'thief'); prog('A', always('thief')); }
		else if (v === 3) { setItem(A, pick(CHOICE)); moves(A, 'trick', m); prog('A', (b, s, t) => t === 1 ? ['trick'] : [m]); }
		else if (v === 4) { const mm = multiAll.length ? pickFor(multiAll, B) : m; moves(A, mm); prog('A', always(mm)); }
		else if (v === 5) {
			// a Shedinja user at 1 HP: Rocky Helmet (the user faints, then the ability still looks at the target)
			if (hasShed) { A.species = 'Shedinja'; A.level = 100; }
			setItem(B, 'rockyhelmet'); const c = pickFor(contact, B); moves(A, c); prog('A', always(c));
		}
		else if (v === 6) { setItem(A, pick(CHOICE)); moves(B, 'trick'); prog('A', always(m)); prog('B', (b, s, t) => t === 1 ? ['trick'] : ['recover']); moves(A, m); }
		else { moves(A, m); prog('A', always(m)); }
		if (!ctrl.program[bIdx].length && typeof ctrl.program[bIdx] !== 'function') { moves(B, 'recover'); prog('B', always('recover')); }
		turns(8);
	} else if (kind === 'pickpocket') {
		const v = int(8);
		speeds(aFaster); bulky(B); setAbility(B, 'pickpocket');
		setItem(B, int(2) ? '' : pick(['leftovers', 'blacksludge', 'rockyhelmet'].filter(hasItem)));
		setItem(A, int(4) === 0 ? '' : pick(['leftovers', 'lifeorb', 'expertbelt', ...CHOICE].filter(hasItem)));
		const m = pickFor(contact, B);
		moves(B, 'recover');
		if (v === 0) { moves(A, m); prog('A', always(m)); }
		else if (v === 1) { const n = pickFor(noncontact, B); moves(A, n); prog('A', always(n)); }
		else if (v === 2) { moves(A, 'knockoff'); prog('A', always('knockoff')); }
		else if (v === 3) { moves(A, 'uturn', m); prog('A', always('uturn')); }
		else if (v === 4) { moves(A, 'thief'); setItem(A, ''); prog('A', always('thief')); }
		else if (v === 5) { const mm = multiContact.length ? pickFor(multiContact, B) : m; moves(A, mm); prog('A', always(mm)); }
		else if (v === 6) { setItem(B, ''); setItem(A, pick(CHOICE)); moves(A, m); prog('A', always(m)); }
		else { setItem(A, pick(CHOICE)); moves(A, 'trick', m); prog('A', (b, s, t) => t === 1 ? ['trick'] : [m]); }
		prog('B', always('recover')); turns(8);
	} else if (kind === 'berserk') {
		const v = int(6);
		speeds(aFaster); bulky(B); setAbility(B, 'berserk');
		B.evs = Object.assign({}, B.evs, { hp: 4 * int(64) });
		B.level = 60 + int(41);
		const rec = pick(RECOVERY.filter(has));
		const hit = pickFor(int(3) === 0 ? strongContact.length ? strongContact : strong : strong, B);
		moves(B, rec); moves(A, hit);
		if (v === 0 || v === 1) { prog('A', always(hit)); prog('B', always(rec)); }
		else if (v === 2) {
			// (a frail holder, so that the total of the hits can take it across half HP)
			B.level = 40 + int(30); B.evs = Object.assign({}, B.evs, { hp: 0, def: 0 });
			const mm = multiAll.length ? pickFor(multiAll, B) : hit; moves(A, mm);
			prog('A', always(mm)); prog('B', always(rec));
		} else if (v === 3) {
			// a poisoned holder: the residual damage crosses half HP (no trigger), the next hit may cross it again
			moves(A, 'toxic', hit); prog('A', (b, s, t) => t === 1 ? ['toxic'] : [hit]); prog('B', always(rec));
		} else if (v === 4) {
			setItem(B, pick(['leftovers', 'blacksludge'])); prog('A', always(hit)); prog('B', always(rec));
		} else {
			moves(A, 'willowisp', hit); prog('A', (b, s, t) => t === 1 ? ['willowisp'] : [hit]); prog('B', (b, s, t) => hpFrac(mine(b, bIdx)) > 0.9 ? [rec, ...IDLE] : [rec]);
		}
		turns(10);
	} else if (kind === 'moxie') {
		const v = int(7);
		speeds(true); setAbility(A, 'moxie');
		// a weak target that a hit or two knocks out, with a bench of weak ones behind it
		const weakTeam = (side) => { for (const s of scenario.teams[side]) { s.level = 5 + int(15); if (s !== (side === aIdx ? A : B)) { s.ability = 'No Ability'; } } };
		if (v === 0) {
			// a chain of knockouts: each forced switch is answered by the script
			weakTeam(bIdx); const m = pickFor(strong, B); moves(A, m); A.level = 100; prog('A', always(m)); prog('B', always('recover')); turns(10);
		} else if (v === 1) {
			// a double knockout: a Shedinja user into a Shedinja (or weak) target with a Rocky Helmet or Rough Skin: the user faints with it
			if (hasShed) { A.species = 'Shedinja'; A.level = 100; B.species = 'Shedinja'; B.level = 100; }
			else weakTeam(bIdx);
			if (int(2)) setItem(B, 'rockyhelmet'); else setAbility(B, 'roughskin');
			const m = pickFor(contact, B); moves(A, m); prog('A', always(m)); prog('B', always('recover')); turns(4);
		} else if (v === 2) {
			// a residual knockout: Toxic or Will-O-Wisp on a Shedinja: it goes at the end of the turn, which does not count
			if (hasShed) { B.species = 'Shedinja'; B.level = 100; } else weakTeam(bIdx);
			const st = pick(['toxic', 'willowisp']); moves(A, st, pickFor(attacks, B)); prog('A', (b, s, t) => t === 1 ? [st] : [pickFor(attacks, B)]); prog('B', always('recover')); turns(6);
		} else if (v === 3) {
			// recoil after the knockout: a Shedinja user with a recoil move: the target faints (Moxie raises Attack), then the user to the recoil
			const rc = ['doubleedge', 'takedown', 'flareblitz', 'bravebird', 'headsmash', 'woodhammer'].filter(has);
			weakTeam(bIdx); if (hasShed) { A.species = 'Shedinja'; A.level = 100; }
			const m = rc.length ? pickFor(rc, B) : pickFor(strong, B); moves(A, m); prog('A', always(m)); prog('B', always('recover')); turns(6);
		} else if (v === 4) {
			// a hazard knockout: Stealth Rock or Spikes on a weak team that faints coming in (not a move: no boost)
			weakTeam(bIdx); const hz = pick(['stealthrock', 'spikes'].filter(has)); const m = pickFor(strong, B);
			moves(A, hz, m); prog('A', (b, s, t) => t === 1 ? [hz] : [m]); prog('B', always('recover')); turns(10);
		} else if (v === 5) {
			// Life Orb on the user: the knockout, then the recoil
			weakTeam(bIdx); setItem(A, 'lifeorb'); const m = pickFor(strong, B); moves(A, m); prog('A', always(m)); prog('B', always('recover')); turns(10);
		} else {
			// a multi-hit move that knocks out in the middle of its hits
			weakTeam(bIdx); const m = multiAll.length ? pickFor(multiAll, B) : pickFor(strong, B); moves(A, m); prog('A', always(m)); prog('B', always('recover')); turns(10);
		}
		moves(B, 'recover');
	} else if (kind === 'dust') {
		const v = int(6);
		speeds(aFaster); bulky(B); setAbility(B, 'shielddust');
		const m = v === 3 && secSelf.length ? pick(secSelf) : v === 4 && secStatus.length ? pick(secStatus) : pick(secTarget);
		moves(A, m, pickFor(attacks, B)); moves(B, 'recover');
		prog('A', always(m)); prog('B', always('recover'));
		if (v === 1) setAbility(A, 'toxicchain');
		else if (v === 2) setItem(B, pick(['rockyhelmet', 'leftovers']));
		else if (v === 5) { const m2 = pick(secTarget); moves(A, m, m2); prog('A', (b, s, t) => t % 2 ? [m] : [m2]); }
		turns(9);
	} else if (kind === 'mirror') {
		// the same species and speed on both sides: Residual handlers tie, and the order of the tie is drawn
		const s0 = pick(real);
		setSpecies(A, s0); setSpecies(B, s0); steady(A, 0); steady(B, 0);
		A.ability = abilityName('baddreams'); B.ability = abilityName(int(2) ? 'baddreams' : 'noability');
		setItem(A, pick(['leftovers', 'blacksludge', 'toxicorb', 'flameorb'].filter(hasItem))); setItem(B, pick(['leftovers', 'blacksludge', 'toxicorb', 'flameorb'].filter(hasItem)));
		moves(A, pickFor(attacks, B)); moves(B, pickFor(attacks, A));
		prog('A', always(A.moves[0])); prog('B', always(B.moves[0])); turns(8);
	}
	scenario.ctrl = ctrl;
	return scenario;
}

// ---------------------------------------------------------------------------
// The scripts: an object `record` takes for `choices` (it calls `.slice()` and then `.shift()` at every decision point).

function chooser(ctrl, rnd) {
	const FULL = 'team 123456';
	const state = { turnsDone: 0 };
	const switchesOf = (side) => {
		const req = side.activeRequest;
		if (!req || !req.side) return [];
		return req.side.pokemon.map((p, i) => (p.active || p.condition.endsWith(' fnt')) ? null : `switch ${i + 1}`).filter(Boolean);
	};
	const choose = (sideIdx) => {
		const b = currentBattle;
		const side = b.sides[sideIdx];
		const req = side.activeRequest;
		if (!req || req.wait) return null;
		if (req.teamPreview) return FULL;
		if (req.forceSwitch) {
			const sw = switchesOf(side);
			return sw.length ? sw[Math.floor(rnd() * sw.length)] : null;
		}
		const t = b.turn;
		if (t > ctrl.turns) return undefined; // the random policy takes over
		const program = ctrl.program[sideIdx];
		if (typeof program !== 'function') return undefined;
		const want = program(b, side, t);
		const ms = req.active[0].moves;
		for (const id of want) {
			if (id.startsWith('switch')) { if (switchesOf(side).includes(id)) return id; continue; }
			const i = ms.findIndex(m => m.id === id && !m.disabled && !(m.pp !== undefined && m.pp <= 0));
			if (i >= 0) return `move ${i + 1}`;
		}
		return undefined;
	};
	return { slice: () => ({ shift: () => [choose(0), choose(1)] }) };
}

// ---------------------------------------------------------------------------
// The tally. Every wrapper passes its arguments and result through.

function newTally() {
	return {
		flamebody: { contactDraw: 0, burned: 0, noDraw: 0, attackerStatused: 0, attackerFireType: 0, multihitDraws: 0, withHelmet: 0, attackerFainted: 0, synchronizeAnswer: 0 },
		static: { contactDraw: 0, paralysed: 0, noDraw: 0, attackerStatused: 0, attackerElectricType: 0, multihitDraws: 0, withHelmet: 0, attackerFainted: 0 },
		roughskin: { damage: 0, withHelmet: 0, nonContactNoDamage: 0, multihit: 0, faintedAttacker: 0 },
		toxicchain: { draw: 0, poisoned: 0, shieldDust: 0, immuneType: 0, targetStatused: 0, noContactNeeded: 0, multihitDraws: 0, synchronizeAnswer: 0 },
		baddreams: { tieInResidual: 0 },
		synchronize: { fromMove: 0, fromSecondary: 0, fromToxicSpikes: 0, fromOrb: 0, sourceImmune: 0, sourceStatused: 0, fromAbility: 0 },
		magician: { stole: 0, emptyHandTargetEmpty: 0, fullHand: 0, afterKnockOff: 0, multihit: 0, choiceItemTaken: 0, userFainted: 0 },
		pickpocket: { stole: 0, holderFull: 0, attackerEmpty: 0, nonContact: 0, afterKnockOff: 0, switching: 0, choiceItemTaken: 0 },
		berserk: { boosted: 0, crossedBySingleHit: 0, crossedByMultihit: 0, crossedByResidual: 0, hitNoCross: 0, hitFromAboveHalfStaysAbove: 0 },
		moxie: { boosted: 0, doubleKnockout: 0, residualKnockoutNoBoost: 0, hazardKnockoutNoBoost: 0, recoilFaintAfterBoost: 0, multihitKnockout: 0, chained: 0 },
		shielddust: { modified: 0, droppedAll: 0, keptSelf: 0, statusDropped: 0, flinchDropped: 0, dropsDropped: 0 },
	};
}
let activeTally = null;
const t = () => activeTally;
const idOf = (e) => e && e.id;

function install() {
	const setPlayer = Battle.prototype.setPlayer;
	Battle.prototype.setPlayer = function (...args) { currentBattle = this; return setPlayer.apply(this, args); };

	// randomChance by the running effect
	const randomChance = Battle.prototype.randomChance;
	Battle.prototype.randomChance = function (...args) {
		const c = t();
		const id = c && this.effect && this.effect.id;
		const r = randomChance.apply(this, args);
		if (c && args[0] === 3 && args[1] === 10) {
			const ev = this.event;
			if (id === 'flamebody' || id === 'static') {
				const tally = c[id];
				tally.contactDraw++;
				const src = ev && ev.source;
				if (src) {
					if (src.status) tally.attackerStatused++;
					if (src.hasType(id === 'flamebody' ? 'Fire' : 'Electric')) tally[id === 'flamebody' ? 'attackerFireType' : 'attackerElectricType']++;
					const mvu = this.activeMove;
					if (mvu && mvu.multihit) tally.multihitDraws++;
					if (ev.target && ev.target.item === 'rockyhelmet') tally.withHelmet++;
					if (src.hp <= 0) tally.attackerFainted++;
					if (src.ability === 'synchronize') c.flamebody.synchronizeAnswer += id === 'flamebody' ? 1 : 0;
				}
			}
			if (id === 'toxicchain') {
				c.toxicchain.draw++;
				const tgt = ev && ev.target;
				if (tgt) {
					if (tgt.status) c.toxicchain.targetStatused++;
					if (tgt.hasType('Poison') || tgt.hasType('Steel')) c.toxicchain.immuneType++;
					if (this.activeMove && !this.activeMove.flags.contact) c.toxicchain.noContactNeeded++;
					if (this.activeMove && this.activeMove.multihit) c.toxicchain.multihitDraws++;
					if (tgt.ability === 'synchronize') c.toxicchain.synchronizeAnswer++;
				}
			}
		}
		return r;
	};
	// a status set by an effect of this slice
	const trySetStatus = Pokemon.prototype.trySetStatus;
	Pokemon.prototype.trySetStatus = function (status, source, sourceEffect) {
		const c = t();
		const r = trySetStatus.call(this, status, source, sourceEffect);
		if (c) {
			const id = (sourceEffect && sourceEffect.id) || (this.battle.effect && this.battle.effect.id);
			if (r && id === 'flamebody') c.flamebody.burned++;
			if (r && id === 'static') c.static.paralysed++;
			if (r && id === 'toxicchain') c.toxicchain.poisoned++;
		}
		return r;
	};
	// Synchronize's branches, by the source of the status (setStatus is where AfterSetStatus runs)
	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, sourceEffect, ...rest) {
		const c = t();
		if (c) {
			if (eventid === 'AfterSetStatus' && target && target.ability === 'synchronize' && source && source !== target) {
				const eid = sourceEffect && sourceEffect.id;
				if (eid === 'toxicspikes') c.synchronize.fromToxicSpikes++;
				else if (sourceEffect && sourceEffect.effectType === 'Move' && this.activeMove && sourceEffect.id === this.activeMove.id) {
					if (this.activeMove.category === 'Status') c.synchronize.fromMove++; else c.synchronize.fromSecondary++;
				} else if (sourceEffect && sourceEffect.effectType === 'Ability') c.synchronize.fromAbility++;
				if (source.status) c.synchronize.sourceStatused++;
				else if (!dex.getImmunity(rest[0].id === 'tox' ? 'psn' : rest[0].id, source.getTypes())) c.synchronize.sourceImmune++;
			}
			if (eventid === 'AfterSetStatus' && target && target.ability === 'synchronize' && source === target) c.synchronize.fromOrb++;
			if (eventid === 'DamagingHit' && target && source && !Array.isArray(target)) {
				if (target.ability === 'toxicchain') { /* toxicchain holds onSource */ }
				if (source.ability === 'toxicchain' && target.hasAbility('shielddust')) c.toxicchain.shieldDust++;
				if (target.ability === 'roughskin' && !this.activeMove.flags.contact) c.roughskin.nonContactNoDamage++;
				if ((target.ability === 'flamebody' || target.ability === 'static') && !this.activeMove.flags.contact) c[target.ability].noDraw++;
			} else if (eventid === 'DamagingHit' && Array.isArray(target) && target.length) {
				const tg = target[0];
				if (tg.ability === 'roughskin' && !this.activeMove.flags.contact) c.roughskin.nonContactNoDamage++;
				if ((tg.ability === 'flamebody' || tg.ability === 'static') && !this.activeMove.flags.contact) c[tg.ability].noDraw++;
				if (source && source.ability === 'toxicchain' && tg.hasAbility('shielddust')) c.toxicchain.shieldDust++;
			}
			if (eventid === 'ModifySecondaries' && target && target.ability === 'shielddust') {
				const before = rest[0];
				const r = runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
				c.shielddust.modified++;
				if (r.length === 0) c.shielddust.droppedAll++;
				if (r.length > 0) c.shielddust.keptSelf++;
				for (const s of before) {
					if (r.includes(s)) continue;
					if (s.status) c.shielddust.statusDropped++;
					if (s.volatileStatus === 'flinch') c.shielddust.flinchDropped++;
					if (s.boosts) c.shielddust.dropsDropped++;
				}
				return r;
			}
			if (eventid === 'AfterMoveSecondarySelf' && target && target.ability === 'magician' && this.activeMove && this.activeMove.category !== 'Status') {
				const foe = source;
				if (target.item) c.magician.fullHand++;
				else if (foe && !foe.item) c.magician.emptyHandTargetEmpty++;
				if (this.activeMove.multihit) c.magician.multihit++;
				if (target.hp <= 0) c.magician.userFainted++;
				if (this.activeMove.id === 'knockoff') c.magician.afterKnockOff++;
			}
			const tg0 = Array.isArray(target) ? target[0] : target;
			if (eventid === 'AfterMoveSecondary' && tg0 && tg0.ability === 'pickpocket' && source && this.activeMove) {
				if (!this.activeMove.flags.contact) c.pickpocket.nonContact++;
				else {
					if (tg0.item) c.pickpocket.holderFull++;
					if (!source.item) c.pickpocket.attackerEmpty++;
					if (tg0.switchFlag || source.switchFlag) c.pickpocket.switching++;
					if (this.activeMove.id === 'knockoff') c.pickpocket.afterKnockOff++;
				}
			}
			if (eventid === 'AfterFaint' && target && source) {
				if (source.ability === 'moxie') {
					if (sourceEffect && sourceEffect.effectType === 'Move') {
						if (source.hp > 0 && source.isActive) c.moxie.boosted++;
						if (this.activeMove && this.activeMove.multihit) c.moxie.multihitKnockout++;
						if (source.boosts.atk > 0) c.moxie.chained++;
					}
				}
				if (target.ability === 'moxie' && rest[0] >= 2) c.moxie.doubleKnockout++;
			}
			if (eventid === 'AfterFaint' && target) {
				const moxieAlive = this.sides.some(s => s.active[0] && s.active[0].ability === 'moxie' && s.active[0].hp > 0 && s.active[0] !== target);
				if (moxieAlive && (!sourceEffect || sourceEffect.effectType !== 'Move')) {
					if (sourceEffect && ['psn', 'tox', 'brn'].includes(sourceEffect.id)) c.moxie.residualKnockoutNoBoost++;
					if (sourceEffect && ['stealthrock', 'spikes'].includes(sourceEffect.id)) c.moxie.hazardKnockoutNoBoost++;
				}
			}
		}
		return runEvent.call(this, eventid, target, source, sourceEffect, ...rest);
	};
	// the stolen items, by the log line each theft writes
	const add = Battle.prototype.add;
	Battle.prototype.add = function (...args) {
		const c = t();
		if (c && args[0] === '-item') {
			const from = args.find(a => typeof a === 'string' && a.startsWith('[from] ability:'));
			if (from === '[from] ability: Magician') {
				c.magician.stole++;
				if (CHOICE.includes(dex.items.get(args[2]).id)) c.magician.choiceItemTaken++;
			}
			if (from === '[from] ability: Pickpocket') {
				c.pickpocket.stole++;
				if (CHOICE.includes(dex.items.get(args[2]).id)) c.pickpocket.choiceItemTaken++;
			}
		}
		return add.call(this, ...args);
	};
	// Berserk: boosts by the ability, and the damage that crosses half HP, by its cause
	const boost = Battle.prototype.boost;
	Battle.prototype.boost = function (boosts, target, source, effect, ...rest) {
		const c = t();
		const eid = (effect && effect.id) || (this.effect && this.effect.id);
		const r = boost.call(this, boosts, target, source, effect, ...rest);
		if (c && r && eid === 'berserk') c.berserk.boosted++;
		if (c && r && eid === 'moxie') c.moxie.boosted += 0;
		return r;
	};
	const spreadDamage = Battle.prototype.spreadDamage;
	Battle.prototype.spreadDamage = function (damage, targetArray, source, effect, ...rest) {
		const c = t();
		const tg = c && targetArray && targetArray.length === 1 ? targetArray[0] : null;
		if (tg && effect) {
			if (effect.id === 'roughskin') {
				c.roughskin.damage++;
				if (source && source.item === 'rockyhelmet') c.roughskin.withHelmet++;
				if (this.activeMove && this.activeMove.multihit) c.roughskin.multihit++;
				if (typeof damage[0] === 'number' && tg.hp - Math.floor(damage[0]) <= 0) c.roughskin.faintedAttacker++;
			}
			if (tg.ability === 'berserk' && tg.hp > 0) {
				const before = tg.hp, half = tg.maxhp / 2;
				const r = spreadDamage.call(this, damage, targetArray, source, effect, ...rest);
				const crossed = before > half && tg.hp <= half;
				if (effect.effectType === 'Move') {
					if (crossed) c.berserk[effect.multihit ? 'crossedByMultihit' : 'crossedBySingleHit']++;
					else if (before > half) c.berserk.hitFromAboveHalfStaysAbove++;
					else c.berserk.hitNoCross++;
				} else if (crossed) c.berserk.crossedByResidual++;
				return r;
			}
		}
		return spreadDamage.call(this, damage, targetArray, source, effect, ...rest);
	};
	// a Moxie holder that faints of recoil after it was boosted (the knockout that boosted it came first)
	const faint = Pokemon.prototype.faint;
	Pokemon.prototype.faint = function (source, effect) {
		const c = t();
		if (c && !this.fainted && !this.faintQueued && effect && effect.id === 'recoil' && this.ability === 'moxie' && this.boosts.atk > 0) c.moxie.recoilFaintAfterBoost++;
		return faint.call(this, source, effect);
	};
	// Residual ties: two handlers of the same order, priority and speed in one Residual event
	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, comparator) {
		const c = t();
		if (c && list.length > 1 && list.some(h => h.effect && h.effect.id === 'baddreams')) {
			let tie = false;
			for (let i = 1; i < list.length; i++) {
				const a = list[i - 1], b2 = list[i];
				if (a.order === b2.order && a.priority === b2.priority && a.speed === b2.speed) tie = true;
			}
			if (tie) c.baddreams.tieInResidual++;
		}
		return speedSort.call(this, list, comparator);
	};
}

// counters that cannot be hit by construction (none: every branch below is reached by a scripted kind)
const STRUCTURAL_ZERO = [];

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice8d.js <outDir> <count> <poolFile> [firstSeed=1]');
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
		fs.writeFileSync(path.join(outDir, `slice8d-${k}.json`), JSON.stringify(pool[k]));
	}
	activeTally = newTally();
	install();
	let turns = 0, draws = 0, truncated = 0, written = 0, scripted = 0;
	const kindCount = {};
	for (let seed = first; seed < first + count; seed++) {
		const scenario = assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		kindCount[scenario.kind] = (kindCount[scenario.kind] || 0) + 1;
		if (scenario.ctrl) {
			scenario.choices = chooser(scenario.ctrl, mulberry32(seed * 2654435761 + 40503));
			scripted++;
		}
		delete scenario.ctrl;
		let trace;
		try { trace = record(scenario, process.env.SLICE8D_LOG ? { onEnd: (b) => console.error(b.log.join('\n')) } : undefined); } catch (e) { console.error(`seed ${seed} (${scenario.kind}): ${e.message}`); throw e; }
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice8d-${seed}.json`), JSON.stringify(trace));
		written++;
	}
	console.error(`${count} slice8d seeds, ${written} traces written (${scripted} scripted), ${truncated} truncated at the turn limit, ` +
		`${pool.moves.length} moves, ${pool.species.length} species, ${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws`);
	console.error(`kinds: ${JSON.stringify(kindCount)}`);
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

module.exports = { assign };
