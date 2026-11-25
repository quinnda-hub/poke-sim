#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 2" (the first abilities and items) and records each on Showdown.
//
// Slice 2 is slice 1 plus abilities and items. The teams are slice 1's (gen-slice1.js makeScenario, so the
// move, species and spread draws are the same), then each Pokemon gets an ability and an item drawn from the
// pools the engine accepts (`cabal run -v0 engine-pool`), from a generator of its own, so slice 1 stays
// reproducible from its seeds. A share of the Pokemon keep "No Ability" and no item, so the paths of slices 0
// and 1 stay covered here. Ability draws are biased toward what makes them observable: a pinch ability gets a
// move of its type, Technician a move of at most 60 base power. Item draws favour the residual items, so that
// two of them tie on speed often enough to be tallied. A mirror match keeps its two teams identical.
//
// Usage:
//   node gen-slice2.js <outDir> <count> <poolFile> [firstSeed=1] [profile=random]
//
// Profiles:
//   random    Slice 1's teams with abilities and items, as above.
//   struggle  Slice 0's struggle teams (gen-slice0.js: Ghost types whose only move is a blocked low-PP move, so
//             the battle is fought with Struggle) with abilities and items, weighted toward those that act on
//             Struggle: Rocky Helmet (Struggle makes contact), Life Orb, Technician (50 base power), Huge Power
//             and Pure Power. The moves are left alone.
//
// <poolFile> is JSON {"moves", "species", "abilities", "items"} (ids).
//
// Output: <outDir>/slice2-<seed>.json (random) or <outDir>/slice2-struggle-<seed>.json (struggle), plus slice2-moves.json, slice2-species.json, slice2-abilities.json and
// slice2-items.json (copies of the pool, which the parity test compares with what the engine accepts). A tally of
// the outcomes that distinguish an ability or item handler is printed to stderr; it is computed by wrapping
// Showdown's own methods (observation only, nothing is changed).

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Battle } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { record } = require('./record.js');
const { makeScenario } = require('./gen-slice1.js');
const { makeScenario: makeScenario0 } = require('./gen-slice0.js');

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

const PINCH = { blaze: 'Fire', overgrow: 'Grass', torrent: 'Water', swarm: 'Bug' };
const TYPE_ITEMS = {
	blackbelt: 'Fighting', blackglasses: 'Dark', charcoal: 'Fire', dragonfang: 'Dragon', fairyfeather: 'Fairy',
	hardstone: 'Rock', magnet: 'Electric', metalcoat: 'Steel', miracleseed: 'Grass', mysticwater: 'Water',
	nevermeltice: 'Ice', poisonbarb: 'Poison', sharpbeak: 'Flying', silkscarf: 'Normal', silverpowder: 'Bug',
	softsand: 'Ground', spelltag: 'Ghost', twistedspoon: 'Psychic',
};
const RESIDUAL_ITEMS = ['leftovers', 'blacksludge', 'flameorb', 'toxicorb'];

function assign(scenario, pool, seed) {
	const rnd = mulberry32(seed * 40503 + 7);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const abilities = pool.abilities.filter(id => id !== 'noability');
	const items = pool.items.filter(id => id !== '');
	// the residual items weigh three times as much as the others
	const weighted = items.concat(items.filter(id => id === 'leftovers').flatMap(id => [id, id]))
		.concat(items.filter(id => RESIDUAL_ITEMS.includes(id)));
	const damaging = pool.moves.map(id => dex.moves.get(id)).filter(m => m.category !== 'Status' && m.basePower > 0);
	const mirror = JSON.stringify(scenario.teams[0]) === JSON.stringify(scenario.teams[1]);
	const set = (s) => {
		const ab = int(100) < 30 ? 'noability' : pick(abilities);
		const it = int(100) < 20 ? '' : pick(weighted);
		if (PINCH[ab] && int(2) === 0) {
			const ofType = damaging.filter(m => m.type === PINCH[ab]);
			if (ofType.length) s.moves[int(s.moves.length)] = pick(ofType).id;
		}
		if (ab === 'technician' && int(2) === 0) {
			const weak = damaging.filter(m => m.basePower <= 60);
			s.moves[int(s.moves.length)] = pick(weak).id;
		}
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) s.moves.push(pick(pool.moves));
		s.moves = [...new Set(s.moves)];
		while (s.moves.length < 4) s.moves.push(pick(pool.moves));
		s.ability = ab === 'noability' ? 'No Ability' : dex.abilities.get(ab).name;
		s.item = it === '' ? '' : dex.items.get(it).name;
	};
	for (const s of scenario.teams[0]) set(s);
	if (mirror) scenario.teams[1] = JSON.parse(JSON.stringify(scenario.teams[0]));
	else for (const s of scenario.teams[1]) set(s);
	return scenario;
}

// The struggle profile: abilities and items only; the moves stay slice 0's.
function assignStruggle(scenario, pool, seed) {
	const rnd = mulberry32(seed * 69069 + 11);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const abilities = pool.abilities.filter(id => id !== 'noability');
	const items = pool.items.filter(id => id !== '');
	const favour = (xs, ids) => xs.concat(xs.filter(id => ids.includes(id)).flatMap(id => [id, id, id, id]));
	const ab = favour(abilities, ['technician', 'hugepower', 'purepower']);
	const it = favour(items, ['rockyhelmet', 'lifeorb']);
	const mirror = JSON.stringify(scenario.teams[0]) === JSON.stringify(scenario.teams[1]);
	const set = (s) => {
		const a = int(100) < 20 ? 'noability' : pick(ab);
		const i = int(100) < 15 ? '' : pick(it);
		s.ability = a === 'noability' ? 'No Ability' : dex.abilities.get(a).name;
		s.item = i === '' ? '' : dex.items.get(i).name;
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
		leftoversHeal: 0, leftoversAtFullHp: 0,
		blackSludgeHealPoison: 0, blackSludgeDamageNonPoison: 0, blackSludgeAtFullHpPoison: 0,
		lifeOrbRecoil: 0, lifeOrbRecoilKO: 0,
		rockyHelmetContact: 0, rockyHelmetKOofAttacker: 0, rockyHelmetNoContactMove: 0,
		flameOrbBurn: 0, flameOrbBlockedByType: 0, flameOrbBlockedByStatus: 0,
		toxicOrbToxic: 0, toxicOrbBlockedByType: 0, toxicOrbBlockedByStatus: 0,
		assaultVestDisabledStatusMove: 0, assaultVestNoStatusMove: 0,
		pinchActive: 0, pinchInactiveAboveThird: 0,
		technicianBoosted: 0, technicianAbove60: 0,
		expertBeltBoost: 0, expertBeltNotSuperEffective: 0,
		typeItemBoost: 0,
		hugePowerOrPurePower: 0, filterLikeReduced: 0, wiseGlassesBoost: 0, assaultVestSpDHits: 0,
		residualItemTieOnSpeed: 0,
		struggleRockyHelmet: 0, struggleLifeOrbRecoil: 0, struggleTechnicianBoosted: 0, struggleHugePowerOrPurePower: 0,
	};
}

function install() {
	if (Battle.prototype.__slice2Instrumented) return;
	Battle.prototype.__slice2Instrumented = true;
	const t = () => activeTally;

	const runEvent = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, source, effect, relayVar, ...rest) {
		const tl = t();
		if (!tl) return runEvent.call(this, eventid, target, source, effect, relayVar, ...rest);
		const c = tl;
		const first = Array.isArray(target) ? target[0] : target;
		const mon = first && first.hp !== undefined ? first : null;
		const hpBefore = source && source.hp !== undefined ? source.hp : null;
		const r = runEvent.call(this, eventid, target, source, effect, relayVar, ...rest);
		if (!mon) return r;
		const move = effect && effect.effectType === 'Move' ? effect : null;
		switch (eventid) {
		case 'BasePower':
			if (mon.ability === 'technician' && typeof relayVar === 'number') {
				if (relayVar <= 60) { if (r > relayVar) { c.technicianBoosted++; if (move && move.id === 'struggle') c.struggleTechnicianBoosted++; } } else c.technicianAbove60++;
			}
			if (TYPE_ITEMS[mon.item] && move && move.type === TYPE_ITEMS[mon.item] && r > relayVar) c.typeItemBoost++;
			if (mon.item === 'wiseglasses' && move && move.category === 'Special' && r > relayVar) c.wiseGlassesBoost++;
			break;
		case 'ModifyAtk':
		case 'ModifySpA':
			if (PINCH[mon.ability] && move && move.type === PINCH[mon.ability]) {
				if (mon.hp <= mon.maxhp / 3) { if (r > relayVar) c.pinchActive++; } else if (r === relayVar) c.pinchInactiveAboveThird++;
			}
			if ((mon.ability === 'hugepower' || mon.ability === 'purepower') && eventid === 'ModifyAtk' && r > relayVar) {
				c.hugePowerOrPurePower++;
				if (move && move.id === 'struggle') c.struggleHugePowerOrPurePower++;
			}
			break;
		case 'ModifySpD':
			if (mon.item === 'assaultvest' && r > relayVar) c.assaultVestSpDHits++;
			break;
		case 'ModifyDamage':
			if (mon.item === 'expertbelt' && source && move) {
				if (source.getMoveHitData(move).typeMod > 0) { if (r > relayVar) c.expertBeltBoost++; } else c.expertBeltNotSuperEffective++;
			}
			if (source && ['filter', 'solidrock', 'prismarmor'].includes(source.ability) && move &&
				source.getMoveHitData(move).typeMod > 0 && r < relayVar) c.filterLikeReduced++;
			break;
		case 'DisableMove':
			if (mon.item === 'assaultvest') {
				if (mon.moveSlots.some(s => dex.moves.get(s.id).category === 'Status')) {
					if (mon.moveSlots.some(s => s.disabled && dex.moves.get(s.id).category === 'Status')) c.assaultVestDisabledStatusMove++;
				} else c.assaultVestNoStatusMove++;
			}
			break;
		case 'DamagingHit':
			if (mon.item === 'rockyhelmet' && source && move) {
				if (move.flags.contact) {
					if (hpBefore > source.hp) {
						c.rockyHelmetContact++;
						if (move.id === 'struggle') c.struggleRockyHelmet++;
						if (source.hp === 0) c.rockyHelmetKOofAttacker++;
					}
				} else c.rockyHelmetNoContactMove++;
			}
			break;
		default:
		}
		return r;
	};

	// Life Orb: the user's hp around runEvent('AfterMoveSecondarySelf', user, target, move)
	const runEvent2 = Battle.prototype.runEvent;
	Battle.prototype.runEvent = function (eventid, target, ...rest) {
		const c = t();
		if (c && eventid === 'AfterMoveSecondarySelf' && target && target.item === 'lifeorb') {
			const before = target.hp;
			const r = runEvent2.call(this, eventid, target, ...rest);
			if (before > 0 && target.hp < before) {
				c.lifeOrbRecoil++;
				if (rest[1] && rest[1].id === 'struggle') c.struggleLifeOrbRecoil++;
				if (target.hp === 0) c.lifeOrbRecoilKO++;
			}
			return r;
		}
		return runEvent2.call(this, eventid, target, ...rest);
	};

	const singleEvent = Battle.prototype.singleEvent;
	Battle.prototype.singleEvent = function (eventid, effect, state, target, ...rest) {
		const c = t();
		if (!c || eventid !== 'Residual' || !target || target.hp === undefined) return singleEvent.call(this, eventid, effect, state, target, ...rest);
		const hp = target.hp, status = target.status, id = effect.id;
		const r = singleEvent.call(this, eventid, effect, state, target, ...rest);
		if (target.item !== id || !RESIDUAL_ITEMS.includes(id) || hp === 0) return r;
		if (id === 'leftovers') { if (hp >= target.maxhp) c.leftoversAtFullHp++; else if (target.hp > hp) c.leftoversHeal++; }
		if (id === 'blacksludge') {
			// target.types, not hasType(): hasType runs runEvent('Type'), which the battle itself would not run here
			if (target.types.includes('Poison')) { if (hp >= target.maxhp) c.blackSludgeAtFullHpPoison++; else if (target.hp > hp) c.blackSludgeHealPoison++; }
			else if (target.hp < hp) c.blackSludgeDamageNonPoison++;
		}
		if (id === 'flameorb' || id === 'toxicorb') {
			const want = id === 'flameorb' ? 'brn' : 'tox';
			const k = id === 'flameorb' ? 'flameOrb' : 'toxicOrb';
			if (!status && target.status === want) c[k + (id === 'flameorb' ? 'Burn' : 'Toxic')]++;
			else if (status) c[k + 'BlockedByStatus']++;
			else c[k + 'BlockedByType']++;
		}
		return r;
	};

	const speedSort = Battle.prototype.speedSort;
	Battle.prototype.speedSort = function (list, ...rest) {
		const c = t();
		if (c && list.length > 1 && list[0] && list[0].effect) {
			const hs = list.filter(h => h.effect && h.effect.effectType === 'Item' && RESIDUAL_ITEMS.includes(h.effect.id) &&
				h.callback && (h.order === 5 || h.order === 28));
			for (let i = 0; i < hs.length; i++) {
				for (let j = i + 1; j < hs.length; j++) {
					const a = hs[i], b = hs[j];
					if (a.effectHolder !== b.effectHolder && a.order === b.order && a.priority === b.priority &&
						a.subOrder === b.subOrder && a.speed === b.speed) c.residualItemTieOnSpeed++;
				}
			}
		}
		return speedSort.call(this, list, ...rest);
	};
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg, profileArg] = process.argv.slice(2);
	const profile = profileArg || 'random';
	if (!outDir || !countArg || !poolFile || !['random', 'struggle'].includes(profile)) {
		console.error('usage: node gen-slice2.js <outDir> <count> <poolFile> [firstSeed=1] [profile=random|struggle]');
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
		fs.writeFileSync(path.join(outDir, `slice2-${k}.json`), JSON.stringify(pool[k]));
	}
	const tally = newTally();
	activeTally = tally;
	install();
	let turns = 0, draws = 0, truncated = 0;
	const usedAbilities = {}, usedItems = {};
	for (let seed = first; seed < first + count; seed++) {
		const scenario = profile === 'struggle'
			? assignStruggle(makeScenario0(seed, 'gen9ou', 'struggle'), pool, seed)
			: assign(makeScenario(seed, 'gen9ou', pool), pool, seed);
		for (const s of scenario.teams.flat()) {
			usedAbilities[s.ability] = (usedAbilities[s.ability] || 0) + 1;
			usedItems[s.item || '(none)'] = (usedItems[s.item || '(none)'] || 0) + 1;
		}
		const trace = record(scenario);
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		const name = profile === 'struggle' ? `slice2-struggle-${seed}.json` : `slice2-${seed}.json`;
		fs.writeFileSync(path.join(outDir, name), JSON.stringify(trace));
	}
	console.error(`${count} slice2 ${profile} battles, ${pool.moves.length} moves, ${pool.species.length} species, ` +
		`${pool.abilities.length} abilities, ${pool.items.length} items, ${turns} turns, ${draws} draws, ${truncated} truncated`);
	console.error('tally (from Showdown\'s own events):');
	for (const [k, v] of Object.entries(tally)) console.error(`  ${k}: ${v}`);
	console.error(`  Pokemon per ability: ${JSON.stringify(usedAbilities)}`);
	console.error(`  Pokemon per item: ${JSON.stringify(usedItems)}`);
}
