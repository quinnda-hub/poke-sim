#!/usr/bin/env node
'use strict';
// Generates random battles for "slice 1" and records each on Showdown.
//
// Slice 1 adds the rule vocabulary (multi-hit, drain, recoil, self-heal, spread
// targets, and later stat stages and status conditions). Unlike gen-slice0.js,
// the move and species pools are not computed here: they are the lists the
// engine itself accepts, printed by `cabal run -v0 engine-pool`, so the traces
// exercise exactly what the engine claims to support.
//
// Usage:
//   node gen-slice1.js <outDir> <count> <poolFile> [firstSeed=1]
//
// <poolFile> is JSON {"moves": [ids], "species": [ids]}.
//
// Output: <outDir>/slice1-<seed>.json, one trace per battle, plus
// <outDir>/slice1-moves.json and <outDir>/slice1-species.json (copies of the
// pool, which the parity test compares with what the engine accepts). A coverage
// tally computed from Showdown's battle log over all battles is printed to stderr.
//
// Teams follow gen-slice0.js's random profile (levels, spreads, genders, mirror
// rate, natures), except that a third of the move slots are drawn from the
// moves with the slice's mechanics (multi-hit, drain, recoil, heal, spread, stat
// stages, statuses, thawing) and a sixth from the status-inflicting and thawing
// moves (and a twelfth from the freezing ones, which only a rare Ice target
// refuses), so that rare combinations occur often enough to be tallied.

const fs = require('fs');
const path = require('path');

const SHOWDOWN_DIR = process.env.SHOWDOWN_DIR ||
	path.resolve(__dirname, '..', '..', 'local', 'showdown');
const { Dex, Pokemon } = require(path.join(SHOWDOWN_DIR, 'dist', 'sim'));
const { record } = require('./record.js');

const dex = Dex.mod('gen9');
const NATURES = dex.natures.all().map(n => n.name).sort();
const STATS = ['hp', 'atk', 'def', 'spa', 'spd', 'spe'];

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

const isSpread = m => m.target === 'allAdjacent' || m.target === 'allAdjacentFoes';
const hasBoosts = m => !!(m.boosts || (m.self && m.self.boosts) ||
	(m.secondaries || []).some(s => s.boosts || (s.self && s.self.boosts)));
const hasStatus = m => !!(m.status || (m.secondaries || []).some(s => s.status || s.volatileStatus));
const hasThaw = m => !!(m.thawsTarget || (m.flags && m.flags.defrost));
const isSpecial = m => !!(m.multihit || m.drain || m.recoil || m.heal || isSpread(m) || hasBoosts(m) ||
	hasStatus(m) || hasThaw(m));
// The moves whose combinations are rare enough to need extra weight: the status moves that inflict a
// condition, and the moves that thaw (the user thaws with `defrost`, the target with `thawsTarget`).
const isStatusFocus = m => !!(m.status || hasThaw(m));

function makeScenario(seed, format, pool) {
	const rnd = mulberry32(seed * 2654435761);
	const int = (n) => Math.floor(rnd() * n);
	const pick = (xs) => xs[int(xs.length)];
	const special = pool.moves.filter(id => isSpecial(dex.moves.get(id)));
	const focus = pool.moves.filter(id => isStatusFocus(dex.moves.get(id)));
	const freezers = pool.moves.filter(id => (dex.moves.get(id).secondaries || []).some(x => x.status === 'frz'));
	const mirror = int(8) === 0;

	const makeSet = () => {
		const moves = new Set();
		while (moves.size < 4) {
			const k = int(6);
			moves.add(k < 2 ? pick(special) : k === 2 ? pick(focus) : k === 3 && int(2) === 0 ? pick(freezers) : pick(pool.moves));
		}
		const evs = {}, ivs = {};
		for (const s of STATS) { evs[s] = int(4) === 0 ? 0 : int(64) * 4; ivs[s] = int(4) === 0 ? int(32) : 31; }
		const set = {
			name: '', species: dex.species.get(pick(pool.species)).name,
			ability: 'No Ability', item: '', moves: [...moves],
			nature: pick(NATURES), evs, ivs,
			level: int(5) === 0 ? 1 + int(100) : 100,
		};
		const g = int(3);
		if (g === 0) set.gender = 'M'; else if (g === 1) set.gender = 'F';
		return set;
	};
	const makeTeam = () => Array.from({ length: 6 }, makeSet);

	const t1 = makeTeam();
	const t2 = mirror ? JSON.parse(JSON.stringify(t1)) : makeTeam();
	const hex = (n) => n.toString(16).padStart(4, '0');
	return {
		format,
		seed: `gen5,${hex(int(65536))}${hex(int(65536))}${hex(int(65536))}${hex(int(65536))}`,
		policySeed: seed,
		maxTurns: 300,
		teams: [t1, t2],
	};
}

// ---------------------------------------------------------------------------
// Coverage, from the battle log. Each move use is a segment: the `|move|` line
// and everything up to the next action line. Stat stages are tracked from the
// `-boost` / `-unboost` lines (Showdown logs every change that moved a stage)
// and reset on switch, drag and faint, as clearVolatile does. The active
// Pokemon's status is tracked from `-status`, `-curestatus` and the status that
// `switch` lines carry, and toxic's stage from its residual damage.
//
// Three outcomes leave no trace in the log, so they are counted by wrapping
// Showdown's own methods (observation only, nothing is changed): a status
// refused by a type immunity (`Pokemon.runStatusImmunity`; a secondary's refusal
// is silent) and a flinch that took hold (`Pokemon.addVolatile`).

const BOUNDARY = /^\|(move|switch|drag|turn|cant|upkeep|win|tie)\|?/;
const ZERO = () => ({ atk: 0, def: 0, spa: 0, spd: 0, spe: 0, accuracy: 0, evasion: 0 });
const STAGE_MUL = s => (s >= 0 ? (2 + s) / 2 : 2 / (2 - s));

function newTally() {
	const t = {
		hitcount: { 2: 0, 3: 0, 4: 0, 5: 0 },
		multihitStoppedByFaint: 0,
		drainHeal: 0, drainAtFullHp: 0,
		recoilFaint: 0, doubleKnockout: 0,
		healOk: 0, healFailFull: 0,
		spreadImmune: 0,
		boostFailedAtPlus6: 0, dropFailedAtMinus6: 0,
		critIgnoredNegativeAttack: 0, critIgnoredPositiveDefense: 0,
		moveWithAccuracyStage: 0, accuracyStageAndMiss: 0,
		selfDropUsed: 0, boostSecondaryOnTarget: 0, boostSecondarySelf: 0,
		switchOutWithBoosts: 0,
		speedBoostReversedOrder: 0,
		lowAccuracyUnderPositiveStage: 0,
		statusByMove: { brn: 0, par: 0, psn: 0, tox: 0, frz: 0 },
		statusBySecondary: { brn: 0, par: 0, psn: 0, tox: 0, frz: 0 },
		statusImmune: { 'brn:Fire': 0, 'par:Electric': 0, 'psn:Poison': 0, 'psn:Steel': 0, 'frz:Ice': 0 },
		thunderWaveVsGround: 0,
		statusMoveFailsOnStatused: 0,
		fullParalysis: 0,
		thawByRoll: 0, thawByDefrostMove: 0, thawByTargetHit: 0,
		flinchCant: 0, flinchAfterTargetMoved: 0,
		burnedPhysicalHit: 0,
		toxStage3: 0, toxStageResetOnSwitch: 0,
		residualKnockout: 0, residualTieBothDamaged: 0,
		toxicByPoisonUser: 0,
	};
	return { t, used: new Set() };
}

// Observation hooks on Showdown's Pokemon class; counts go to the tally being filled.
const STATUS_IMMUNE = { brn: ['Fire'], par: ['Electric'], psn: ['Poison', 'Steel'], frz: ['Ice'] };
let activeTally = null;
function installInstruments(tally) {
	activeTally = tally;
	if (Pokemon.prototype.__sliceInstrumented) return;
	Pokemon.prototype.__sliceInstrumented = true;
	const runStatusImmunity = Pokemon.prototype.runStatusImmunity;
	Pokemon.prototype.runStatusImmunity = function (type, message) {
		const r = runStatusImmunity.call(this, type, message);
		if (!r && !this.fainted && activeTally && STATUS_IMMUNE[type]) {
			for (const ty of this.getTypes()) {
				if (STATUS_IMMUNE[type].includes(ty)) activeTally.t.statusImmune[`${type}:${ty}`]++;
			}
		}
		return r;
	};
	const addVolatile = Pokemon.prototype.addVolatile;
	Pokemon.prototype.addVolatile = function (status, ...rest) {
		const moved = !!this.moveThisTurn;
		const r = addVolatile.call(this, status, ...rest);
		const id = typeof status === 'string' ? status : status.id;
		if (r && id === 'flinch' && activeTally && moved) activeTally.t.flinchAfterTargetMoved++;
		return r;
	};
}

function tallyLog(tally, log, battle) {
	const { t, used } = tally;
	const stages = { p1: ZERO(), p2: ZERO() };
	const speOf = (side, name) => {
		const mon = battle.sides[side === 'p1' ? 0 : 1].pokemon.find(p => p.name === name);
		return mon ? mon.storedStats.spe : null;
	};
	const activeStatus = { p1: '', p2: '' };
	const activeKey = { p1: null, p2: null };
	const toxStage = { p1: 0, p2: 0 };
	const toxByMon = new Map(); // per side and details: the stage a switched-out badly poisoned Pokemon keeps
	let resid = {}; // this turn's residual damage by side: { kind, speed }
	let seg = null;
	let prevMove = null; // the previous `|move|` of this turn, when nothing else happened since
	const flush = () => {
		if (!seg) return;
		const m = dex.moves.get(seg.id);
		const body = seg.lines;
		const user = seg.side;
		const foeSide = user === 'p1' ? 'p2' : 'p1';
		const has = (re) => body.some(l => re.test(l));
		const hitcount = body.map(l => /^\|-hitcount\|[^|]*\|(\d+)/.exec(l)).filter(Boolean).map(r => +r[1])[0];
		if (hitcount !== undefined && t.hitcount[hitcount] !== undefined) t.hitcount[hitcount]++;
		if (typeof m.multihit === 'number' && hitcount !== undefined && hitcount < m.multihit &&
			has(new RegExp(`^\\|faint\\|${foeSide}`))) t.multihitStoppedByFaint++;
		if (m.drain) {
			if (has(/^\|-heal\|.*\[from\] drain/)) t.drainHeal++;
			else if (has(new RegExp(`^\\|-damage\\|${foeSide}[ab]: [^|]*\\|[^|]*$`))) t.drainAtFullHp++;
		}
		if (m.recoil && has(new RegExp(`^\\|-damage\\|${user}[ab]: [^|]*\\|0 fnt\\|\\[from\\] Recoil`))) {
			t.recoilFaint++;
			if (has(new RegExp(`^\\|faint\\|${foeSide}`))) t.doubleKnockout++;
		}
		if (m.heal && m.target === 'self') {
			if (has(new RegExp(`^\\|-fail\\|${user}[ab]: [^|]*\\|heal`))) t.healFailFull++;
			else if (has(new RegExp(`^\\|-heal\\|${user}[ab]: [^|]*\\|[^|]*$`))) t.healOk++;
		}
		if (isSpread(m) && has(/^\|-immune\|/)) t.spreadImmune++;
		const inflicted = body.map(l => new RegExp(`^\\|-status\\|${foeSide}[ab]: [^|]*\\|(brn|par|psn|tox|frz)`).exec(l)).filter(Boolean)[0];
		if (inflicted) (m.category === 'Status' ? t.statusByMove : t.statusBySecondary)[inflicted[1]]++;
		if (m.category === 'Status' && m.status && seg.foeStatus && has(/^\|-fail\|/)) t.statusMoveFailsOnStatused++;
		const typesOf = (name) => (name && dex.species.get(name).exists ? dex.species.get(name).types : []);
		if (m.id === 'thunderwave' && has(/^\|-immune\|/) && typesOf(seg.targetName).includes('Ground')) t.thunderWaveVsGround++;
		if (m.id === 'toxic' && typesOf(seg.userName).includes('Poison')) t.toxicByPoisonUser++;
		if (seg.userStatus === 'brn' && m.category === 'Physical' &&
			has(new RegExp(`^\\|-damage\\|${foeSide}[ab]: [^|]*\\|[^|]*$`))) t.burnedPhysicalHit++;
		const changed = (side) => has(new RegExp(`^\\|-(un)?boost\\|${side}[ab]: [^|]*\\|[a-z]+\\|[1-9]`));
		if (m.self && m.self.boosts && changed(user)) t.selfDropUsed++;
		if (m.category !== 'Status') {
			const secs = m.secondaries || [];
			if (secs.some(x => x.boosts) && changed(foeSide)) t.boostSecondaryOnTarget++;
			if (secs.some(x => x.self && x.self.boosts) && changed(user)) t.boostSecondarySelf++;
		}
		seg = null;
	};
	for (let li = 0; li < log.length; li++) {
		const line = log[li];
		const parts = line.split('|');
		const sideOfLine = /^(p[12])[ab]: /.exec(parts[2] || '');
		const lineSide = sideOfLine ? sideOfLine[1] : null;
		if (parts[1] === 'switch' || parts[1] === 'drag') {
			const key = `${lineSide}:${parts[3]}`;
			if (activeKey[lineSide] !== null) toxByMon.set(activeKey[lineSide], toxStage[lineSide]);
			const status = ((parts[4] || '').split(' ')[1]) || '';
			if (status === 'tox' && (toxByMon.get(key) || 0) > 0) t.toxStageResetOnSwitch++;
			toxByMon.set(key, 0);
			activeKey[lineSide] = key;
			activeStatus[lineSide] = status;
			toxStage[lineSide] = 0;
		} else if (parts[1] === '-status') {
			activeStatus[lineSide] = parts[3];
			if (parts[3] === 'tox') toxStage[lineSide] = 0;
		} else if (parts[1] === '-curestatus') {
			activeStatus[lineSide] = '';
			if (parts[3] === 'frz') {
				if ((parts[4] || '').startsWith('[from] move:')) t.thawByDefrostMove++;
				else if ((log[li + 1] || '').startsWith(`|move|${lineSide}`)) t.thawByRoll++;
				else t.thawByTargetHit++;
			}
		} else if (parts[1] === 'faint') {
			activeStatus[lineSide] = '';
		} else if (parts[1] === 'cant') {
			if (parts[3] === 'par') t.fullParalysis++;
			if (parts[3] === 'flinch') t.flinchCant++;
		} else if (parts[1] === '-damage' && /^\[from\] (psn|brn)$/.test(parts[4] || '')) {
			const kind = parts[4] === '[from] brn' ? 'brn' : 'psn';
			if (kind === 'psn' && activeStatus[lineSide] === 'tox') {
				toxStage[lineSide] = Math.min(15, toxStage[lineSide] + 1);
				if (toxStage[lineSide] >= 3) t.toxStage3++;
			}
			if ((parts[3] || '').startsWith('0 fnt')) t.residualKnockout++;
			const name = /^p[12][ab]: (.*)$/.exec(parts[2])[1];
			const spe = speOf(lineSide, name);
			const speed = spe === null ? null : Math.floor(spe * STAGE_MUL(stages[lineSide].spe));
			resid[lineSide] = { kind, speed };
		} else if (parts[1] === 'upkeep' || parts[1] === 'turn') {
			if (resid.p1 && resid.p2 && resid.p1.kind === resid.p2.kind && resid.p1.speed !== null &&
				resid.p1.speed === resid.p2.speed) t.residualTieBothDamaged++;
			resid = {};
		}
		const sw = /^\|(switch|drag)\|(p[12])[ab]: /.exec(line);
		if (sw) {
			const st = stages[sw[2]];
			if (Object.values(st).some(v => v !== 0)) t.switchOutWithBoosts++;
			stages[sw[2]] = ZERO();
		}
		const fnt = /^\|faint\|(p[12])[ab]: /.exec(line);
		if (fnt) stages[fnt[1]] = ZERO();
		const bs = /^\|-(boost|unboost)\|(p[12])[ab]: [^|]*\|([a-z]+)\|(\d+)/.exec(line);
		if (bs) {
			const n = +bs[4];
			if (n === 0) {
				if (bs[1] === 'boost') t.boostFailedAtPlus6++; else t.dropFailedAtMinus6++;
			} else {
				stages[bs[2]][bs[3]] += bs[1] === 'boost' ? n : -n;
			}
		}
		if (/^\|-crit\|/.test(line) && seg) {
			const m = dex.moves.get(seg.id);
			const foe = seg.side === 'p1' ? 'p2' : 'p1';
			const [a, d] = m.category === 'Physical' ? ['atk', 'def'] : ['spa', 'spd'];
			if (stages[seg.side][a] < 0) t.critIgnoredNegativeAttack++;
			if (stages[foe][d] > 0) t.critIgnoredPositiveDefense++;
		}
		if (BOUNDARY.test(line)) {
			flush();
			const mv = /^\|move\|(p[12])[ab]: ([^|]*)\|([^|]*)\|/.exec(line);
			if (mv) {
				const m = dex.moves.get(mv[3]);
				const id = m.id;
				if (id !== 'struggle') used.add(id);
				const foe = mv[1] === 'p1' ? 'p2' : 'p1';
				const tgt = /^\|move\|[^|]*\|[^|]*\|(p[12])[ab]: ([^|\[]*)/.exec(line);
				seg = {
					id, side: mv[1], lines: [], userStatus: activeStatus[mv[1]], foeStatus: activeStatus[foe],
					userName: mv[2], targetName: tgt ? tgt[2] : null,
				};
				if (m.accuracy !== true && m.target !== 'self' &&
					(stages[mv[1]].accuracy !== 0 || stages[foe].evasion !== 0)) {
					t.moveWithAccuracyStage++;
					seg.accStage = true;
					const net = Math.max(-6, Math.min(6, Math.max(-6, Math.min(6, stages[mv[1]].accuracy)) - stages[foe].evasion));
					if (net > 0 && m.accuracy < 75) t.lowAccuracyUnderPositiveStage++;
				}
				// A speed stage reversing the order: both moved this turn with equal priority, and the
				// order seen is the order of stage-adjusted Spe but not of the unadjusted Spe.
				if (prevMove && prevMove.side !== mv[1] && prevMove.priority === m.priority) {
					const first = speOf(prevMove.side, prevMove.name), second = speOf(mv[1], mv[2]);
					if (first !== null && second !== null) {
						const eff = (side, spe) => Math.floor(spe * STAGE_MUL(prevMove.stages[side].spe));
						if (first < second && eff(prevMove.side, first) > eff(mv[1], second)) t.speedBoostReversedOrder++;
					}
				}
				prevMove = { side: mv[1], name: mv[2], priority: m.priority, stages: { p1: { ...stages.p1 }, p2: { ...stages.p2 } } };
			} else {
				prevMove = null;
			}
		} else if (seg) {
			seg.lines.push(line);
			if (seg.accStage && /^\|-miss\|/.test(line)) t.accuracyStageAndMiss++;
		}
	}
	flush();
}

if (require.main === module) {
	const [outDir, countArg, poolFile, firstArg] = process.argv.slice(2);
	if (!outDir || !countArg || !poolFile) {
		console.error('usage: node gen-slice1.js <outDir> <count> <poolFile> [firstSeed=1]');
		process.exit(2);
	}
	const count = parseInt(countArg, 10), first = parseInt(firstArg || '1', 10);
	const pool = JSON.parse(fs.readFileSync(poolFile, 'utf8'));
	if (!Array.isArray(pool.moves) || !Array.isArray(pool.species) || pool.moves.length < 4) {
		throw new Error(`bad pool file ${poolFile}`);
	}
	for (const id of pool.moves) if (!dex.moves.get(id).exists) throw new Error(`unknown move ${id}`);
	for (const id of pool.species) if (!dex.species.get(id).exists) throw new Error(`unknown species ${id}`);
	fs.mkdirSync(outDir, { recursive: true });
	fs.writeFileSync(path.join(outDir, 'slice1-moves.json'), JSON.stringify(pool.moves));
	fs.writeFileSync(path.join(outDir, 'slice1-species.json'), JSON.stringify(pool.species));
	const tally = newTally();
	installInstruments(tally);
	let turns = 0, draws = 0, truncated = 0;
	for (let seed = first; seed < first + count; seed++) {
		const trace = record(makeScenario(seed, 'gen9ou', pool), { onEnd: b => tallyLog(tally, b.log, b) });
		turns += trace.final.turn;
		draws += trace.steps.reduce((n, s) => n + s.draws.length, 0) + trace.finalDraws.length;
		if (trace.truncated) truncated++;
		fs.writeFileSync(path.join(outDir, `slice1-${seed}.json`), JSON.stringify(trace));
	}
	const unused = pool.moves.filter(id => !tally.used.has(id));
	console.error(`${count} slice1 battles, ${pool.moves.length} moves, ${pool.species.length} species, ` +
		`${turns} turns, ${draws} draws, ${truncated} truncated`);
	console.error('coverage (from the Showdown battle log):');
	for (const [k, v] of Object.entries(tally.t)) console.error(`  ${k}: ${JSON.stringify(v)}`);
	console.error(`  pool moves used: ${tally.used.size}/${pool.moves.length}` +
		(unused.length ? `, never used: ${unused.join(' ')}` : ''));
}

module.exports = { makeScenario, tallyLog, newTally };
