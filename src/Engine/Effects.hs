{-# LANGUAGE OverloadedStrings #-}
-- | The hand-written handler bodies of the abilities and items the engine supports (docs/handlers.md section
-- 3). The metadata (order, priority, subOrder) and the list of handlers come from the export
-- (@abilities.json@, @items.json@); only the bodies are written here, each from its Showdown source
-- (data/abilities.ts, data/items.ts).
--
-- An ability or item is supported only when it is in these tables and the export lists no handler without a
-- body here ('Engine.Dex').
--
-- A move's own handlers (slice 4a) are bodies in 'moveBodies', on the same terms: the move is an effect, and it is
-- supported only when every handler the export lists for it (other than its @condition.*@ handlers, which belong to
-- its field condition) has a body.
--
-- = Rule kinds
--
-- A body is a data rule only when the vocabulary expresses Showdown's handler exactly, and a rule kind or a
-- condition kind exists only when at least three effects on the usage whitelist (data/usage/gen9ou.json) need
-- it (docs/handlers.md section 3):
--
-- +------------------------------+------------------------------------------------------------------------+
-- | rule or condition            | whitelisted effects                                                    |
-- +==============================+========================================================================+
-- | @RChainModify n d@ (no cond) | hugepower (ModifyAtk 2), assaultvest (ModifySpD 3/2), lifeorb          |
-- |                              | (ModifyDamage 5324/4096)                                               |
-- | @CMoveType@                  | 12 type items (blackglasses, charcoal, dragonfang, magnet, metalcoat,  |
-- |                              | miracleseed, mysticwater, nevermeltice, silkscarf, silverpowder,       |
-- |                              | softsand, spelltag), blaze, overgrow, torrent                          |
-- | @CHolderHpAtMost@            | blaze, overgrow, torrent                                               |
-- +------------------------------+------------------------------------------------------------------------+
--
-- Everything else is a native handler: a move-category condition (wiseglasses is the only whitelisted user),
-- a super-effective condition (expertbelt), the base-power-at-most-60 check (technician), residual heal and
-- damage (leftovers, blacksludge), the status orbs (flameorb, toxicorb), contact damage (rockyhelmet), Life
-- Orb's recoil, and the Assault Vest's move disabling.
--
-- Tailwind's @onModifySpe@ is @RChainModify 2 1 []@, a rule with three whitelisted users now (hugepower,
-- purepower, tailwind). The weather-conditioned doubling @RChainModify 2 1 [CWeather ids]@ is the condition kind
-- @CWeather@, used by three whitelisted abilities (swiftswim, chlorophyll, sandrush; slushrush shares it). Reflect and Light Screen's damage halving, the duration callbacks and Trick Room's restart
-- are native: one or two users each.
--
-- The seeds share one body shape (@RUseItem [CTerrain ids]@: use the item when the terrain is one of these), and
-- three whitelisted seeds (grassyseed, psychicseed, mistyseed) meet the bar, so it is a rule. The Protosynthesis and
-- Quark Drive bodies (a best-stat check and a start that computes it) are shared by two abilities, under the bar:
-- natives. Booster Energy's and Unburden's bodies are used once each: natives.
--
-- The weather and terrain setters (drought, drizzle, sandstream, snowwarning, grassysurge, psychicsurge, mistysurge,
-- electricsurge) are natives. Eight whitelisted-or-free users would meet the bar for one rule kind ("set this field
-- condition"), but the ids of the field conditions are only known after the field table is built from the moves, so
-- the rule would need the table before the abilities; the natives are four lines each. Not adopted: a candidate for
-- folding when the abilities are next touched. The terrain base-power bodies are natives too: electric and psychic
-- share "move type and a grounded attacker" (two users), grassy has two branches in one handler, and misty tests
-- the defender.
--
-- The field conditions of slice 3a are all native, none of them reaches the bar: layers (spikes, toxicspikes) have
-- two whitelisted users, so there is no rule kind for them; each hazard's @onSwitchIn@ is a body of its own
-- (stealthrock: damage by Rock effectiveness; spikes: damage by layer; toxicspikes: poison or absorb; stickyweb: a
-- Speed drop), each used by one whitelisted condition; toxicdebris is the one ability that sets a side condition
-- on its attacker. Heavy-Duty Boots has no handler of its own: the hazard bodies read the item.
--
-- The moves' bodies (slice 4a) are all native. A move's @onBasePower@ is a modifier under a condition, but each
-- condition has one whitelisted user: facade (the user has a status other than sleep), psyblade (Electric Terrain),
-- expandingforce (Psychic Terrain and a grounded user) and ficklebeam (a draw), so none reaches the bar of three and no
-- rule kind is added (venoshock, barbbarrage and brine, which are not whitelisted, do not count). The accuracy and type
-- changes write to the move in use ('Engine.Dispatch.singleMoveEvent'), not to an event's modifier, which 'HandlerRule'
-- does not express. "Accuracy by weather" (thunder, hurricane, blizzard, bleakwindstorm) has four whitelisted users and
-- would meet the bar as a rule kind; it is not adopted, because the rule interpreter has no channel for a changed move, and
-- the four bodies are three lines each: a candidate for folding when that channel exists. A body shared by several moves is one native: the weight
-- bands (lowkick, grassknot), Stored Power's boost count (storedpower, powertrip), the rain accuracy (thunder, hurricane),
-- the halving (ruination, superfang), the poisoned-target doubling (venoshock, barbbarrage) and the Dynamax check of
-- onTryHit (lowkick, grassknot, heavyslam).
--
-- Slice 4c's bodies are native for the same reason: each of Rapid Spin and Mortal Spin (the hazard sweep), Ice Spinner,
-- Ceaseless Edge, Stone Axe, Defog, Tidy Up, Clear Smog, Court Change, Haze, Aurora Veil's @onTry@ and the screen breakers'
-- @onTryHit@ is one move's own handler or shared by two or three whitelisted moves, and none is "a modifier under a condition".
-- Aurora Veil's @onAnyModifyDamage@ joins Reflect's and Light Screen's: one more native, each used by one condition.
module Engine.Effects
  ( abilityBodies
  , itemBodies
  , fieldCondBodies
  , moveBodies
  ) where

import qualified Data.Text as T

import Engine.Event

-- | The bodies of an ability, or 'Nothing' when the engine does not implement it. The arguments resolve a type
-- name and a weather name to their ids.
abilityBodies :: (T.Text -> Maybe Int) -> (T.Text -> Maybe Int) -> T.Text -> Maybe [(Prefix, EventId, Body)]
abilityBodies ty wx name = case name of
  "noability" -> Just []
  -- data/abilities.ts blaze, overgrow, torrent, swarm: ModifyAtk and ModifySpA,
  -- `move.type === T && attacker.hp <= attacker.maxhp / 3` -> chainModify(1.5)
  "blaze" -> pinch "Fire"
  "overgrow" -> pinch "Grass"
  "torrent" -> pinch "Water"
  "swarm" -> pinch "Bug"
  -- hugepower, purepower: onModifyAtk -> chainModify(2)
  "hugepower" -> Just [(POwn, EvModifyAtk, BRule (RChainModify 2 1 []))]
  "purepower" -> Just [(POwn, EvModifyAtk, BRule (RChainModify 2 1 []))]
  -- data/abilities.ts swiftswim, chlorophyll, slushrush: onModifySpe, `weather` is one of ... -> chainModify(2);
  -- sandrush the same with `this.field.isWeather('sandstorm')`, and onImmunity (type 'sandstorm' -> false)
  "swiftswim" -> speed ["raindance", "primordialsea"]
  "chlorophyll" -> speed ["sunnyday", "desolateland"]
  "slushrush" -> speed ["hail", "snowscape"]
  "sandrush" -> (++ [(POwn, EvImmunity, BNative NSandRushImmunity)]) <$> speed ["sandstorm"]
  -- hydration: onResidual, a status under rain -> cureStatus
  "hydration" -> Just [(POwn, EvResidual, BNative NHydration)]
  -- drought, drizzle, sandstream, snowwarning: onStart sets the weather (Drought and Drizzle return for Groudon with a
  -- Red Orb and Kyogre with a Blue Orb: neither orb is a supported item)
  "drought" -> Just [(POwn, EvStart, BNative NDrought)]
  "drizzle" -> Just [(POwn, EvStart, BNative NDrizzle)]
  "sandstream" -> Just [(POwn, EvStart, BNative NSandStream)]
  "snowwarning" -> Just [(POwn, EvStart, BNative NSnowWarning)]
  -- grassysurge, psychicsurge, mistysurge, electricsurge: onStart sets the terrain
  "grassysurge" -> Just [(POwn, EvStart, BNative NGrassySurge)]
  "psychicsurge" -> Just [(POwn, EvStart, BNative NPsychicSurge)]
  "mistysurge" -> Just [(POwn, EvStart, BNative NMistySurge)]
  "electricsurge" -> Just [(POwn, EvStart, BNative NElectricSurge)]
  -- overcoat: onImmunity (sandstorm, hail, powder -> false), onTryHit (a powder move the target is not naturally immune
  -- to is blocked)
  "overcoat" -> Just [(POwn, EvImmunity, BNative NOvercoatImmunity), (POwn, EvTryHit, BNative NOvercoatTryHit)]
  -- leafguard: onSetStatus (any status, in sun), onTryAddVolatile (yawn, in sun)
  "leafguard" -> Just [(POwn, EvSetStatus, BNative NLeafGuardSetStatus), (POwn, EvTryAddVolatile, BNative NLeafGuardTryAddVolatile)]
  -- protosynthesis: onStart runs its onWeatherChange (priority -2 as the SwitchIn), which adds the volatile in sun and
  -- removes it otherwise unless it came from Booster Energy; onEnd deletes the volatile
  "protosynthesis" ->
    Just [ (POwn, EvStart, BNative NProtosynthesisStart), (POwn, EvWeatherChange, BNative NProtosynthesisWeatherChange)
         , (POwn, EvEnd, BNative NProtosynthesisEnd) ]
  -- quarkdrive: the same under Electric Terrain
  "quarkdrive" ->
    Just [ (POwn, EvStart, BNative NQuarkDriveStart), (POwn, EvTerrainChange, BNative NQuarkDriveTerrainChange)
         , (POwn, EvEnd, BNative NQuarkDriveEnd) ]
  -- unburden: onAfterUseItem adds the volatile, onTakeItem too, onEnd removes it
  "unburden" ->
    Just [ (POwn, EvAfterUseItem, BNative NUnburdenAfterUseItem), (POwn, EvTakeItem, BNative NUnburdenTakeItem)
         , (POwn, EvEnd, BNative NUnburdenEnd) ]
  -- toxicdebris: onDamagingHit, a Physical move sets Toxic Spikes on the attacker's side
  "toxicdebris" -> Just [(POwn, EvDamagingHit, BNative NToxicDebris)]
  -- embodyaspectteal: onStart (also its SwitchIn: the state is new at every switch-in, so it boosts again)
  "embodyaspectteal" -> Just [(POwn, EvStart, BNative NEmbodyAspectTeal)]
  -- technician: onBasePower, basePower after the accumulated modifier <= 60 -> chainModify(1.5)
  "technician" -> Just [(POwn, EvBasePower, BNative NTechnician)]
  -- Slice 6c. regenerator (abilities.ts 3834): onSwitchOut heals baseMaxhp / 3 (Pokemon.heal); naturalcure (2812): onSwitchOut clears
  -- the status (its onCheckShow is log-only in singles and has no event point here: 'Engine.Dex.effectFor' skips it)
  "regenerator" -> Just [(POwn, EvSwitchOut, BNative NRegeneratorSwitchOut)]
  "naturalcure" -> Just [(POwn, EvSwitchOut, BNative NNaturalCureSwitchOut)]
  -- filter, solidrock, prismarmor: onSourceModifyDamage, `typeMod > 0` -> chainModify(0.75)
  "filter" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  "solidrock" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  "prismarmor" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  -- slice 8a
  "intimidate" -> Just [(POwn, EvStart, BNative NIntimidateStart)]
  "dauntlessshield" -> Just [(POwn, EvStart, BNative NDauntlessShieldStart)]
  "defiant" -> Just [(POwn, EvAfterEachBoost, BNative NDefiantAfterEachBoost)]
  "contrary" -> Just [(POwn, EvChangeBoost, BNative NContraryChangeBoost)]
  "clearbody" -> Just [(POwn, EvTryBoost, BNative NClearBodyTryBoost)]
  "guarddog" -> Just [(POwn, EvDragOut, BNative NGuardDogDragOut), (POwn, EvTryBoost, BNative NGuardDogTryBoost)]
  "innerfocus" -> Just [(POwn, EvTryAddVolatile, BNative NInnerFocusTryAddVolatile), (POwn, EvTryBoost, BNative NInnerFocusTryBoost)]
  "bigpecks" -> Just [(POwn, EvTryBoost, BNative NBigPecksTryBoost)]
  "mirrorarmor" -> Just [(POwn, EvTryBoost, BNative NMirrorArmorTryBoost)]
  "keeneye" -> Just [(POwn, EvModifyMove, BNative NKeenEyeModifyMove), (POwn, EvTryBoost, BNative NKeenEyeTryBoost)]
  "weakarmor" -> Just [(POwn, EvDamagingHit, BNative NWeakArmorDamagingHit)]
  "justified" -> Just [(POwn, EvDamagingHit, BNative NJustifiedDamagingHit)]
  "gooey" -> Just [(POwn, EvDamagingHit, BNative NGooeyDamagingHit)]
  "speedboost" -> Just [(POwn, EvResidual, BNative NSpeedBoostResidual)]
  -- slice 8b
  -- LATENT(8f): multiscale, heatproof, purifyingsalt, waterbubble, flashfire and unaware are @breakable@: the dispatcher's ability gate skips their
  -- handlers while the move in use ignores abilities (Mold Breaker and its kin), which is 8f's `ignoreAbility` and is not set yet.
  -- multiscale: onSourceModifyDamage, the holder at full HP -> chainModify(0.5)
  "multiscale" -> Just [(PSource, EvModifyDamage, BNative NMultiscale)]
  -- sharpness: onBasePower, `move.flags['slicing']` -> chainModify(1.5)
  "sharpness" -> Just [(POwn, EvBasePower, BNative NSharpness)]
  -- tintedlens: onModifyDamage, `typeMod < 0` -> chainModify(2); sniper: onModifyDamage, a critical hit -> chainModify(1.5)
  "tintedlens" -> Just [(POwn, EvModifyDamage, BNative NTintedLens)]
  "sniper" -> Just [(POwn, EvModifyDamage, BNative NSniper)]
  -- guts: onModifyAtk, any status -> chainModify(1.5) (the burn's halving is skipped in modifyDamage)
  "guts" -> Just [(POwn, EvModifyAtk, BNative NGuts)]
  -- transistor: onModifyAtk and onModifySpA, an Electric move -> chainModify([5325, 4096])
  "transistor" -> typeRule "Electric" 5325 4096 [POwn]
  -- heatproof: onSourceModifyAtk and onSourceModifySpA, a Fire move -> chainModify(0.5); onDamage halves burn damage
  "heatproof" -> (++ [(POwn, EvDamage, BNative NHeatproofDamage)]) <$> typeRule "Fire" 1 2 [PSource]
  -- purifyingsalt: onSetStatus (always false), onTryAddVolatile (yawn), onSourceModifyAtk and onSourceModifySpA, a Ghost move -> chainModify(0.5)
  "purifyingsalt" ->
    (++ [(POwn, EvSetStatus, BNative NPurifyingSaltSetStatus), (POwn, EvTryAddVolatile, BNative NPurifyingSaltTryAddVolatile)])
      <$> typeRule "Ghost" 1 2 [PSource]
  -- waterbubble: onModifyAtk and onModifySpA, a Water move -> chainModify(2); onSourceModifyAtk and onSourceModifySpA, a Fire move -> chainModify(0.5);
  -- onUpdate cures a burn; onSetStatus is false for a burn
  "waterbubble" -> do
    own <- typeRule "Water" 2 1 [POwn]
    src <- typeRule "Fire" 1 2 [PSource]
    Just (own ++ src ++ [(POwn, EvUpdate, BNative NWaterBubbleUpdate), (POwn, EvSetStatus, BNative NWaterBubbleSetStatus)])
  -- flashfire: onTryHit absorbs a Fire move and gives the holder the volatile (whose handlers are 'nativeBody' CFlashFire's); onEnd removes it
  "flashfire" -> Just [(POwn, EvTryHit, BNative NFlashFireTryHit), (POwn, EvEnd, BNative NFlashFireEnd)]
  -- supremeoverlord: onStart records the fallen count, onBasePower scales by it, onEnd only logs
  "supremeoverlord" ->
    Just [(POwn, EvStart, BNative NSupremeOverlordStart), (POwn, EvBasePower, BNative NSupremeOverlordPower), (POwn, EvEnd, BNative NLogOnly)]
  -- vesselofruin: onStart only logs; onAnyModifySpA lowers the special attack of every Pokemon but a Vessel of Ruin holder
  "vesselofruin" -> Just [(POwn, EvStart, BNative NLogOnly), (PAny, EvModifySpA, BNative NVesselOfRuinSpA)]
  -- unaware: onAnyModifyBoost
  "unaware" -> Just [(PAny, EvModifyBoost, BNative NUnawareModifyBoost)]
  -- slice 8c. The abilities that block a hit or guard against damage.
  -- LATENT(8f): all of them but Magic Guard and Poison Heal are `breakable` (Good as Gold, Water Absorb, Levitate, Sturdy, Sap Sipper, Soundproof,
  -- Bulletproof, Sweet Veil, Shell Armor): the dispatcher already skips their handlers while 'Engine.Damage.suppressingAbility' holds, and Levitate's
  -- grounding reads it, but no move sets @ignoreAbility@ until Mold Breaker arrives. Magic Bounce's reflected moves reach Good as Gold the same way.
  -- goodasgold (abilities.ts 1631): onTryHit, a Status move from another Pokemon -> null
  "goodasgold" -> Just [(POwn, EvTryHit, BNative NGoodAsGoldTryHit)]
  -- waterabsorb (5365): onTryHit, a Water move from another Pokemon -> heal baseMaxhp / 4, null
  "waterabsorb" -> Just [(POwn, EvTryHit, BNative NWaterAbsorbTryHit)]
  -- levitate (2312): no handler; airborneness is Pokemon#isGrounded ('Engine.Damage.isGrounded')
  "levitate" -> Just []
  -- sturdy (4674): onTryHit blocks an OHKO move (none is accepted), onDamage (priority -30) leaves 1 HP from full HP
  "sturdy" -> Just [(POwn, EvTryHit, BNative NSturdyTryHit), (POwn, EvDamage, BNative NSturdyDamage)]
  -- magicguard (2466): onDamage cancels every damage that is not a Move's
  "magicguard" -> Just [(POwn, EvDamage, BNative NMagicGuardDamage)]
  -- poisonheal (3332): onDamage (priority 1), psn and tox damage heals baseMaxhp / 8 instead
  "poisonheal" -> Just [(POwn, EvDamage, BNative NPoisonHealDamage)]
  -- sapsipper (4024): onTryHit (priority 1), a Grass move from another Pokemon -> +1 Attack, null; onAllyTryHitSide for an ally
  "sapsipper" -> Just [(POwn, EvTryHit, BNative NSapSipperTryHit), (PAlly, EvTryHitSide, BNative NSapSipperAllyTryHitSide)]
  -- soundproof (4437): onTryHit, a sound move from another Pokemon -> null; onAllyTryHitSide only logs
  "soundproof" -> Just [(POwn, EvTryHit, BNative NSoundproofTryHit), (PAlly, EvTryHitSide, BNative NSoundproofAllyTryHitSide)]
  -- bulletproof (480): onTryHit, a bullet move -> null
  "bulletproof" -> Just [(POwn, EvTryHit, BNative NBulletproofTryHit)]
  -- sweetveil (4788): onAllySetStatus (slp), onAllyTryAddVolatile (yawn)
  "sweetveil" -> Just [(PAlly, EvSetStatus, BNative NSweetVeilSetStatus), (PAlly, EvTryAddVolatile, BNative NSweetVeilTryAddVolatile)]
  -- shellarmor (4223): `onCriticalHit: false`, a constant handler
  "shellarmor" -> Just [(POwn, EvCriticalHit, BConst (RBool False))]
  -- slice 8d (data/abilities.ts)
  -- flamebody, static: onDamagingHit, a contact move and randomChance(3, 10) -> source.trySetStatus('brn' / 'par', target)
  "flamebody" -> Just [(POwn, EvDamagingHit, BNative NFlameBodyDamagingHit)]
  "static" -> Just [(POwn, EvDamagingHit, BNative NStaticDamagingHit)]
  -- roughskin: onDamagingHit (order 1), a contact move damages the source by baseMaxhp / 8
  "roughskin" -> Just [(POwn, EvDamagingHit, BNative NRoughSkinDamagingHit)]
  -- toxicchain: onSourceDamagingHit, randomChance(3, 10) -> target.trySetStatus('tox', source)
  "toxicchain" -> Just [(PSource, EvDamagingHit, BNative NToxicChainDamagingHit)]
  -- baddreams: onResidual (order 28, subOrder 2)
  "baddreams" -> Just [(POwn, EvResidual, BNative NBadDreamsResidual)]
  -- synchronize: onAfterSetStatus
  "synchronize" -> Just [(POwn, EvAfterSetStatus, BNative NSynchronizeAfterSetStatus)]
  -- magician: onAfterMoveSecondarySelf; pickpocket: onAfterMoveSecondary
  "magician" -> Just [(POwn, EvAfterMoveSecondarySelf, BNative NMagicianAfterMoveSecondarySelf)]
  "pickpocket" -> Just [(POwn, EvAfterMoveSecondary, BNative NPickpocketAfterMoveSecondary)]
  -- berserk: onDamage, onAfterMoveSecondary (onTryEatItem has no caller while berries are unsupported: 'Engine.Dex.effectFor' skips it)
  "berserk" -> Just [(POwn, EvDamage, BNative NBerserkDamage), (POwn, EvAfterMoveSecondary, BNative NBerserkAfterMoveSecondary)]
  -- moxie: onSourceAfterFaint
  "moxie" -> Just [(PSource, EvAfterFaint, BNative NMoxieAfterFaint)]
  -- shielddust: onModifySecondaries
  "shielddust" -> Just [(POwn, EvModifySecondaries, BNative NShieldDustModifySecondaries)]
  _ -> Nothing
  where
    speed ws = do
      ids <- traverse wx ws
      Just [(POwn, EvModifySpe, BRule (RChainModify 2 1 [CWeather ids]))]
    pinch t = do
      i <- ty t
      let r = BRule (RChainModify 3 2 [CMoveType i, CHolderHpAtMost 1 3])
      Just [(POwn, EvModifyAtk, r), (POwn, EvModifySpA, r)]
    -- slice 8b: `move.type === T` -> chainModify(n / d) on both stat events, under each of the prefixes
    typeRule t n d ps = do
      i <- ty t
      let r = BRule (RChainModify n d [CMoveType i])
      Just [(p, e, r) | p <- ps, e <- [EvModifyAtk, EvModifySpA]]

-- | The bodies of an item, or 'Nothing' when the engine does not implement it. The arguments resolve a type name and
-- a field-condition name (the seeds' terrain) to their ids.
itemBodies :: (T.Text -> Maybe Int) -> (T.Text -> Maybe Int) -> T.Text -> Maybe [(Prefix, EventId, Body)]
itemBodies ty fid name = case name of
  -- data/items.ts leftovers: onResidual heal(baseMaxhp / 16); blacksludge: heal 1/16 for a Poison-type, else
  -- damage 1/8
  "leftovers" -> Just [(POwn, EvResidual, BNative NLeftovers)]
  "blacksludge" -> Just [(POwn, EvResidual, BNative NBlackSludge)]
  -- lifeorb: onModifyDamage chainModify([5324, 4096]); onAfterMoveSecondarySelf recoil of baseMaxhp / 10
  "lifeorb" ->
    Just [ (POwn, EvModifyDamage, BRule (RChainModify 5324 4096 []))
         , (POwn, EvAfterMoveSecondarySelf, BNative NLifeOrbRecoil) ]
  -- expertbelt: onModifyDamage, `typeMod > 0` -> chainModify([4915, 4096])
  "expertbelt" -> Just [(POwn, EvModifyDamage, BNative NExpertBelt)]
  -- rockyhelmet: onDamagingHit, a contact move damages the attacker by baseMaxhp / 6
  "rockyhelmet" -> Just [(POwn, EvDamagingHit, BNative NRockyHelmet)]
  -- flameorb, toxicorb: onResidual, pokemon.trySetStatus('brn' / 'tox', pokemon)
  "flameorb" -> Just [(POwn, EvResidual, BNative NFlameOrb)]
  "toxicorb" -> Just [(POwn, EvResidual, BNative NToxicOrb)]
  -- slice 7e. choiceband (data/items.ts 971-992): onStart removes choicelock, onModifyMove adds it, onModifyAtk (priority 1)
  -- chainModify(1.5) (the dynamax test is moot: no Dynamax in gen9ou); choicespecs the same with onModifySpA, choicescarf with onModifySpe
  "choiceband" -> Just (choiceItem EvModifyAtk)
  "choicespecs" -> Just (choiceItem EvModifySpA)
  "choicescarf" -> Just (choiceItem EvModifySpe)
  -- assaultvest: onModifySpD chainModify(1.5); onDisableMove disables the Status moves
  "assaultvest" ->
    Just [ (POwn, EvModifySpD, BRule (RChainModify 3 2 []))
         , (POwn, EvDisableMove, BNative NAssaultVestDisable) ]
  -- wiseglasses: onBasePower, `move.category === 'Special'` -> chainModify([4505, 4096])
  "wiseglasses" -> Just [(POwn, EvBasePower, BNative NWiseGlasses)]
  -- heavydutyboots: no handlers ("Hazard Immunity implemented in moves.ts": the hazards' onSwitchIn bodies call
  -- pokemon.hasItem('heavydutyboots'))
  "heavydutyboots" -> Just []
  -- lightclay: no handlers ("implemented in the corresponding thing": the screens' durationCallback reads the item)
  "lightclay" -> Just []
  -- heatrock, damprock, smoothrock, icyrock: no handlers (the weathers' durationCallback reads the item)
  "heatrock" -> Just []
  "damprock" -> Just []
  "smoothrock" -> Just []
  "icyrock" -> Just []
  -- boosterenergy: onStart (sets `started`, then runs onUpdate), onUpdate (a Paradox ability with no sun or no Electric
  -- Terrain: use the item and add the volatile), onTakeItem (Paradox species cannot lose it)
  "boosterenergy" ->
    Just [ (POwn, EvStart, BNative NBoosterEnergyStart), (POwn, EvUpdate, BNative NBoosterEnergyUpdate)
         , (POwn, EvTakeItem, BNative NBoosterTakeItem) ]
  -- electricseed, grassyseed, mistyseed, psychicseed: onStart and onTerrainChange call pokemon.useItem() under their
  -- terrain (onStart also needs `!pokemon.ignoringItem()`, always true here); the boosts are the item's `boosts`
  "electricseed" -> seed "electricterrain"
  "grassyseed" -> seed "grassyterrain"
  "mistyseed" -> seed "mistyterrain"
  "psychicseed" -> seed "psychicterrain"
  -- Slice 6c. ejectbutton (items.ts 1692, onAfterMoveSecondaryPriority 2), redcard (5170): onAfterMoveSecondary
  "ejectbutton" -> Just [(POwn, EvAfterMoveSecondary, BNative NEjectButtonAfterMoveSecondary)]
  "redcard" -> Just [(POwn, EvAfterMoveSecondary, BNative NRedCardAfterMoveSecondary)]
  -- Slice 6d. ejectpack (items.ts 1717-1769): onAfterBoost, onAnySwitchIn (priority -4), onAnyAfterMove, onResidual (order 29), onUseItem,
  -- onUse, onEnd; onAnyAfterMega never fires in Gen 9 (Mega Evolution does not exist), and its handler is dropped by 'Engine.Dex'
  "ejectpack" ->
    Just [ (POwn, EvAfterBoost, BNative NEjectPackAfterBoost), (PAny, EvSwitchIn, BNative NEjectPackTrigger)
         , (PAny, EvAfterMove, BNative NEjectPackTrigger), (POwn, EvResidual, BNative NEjectPackTrigger)
         , (POwn, EvUseItem, BNative NEjectPackUseItem), (POwn, EvUse, BNative NEjectPackUse), (POwn, EvEnd, BNative NEjectPackEnd) ]
  -- terrainextender: no handlers (the terrains' durationCallback reads the item)
  "terrainextender" -> Just []
  _ -> case lookup name typeItems of
    -- onBasePower, `move.type === T` -> chainModify([4915, 4096]) (data/items.ts; each body checked against the
    -- source: the plain type check and nothing else)
    Just t -> do
      i <- ty t
      Just [(POwn, EvBasePower, BRule (RChainModify 4915 4096 [CMoveType i]))]
    Nothing -> Nothing
  where
    seed terrain = do
      i <- fid terrain
      let r = BRule (RUseItem [CTerrain [i]])
      Just [(POwn, EvStart, r), (POwn, EvTerrainChange, r)]

-- | Items whose only handler is the plain type check above. Excluded: souldew and the orbs (a species check),
-- the plates and masks (onTakeItem), punchingglove, pinkbow and polkadotbow (@basePower * 1.1@), muscleband
-- (a category check, no whitelisted user), and the incenses (nonstandard in Gen 9).
typeItems :: [(T.Text, T.Text)]
typeItems =
  [ ("blackbelt", "Fighting"), ("blackglasses", "Dark"), ("charcoal", "Fire"), ("dragonfang", "Dragon")
  , ("fairyfeather", "Fairy"), ("hardstone", "Rock"), ("magnet", "Electric"), ("metalcoat", "Steel")
  , ("miracleseed", "Grass"), ("mysticwater", "Water"), ("nevermeltice", "Ice"), ("poisonbarb", "Poison")
  , ("sharpbeak", "Flying"), ("silkscarf", "Normal"), ("silverpowder", "Bug"), ("softsand", "Ground")
  , ("spelltag", "Ghost"), ("twistedspoon", "Psychic")
  ]

-- | The bodies of a field condition (data/moves.ts @condition@ of the move that sets it), by its key, or 'Nothing'
-- when the engine does not implement it. 'PSide' is the condition's own @onSide<Event>@ callback, 'POwn' its
-- @on<Event>@ (run for each Pokemon in play by @fieldEvent@).
fieldCondBodies :: T.Text -> Maybe [(Prefix, EventId, Body)]
fieldCondBodies name = case name of
  -- stealthrock: onSideStart logs; onSwitchIn damages by the Rock effectiveness (Heavy-Duty Boots: none)
  "stealthrock" -> Just [(PSide, EvStart, BNative NLogOnly), (POwn, EvSwitchIn, BNative NStealthRockSwitchIn)]
  -- spikes: onSideStart layers = 1; onSideRestart fails at 3 layers, else layers++; onSwitchIn damages a grounded
  -- Pokemon by damageAmounts[layers]
  "spikes" ->
    Just [ (PSide, EvStart, BNative NLayerStart), (PSide, EvRestart, BNative NSpikesRestart)
         , (POwn, EvSwitchIn, BNative NSpikesSwitchIn) ]
  -- toxicspikes: the same with 2 layers; onSwitchIn: a grounded Poison type absorbs it, Steel and Boots ignore
  -- it, else poison (1 layer) or bad poison (2)
  "toxicspikes" ->
    Just [ (PSide, EvStart, BNative NLayerStart), (PSide, EvRestart, BNative NToxicSpikesRestart)
         , (POwn, EvSwitchIn, BNative NToxicSpikesSwitchIn) ]
  -- stickyweb: onSideStart logs; onSwitchIn drops a grounded Pokemon's Speed by 1 stage
  "stickyweb" -> Just [(PSide, EvStart, BNative NLogOnly), (POwn, EvSwitchIn, BNative NStickyWebSwitchIn)]
  -- reflect, lightscreen: durationCallback (8 turns with Light Clay, else 5), onAnyModifyDamage (halve the damage of
  -- a Physical / Special move that is not a critical hit, against the side), onSideStart and onSideEnd (log lines);
  -- the countdown is the duration key of onSideResidual
  "reflect" -> screen NReflectDamage
  "lightscreen" -> screen NLightScreenDamage
  -- auroraveil: durationCallback (the same Light Clay rule), onAnyModifyDamage (halve a non-critical hit unless Reflect or
  -- Light Screen already covers its category), onSideStart and onSideEnd (log lines)
  "auroraveil" -> screen NAuroraVeilDamage
  -- tailwind: durationCallback (4), onModifySpe chainModify(2), onSideStart and onSideEnd (log lines)
  "tailwind" ->
    Just [ (POwn, EvDurationCallback, BNative NTailwindDuration), (POwn, EvModifySpe, BRule (RChainModify 2 1 []))
         , (PSide, EvStart, BNative NLogOnly), (PSide, EvEnd, BNative NLogOnly) ]
  -- trickroom: durationCallback (5), onFieldStart and onFieldEnd (log lines), onFieldRestart removes it; its effect
  -- is in getActionSpeed (pokemon.ts 662)
  "trickroom" ->
    Just [ (POwn, EvDurationCallback, BNative NTrickRoomDuration), (PField, EvStart, BNative NLogOnly)
         , (PField, EvRestart, BNative NTrickRoomRestart), (PField, EvEnd, BNative NLogOnly) ]
  -- sunnyday, raindance, sandstorm, snowscape (data/conditions.ts): durationCallback (8 with the setter's rock, else 5),
  -- onFieldStart and onFieldEnd (log lines), onFieldResidual (a log line and eachEvent('Weather'), for sandstorm and
  -- snowscape only while it is the weather), and each weather's own handlers
  "sunnyday" ->
    Just (weather NSunnyDayDamage NWeatherResidual ++ [(POwn, EvImmunity, BNative NSunnyDayImmunity)])
  "raindance" -> Just (weather NRainDanceDamage NWeatherResidual)
  "sandstorm" ->
    Just ([ (POwn, EvModifySpD, BNative NSandstormSpD), (POwn, EvWeather, BNative NSandstormWeather) ]
           ++ common NWeatherResidualIfActive)
  "snowscape" -> Just ((POwn, EvModifyDef, BNative NSnowscapeDef) : common NWeatherResidualIfActive)
  -- electricterrain, grassyterrain, mistyterrain, psychicterrain (data/moves.ts condition): durationCallback (8 with
  -- Terrain Extender, else 5), onFieldStart and onFieldEnd (log lines), and each terrain's own handlers
  "electricterrain" ->
    Just (terrain [ (POwn, EvSetStatus, BNative NElectricTerrainSetStatus)
                  , (POwn, EvTryAddVolatile, BNative NElectricTerrainTryAddVolatile)
                  , (POwn, EvBasePower, BNative NElectricTerrainPower) ])
  "grassyterrain" ->
    Just (terrain [(POwn, EvBasePower, BNative NGrassyTerrainPower), (POwn, EvResidual, BNative NGrassyTerrainHeal)])
  "mistyterrain" ->
    Just (terrain [ (POwn, EvSetStatus, BNative NMistyTerrainSetStatus)
                  , (POwn, EvTryAddVolatile, BNative NMistyTerrainTryAddVolatile)
                  , (POwn, EvBasePower, BNative NMistyTerrainPower) ])
  "psychicterrain" ->
    Just (terrain [(POwn, EvTryHit, BNative NPsychicTerrainTryHit), (POwn, EvBasePower, BNative NPsychicTerrainPower)])
  _ -> Nothing
  where
    terrain own =
      own ++ [ (POwn, EvDurationCallback, BNative NTerrainDuration), (PField, EvStart, BNative NLogOnly)
             , (PField, EvEnd, BNative NLogOnly) ]
    common residual =
      [ (POwn, EvDurationCallback, BNative NWeatherDuration), (PField, EvStart, BNative NLogOnly)
      , (PField, EvResidual, BNative residual), (PField, EvEnd, BNative NLogOnly) ]
    weather dmg residual = (POwn, EvWeatherModifyDamage, BNative dmg) : common residual
    screen dmg =
      Just [ (POwn, EvDurationCallback, BNative NLightClayDuration), (PAny, EvModifyDamage, BNative dmg)
           , (PSide, EvStart, BNative NLogOnly), (PSide, EvEnd, BNative NLogOnly) ]

-- | The bodies of a move's own handlers (data/moves.ts), by its id, or 'Nothing' when the engine does not implement
-- them. A move with no handler of its own needs no entry. The @condition.*@ handlers are not listed here.
--
-- The heal and Pain Split bodies (slice 4b) are native, each used by one to three moves: the weather heal by moonlight, synthesis and
-- morningsun (and shoreup's sand variant), the quarter heal and cure by junglehealing and lunarblessing. Roost has no handler of its
-- own: its heal and volatile are rules (@heal@, @self.volatileStatus@) and the volatile's @onType@ is
-- 'Engine.Event.nativeBody'.
--
-- Not implemented: mistyexplosion has the @onBasePower@ of expandingforce under Misty Terrain, but its @selfdestruct@
-- keeps the move rejected, so a body would run on no path. Payback, triplekick and tripleaxel (per-turn history,
-- per-hit accuracy) are later phases.
--
-- Slice 4d takes items: knockoff, trick, switcheroo, thief and poltergeist. Not accepted, with the reason: covet (not on the
-- whitelist; it would share thief's body), bugbite and pluck (eat the target's berry), fling, recycle, embargo, magicroom and
-- corrosivegas (item state and effects the engine does not hold, or no body here), and the abilities magician, pickpocket,
-- stickyhold, harvest and cudchew.
moveBodies :: T.Text -> Maybe [(Prefix, EventId, Body)]
moveBodies name = case name of
  -- lowkick, grassknot: basePowerCallback by the target's weight; onTryHit fails on a Dynamaxed target
  "lowkick" -> Just [(POwn, EvBasePowerCallback, BNative NWeightPower), (POwn, EvTryHit, BNative NDynamaxTryHit)]
  "grassknot" -> Just [(POwn, EvBasePowerCallback, BNative NWeightPower), (POwn, EvTryHit, BNative NDynamaxTryHit)]
  -- heavyslam: basePowerCallback by the weight ratio; onTryHit as above
  "heavyslam" -> Just [(POwn, EvBasePowerCallback, BNative NHeavySlamPower), (POwn, EvTryHit, BNative NDynamaxTryHit)]
  -- hex: doubled against a target with a status
  "hex" -> Just [(POwn, EvBasePowerCallback, BNative NHexPower)]
  -- storedpower, powertrip: 20 more per positive stat stage of the user
  "storedpower" -> Just [(POwn, EvBasePowerCallback, BNative NStoredPower)]
  "powertrip" -> Just [(POwn, EvBasePowerCallback, BNative NStoredPower)]
  -- acrobatics: doubled with no item
  "acrobatics" -> Just [(POwn, EvBasePowerCallback, BNative NAcrobaticsPower)]
  -- risingvoltage: doubled against a grounded target in Electric Terrain
  "risingvoltage" -> Just [(POwn, EvBasePowerCallback, BNative NRisingVoltagePower)]
  -- facade: onBasePower, chainModify(2) when the user has a status other than sleep
  "facade" -> Just [(POwn, EvBasePower, BNative NFacadePower)]
  -- psyblade: onBasePower, chainModify(1.5) under Electric Terrain
  "psyblade" -> Just [(POwn, EvBasePower, BNative NPsybladePower)]
  -- expandingforce: onBasePower, chainModify(1.5) under Psychic Terrain with a grounded user; onModifyMove widens the target
  "expandingforce" -> Just [(POwn, EvBasePower, BNative NExpandingForcePower), (POwn, EvModifyMove, BNative NExpandingForceModifyMove)]
  -- ficklebeam: onBasePower, randomChance(3, 10) -> chainModify(2)
  "ficklebeam" -> Just [(POwn, EvBasePower, BNative NFickleBeamPower)]
  -- venoshock, barbbarrage: onBasePower, chainModify(2) against a poisoned target; brine: against a target at half HP or less
  "venoshock" -> Just [(POwn, EvBasePower, BNative NVenoshockPower)]
  "barbbarrage" -> Just [(POwn, EvBasePower, BNative NVenoshockPower)]
  "brine" -> Just [(POwn, EvBasePower, BNative NBrinePower)]
  -- thunder, hurricane: onModifyMove, accuracy true in rain and 50 in sun; blizzard: true in snow; bleakwindstorm: true in rain
  "thunder" -> Just [(POwn, EvModifyMove, BNative NRainAccuracyModifyMove)]
  "hurricane" -> Just [(POwn, EvModifyMove, BNative NRainAccuracyModifyMove)]
  "blizzard" -> Just [(POwn, EvModifyMove, BNative NBlizzardModifyMove)]
  "bleakwindstorm" -> Just [(POwn, EvModifyMove, BNative NBleakwindModifyMove)]
  -- weatherball: onModifyType by the weather, onModifyMove doubles the power in any weather the move knows
  "weatherball" -> Just [(POwn, EvModifyType, BNative NWeatherBallType), (POwn, EvModifyMove, BNative NWeatherBallModifyMove)]
  -- terrainpulse: onModifyType by the terrain, onModifyMove doubles the power, both for a grounded user
  "terrainpulse" -> Just [(POwn, EvModifyType, BNative NTerrainPulseType), (POwn, EvModifyMove, BNative NTerrainPulseModifyMove)]
  -- ivycudgel: onModifyType by Ogerpon's forms (no form is a species here), onPrepareHit only logs
  "ivycudgel" -> Just [(POwn, EvModifyType, BNative NIvyCudgelType), (POwn, EvPrepareHit, BNative NLogOnly)]
  -- freezedry: onEffectiveness, super effective against Water; flyingpress: adds Flying's effectiveness
  "freezedry" -> Just [(POwn, EvEffectiveness, BNative NFreezeDryEffectiveness)]
  "flyingpress" -> Just [(POwn, EvEffectiveness, BNative NFlyingPressEffectiveness)]
  -- grassyglide: onModifyPriority, +1 under Grassy Terrain for a grounded user
  "grassyglide" -> Just [(POwn, EvModifyPriority, BNative NGrassyGlidePriority)]
  -- ruination, superfang: damageCallback, half of the target's HP (at least 1)
  "ruination" -> Just [(POwn, EvDamageCallback, BNative NHalfHpDamage)]
  "superfang" -> Just [(POwn, EvDamageCallback, BNative NHalfHpDamage)]
  -- moonlight, synthesis, morningsun (data/moves.ts 12252, 18732, 12288): onHit heals by the weather; shoreup (16365): by
  -- sandstorm only
  "moonlight" -> Just [(POwn, EvHit, BNative NWeatherHeal)]
  "synthesis" -> Just [(POwn, EvHit, BNative NWeatherHeal)]
  "morningsun" -> Just [(POwn, EvHit, BNative NWeatherHeal)]
  "shoreup" -> Just [(POwn, EvHit, BNative NShoreUpHeal)]
  -- junglehealing (9863), lunarblessing (10554): onHit heals a quarter, then cures the status
  "junglehealing" -> Just [(POwn, EvHit, BNative NHealCureStatus)]
  "lunarblessing" -> Just [(POwn, EvHit, BNative NHealCureStatus)]
  -- strengthsap (18181): onHit lowers the target's Attack and heals by it
  "strengthsap" -> Just [(POwn, EvHit, BNative NStrengthSapHit)]
  -- painsplit (13133): onHit averages the two HPs
  "painsplit" -> Just [(POwn, EvHit, BNative NPainSplitHit)]
  -- Slice 4c. rapidspin (14699), mortalspin (12324): onAfterHit removes Leech Seed and the user's hazards and trapping;
  -- onAfterSubDamage does the same only while the user has HP, where a hit landed on a Substitute
  "rapidspin" -> Just [(POwn, EvAfterHit, BNative NHazardSweepAfterHit), (POwn, EvAfterSubDamage, BNative NAfterSubDamage)]
  "mortalspin" -> Just [(POwn, EvAfterHit, BNative NHazardSweepAfterHit), (POwn, EvAfterSubDamage, BNative NAfterSubDamage)]
  -- icespinner (9422): onAfterHit clears the terrain
  "icespinner" -> Just [(POwn, EvAfterHit, BNative NIceSpinnerAfterHit), (POwn, EvAfterSubDamage, BNative NAfterSubDamage)]
  -- ceaselessedge (2223), stoneaxe (18076): onAfterHit sets Spikes / Stealth Rock on the foe's side
  "ceaselessedge" -> Just [(POwn, EvAfterHit, BNative NCeaselessEdgeAfterHit), (POwn, EvAfterSubDamage, BNative NAfterSubDamage)]
  "stoneaxe" -> Just [(POwn, EvAfterHit, BNative NStoneAxeAfterHit), (POwn, EvAfterSubDamage, BNative NAfterSubDamage)]
  -- defog (3450), tidyup (19632), clearsmog (2557): onHit
  "defog" -> Just [(POwn, EvHit, BNative NDefogHit)]
  "tidyup" -> Just [(POwn, EvHit, BNative NTidyUpHit)]
  "clearsmog" -> Just [(POwn, EvHit, BNative NClearSmogHit)]
  -- courtchange (3035), haze (8162): onHitField
  "courtchange" -> Just [(POwn, EvHitField, BNative NCourtChangeHitField)]
  "haze" -> Just [(POwn, EvHitField, BNative NHazeHitField)]
  -- brickbreak (1825), psychicfangs (14065): onTryHit shatters the target's screens before the damage; ragingbull (14638) the
  -- same, and its onModifyType changes the type only for Tauros's Paldean forms
  "brickbreak" -> Just [(POwn, EvTryHit, BNative NScreenBreakTryHit)]
  "psychicfangs" -> Just [(POwn, EvTryHit, BNative NScreenBreakTryHit)]
  "ragingbull" -> Just [(POwn, EvTryHit, BNative NScreenBreakTryHit), (POwn, EvModifyType, BNative NRagingBullType)]
  -- Slice 4d. knockoff (9966): onBasePower is 1.5 against a target whose item can be taken, onAfterHit takes it
  "knockoff" -> Just [(POwn, EvBasePower, BNative NKnockOffPower), (POwn, EvAfterHit, BNative NKnockOffAfterHit)]
  -- trick (19872), switcheroo (18651): onTryImmunity (not against Sticky Hold), onHit swaps the items
  "trick" -> Just [(POwn, EvTryImmunity, BNative NTrickTryImmunity), (POwn, EvHit, BNative NTrickHit)]
  "switcheroo" -> Just [(POwn, EvTryImmunity, BNative NTrickTryImmunity), (POwn, EvHit, BNative NTrickHit)]
  -- thief (19309): onAfterHit takes the target's item when the user has none
  "thief" -> Just [(POwn, EvAfterHit, BNative NThiefAfterHit)]
  -- poltergeist (13600): onTry fails against a target with no item, onTryHit only logs
  "poltergeist" -> Just [(POwn, EvTry, BNative NPoltergeistTry), (POwn, EvTryHit, BNative NLogOnly)]
  -- auroraveil (830): onTry needs snow (its condition is in 'fieldCondBodies')
  "auroraveil" -> Just [(POwn, EvTry, BNative NAuroraVeilTry)]
  -- Slice 6a. teleport (19183): onTry fails it when the side cannot switch; partingshot (13179): onHit drops Attack and Special
  -- Attack and deletes the move's selfSwitch when neither dropped; chillyreception (2408): priorityChargeCallback adds its volatile
  -- (the volatile's onBeforeMove only logs)
  "teleport" -> Just [(POwn, EvTry, BNative NTeleportTry)]
  "partingshot" -> Just [(POwn, EvHit, BNative NPartingShotHit)]
  "chillyreception" -> Just [(POwn, EvPriorityChargeCallback, BNative NChillyReceptionCharge)]
  -- Slice 7a. protect (13966), spikyshield (17539), banefulbunker (985), endure (4811): onPrepareHit is `!!this.queue.willAct() &&
  -- this.runEvent('StallMove', pokemon)`, onHit adds the stall volatile (the move's own volatile is added before it)
  "protect" -> Just stallingMove
  "spikyshield" -> Just stallingMove
  "banefulbunker" -> Just stallingMove
  "endure" -> Just stallingMove
  -- slice 7d. suckerpunch (18403), thunderclap (19504): onTry reads queue.willMove(target)
  "suckerpunch" -> Just [(POwn, EvTry, BNative NSuckerPunchTry)]
  "thunderclap" -> Just [(POwn, EvTry, BNative NSuckerPunchTry)]
  -- slice 7b. substitute (18311): onTryHit fails it, onHit pays the quarter (its volatile is added just before, runMoveEffects)
  "substitute" -> Just [(POwn, EvTryHit, BNative NSubstituteTryHit), (POwn, EvHit, BNative NSubstituteHit)]
  _ -> Nothing
  where
    -- slice 7a: the handlers the four moves share
    stallingMove = [(POwn, EvPrepareHit, BNative NStallPrepareHit), (POwn, EvHit, BNative NStallHit)]

-- | slice 7e: the handlers every Choice item has (data/items.ts 971-1040): onStart, onModifyMove, and the 1.5 stat boost of its stat.
choiceItem :: EventId -> [(Prefix, EventId, Body)]
choiceItem statEvent =
  [ (POwn, EvStart, BNative NChoiceItemStart)
  , (POwn, EvModifyMove, BNative NChoiceItemModifyMove)
  , (POwn, statEvent, BRule (RChainModify 3 2 [])) ]
