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
  -- unburden: onAfterUseItem adds the volatile, onTakeItem too (no supported move takes items), onEnd removes it
  "unburden" ->
    Just [ (POwn, EvAfterUseItem, BNative NUnburdenAfterUseItem), (POwn, EvTakeItem, BNative NInertTakeItem)
         , (POwn, EvEnd, BNative NUnburdenEnd) ]
  -- toxicdebris: onDamagingHit, a Physical move sets Toxic Spikes on the attacker's side
  "toxicdebris" -> Just [(POwn, EvDamagingHit, BNative NToxicDebris)]
  -- technician: onBasePower, basePower after the accumulated modifier <= 60 -> chainModify(1.5)
  "technician" -> Just [(POwn, EvBasePower, BNative NTechnician)]
  -- filter, solidrock, prismarmor: onSourceModifyDamage, `typeMod > 0` -> chainModify(0.75)
  "filter" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  "solidrock" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  "prismarmor" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  _ -> Nothing
  where
    speed ws = do
      ids <- traverse wx ws
      Just [(POwn, EvModifySpe, BRule (RChainModify 2 1 [CWeather ids]))]
    pinch t = do
      i <- ty t
      let r = BRule (RChainModify 3 2 [CMoveType i, CHolderHpAtMost 1 3])
      Just [(POwn, EvModifyAtk, r), (POwn, EvModifySpA, r)]

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
         , (POwn, EvTakeItem, BNative NInertTakeItem) ]
  -- electricseed, grassyseed, mistyseed, psychicseed: onStart and onTerrainChange call pokemon.useItem() under their
  -- terrain (onStart also needs `!pokemon.ignoringItem()`, always true here); the boosts are the item's `boosts`
  "electricseed" -> seed "electricterrain"
  "grassyseed" -> seed "grassyterrain"
  "mistyseed" -> seed "mistyterrain"
  "psychicseed" -> seed "psychicterrain"
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
-- Not implemented: mistyexplosion has the @onBasePower@ of expandingforce under Misty Terrain, but its @selfdestruct@
-- keeps the move rejected, so a body would run on no path. Payback, triplekick and tripleaxel (per-turn history,
-- per-hit accuracy), the healing and stat-override moves and knockoff are later phases.
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
  _ -> Nothing
