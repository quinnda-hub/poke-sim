{-# LANGUAGE OverloadedStrings #-}
-- | The hand-written handler bodies of the abilities and items the engine supports (docs/handlers.md section
-- 3). The metadata (order, priority, subOrder) and the list of handlers come from the export
-- (@abilities.json@, @items.json@); only the bodies are written here, each from its Showdown source
-- (data/abilities.ts, data/items.ts).
--
-- An ability or item is supported only when it is in these tables and the export lists no handler without a
-- body here ('Engine.Dex').
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
module Engine.Effects
  ( abilityBodies
  , itemBodies
  ) where

import qualified Data.Text as T

import Engine.Event

-- | The bodies of an ability, or 'Nothing' when the engine does not implement it. The argument resolves a
-- type name to its id.
abilityBodies :: (T.Text -> Maybe Int) -> T.Text -> Maybe [(Prefix, EventId, Body)]
abilityBodies ty name = case name of
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
  -- technician: onBasePower, basePower after the accumulated modifier <= 60 -> chainModify(1.5)
  "technician" -> Just [(POwn, EvBasePower, BNative NTechnician)]
  -- filter, solidrock, prismarmor: onSourceModifyDamage, `typeMod > 0` -> chainModify(0.75)
  "filter" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  "solidrock" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  "prismarmor" -> Just [(PSource, EvModifyDamage, BNative NFilterLike)]
  _ -> Nothing
  where
    pinch t = do
      i <- ty t
      let r = BRule (RChainModify 3 2 [CMoveType i, CHolderHpAtMost 1 3])
      Just [(POwn, EvModifyAtk, r), (POwn, EvModifySpA, r)]

-- | The bodies of an item, or 'Nothing' when the engine does not implement it.
itemBodies :: (T.Text -> Maybe Int) -> T.Text -> Maybe [(Prefix, EventId, Body)]
itemBodies ty name = case name of
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
  _ -> case lookup name typeItems of
    -- onBasePower, `move.type === T` -> chainModify([4915, 4096]) (data/items.ts; each body checked against the
    -- source: the plain type check and nothing else)
    Just t -> do
      i <- ty t
      Just [(POwn, EvBasePower, BRule (RChainModify 4915 4096 [CMoveType i]))]
    Nothing -> Nothing

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
