{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Shared machinery for the property suite: random legal teams, random playouts under the sampling
-- interpreter, and invariant checks over a battle state. Nothing here knows an expected battle outcome.
module Playout
  ( genTeams
  , genPivotTeams
  , genStatTeams
  , genGuardTeams -- slice 8c
  -- slice 8d
  , genAbilityTeams
  , playChecked
  , Seen (..)
  , advanceWhile
  , startBattle
  , checkBattle
  , Counter
  , runCounter
  , stepCap
  ) where

import Data.Bits (complement, (.&.))
import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as M
import Data.List (nub, sort)
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Engine
import Engine.Chance (Sample)
import Engine.Dex (Condition (..), Dex (..), MoveData (..), SpeciesData (..), abilityId, itemId, noType, speciesData, supportedAbilities, supportedItems, supportedMoves, supportedSpecies)
import Engine.Damage (monTypes)
import Engine.Event (CondId (..))
import Engine.State

-- | Decision steps after which a playout counts as not having ended.
stepCap :: Int
stepCap = 5000

-- | A pair of random legal teams (1 to 6 Pokemon each) built through 'parseTeam', the path the trace teams
-- and the benchmark use.
genTeams :: Dex -> Gen (Team, Team)
genTeams dx = genTeamsWith dx False False

-- | 'genTeams' weighted toward Regenerator, Natural Cure, Eject Button and Red Card (slice 6c) and the status moves that give Natural Cure
-- something to cure, and toward the moves that switch their user out (U-turn, Flip Turn, Volt Switch, Teleport, Parting Shot, Chilly
-- Reception) and the moves that force the target out (Roar, Whirlwind, Dragon Tail, Circle Throw), the hazards they drag into, Rocky
-- Helmet and Shedinja (1 HP: a contact pivot into a Rocky Helmet faints its user after the flag is set), so that the playouts reach the
-- mid-turn switch requests and the drags that a uniform draw over every move seldom does.
genPivotTeams :: Dex -> Gen (Team, Team)
genPivotTeams dx = genTeamsWith dx True False

-- | slice 8a: 'genPivotTeams' with a third of the sets holding one of the stage and switch-in abilities (Intimidate, Dauntless Shield,
-- Defiant, Contrary, Clear Body, Guard Dog, Inner Focus, Big Pecks, Mirror Armor, Keen Eye, Weak Armor, Justified, Gooey, Speed Boost)
-- and the moves that change stages or that those abilities answer (self-drops, boosts, contact and Dark moves, Sticky Web), so that the
-- boost events meet their handlers. A separate generator, so that the pivot teams' own coverage shares are unchanged.
genStatTeams :: Dex -> Gen (Team, Team)
genStatTeams dx = genTeamsWith dx True True

-- | slice 8c: 'genPivotTeams' with the immunity and guard abilities (Good as Gold, Water Absorb, Levitate, Sturdy, Magic Guard, Poison Heal,
-- Sap Sipper, Soundproof, Bulletproof, Sweet Veil, Shell Armor) on a share of the sets, the items that hurt or heal their holder (the orbs,
-- Black Sludge, Life Orb, Rocky Helmet, Leftovers), and the moves those abilities meet (a Water, Grass, Ground, sound and bullet move, the status
-- moves that target a foe, hazards, a sandstorm and a move that crits often), so that the playouts reach the damage events and the blocked hits
-- that a uniform draw over every ability and item seldom does. The other generators draw as they did before.
genGuardTeams :: Dex -> Gen (Team, Team)
genGuardTeams dx = genTeamsK dx True False True False

genTeamsWith :: Dex -> Bool -> Bool -> Gen (Team, Team)
genTeamsWith dx pivotHeavy statHeavy = genTeamsK dx pivotHeavy statHeavy False False

-- | slice 8d: teams of one to three Pokemon, mostly level 100, weighted toward the on-hit and after-move abilities (Flame Body, Static, Rough
-- Skin, Toxic Chain, Synchronize, Magician, Pickpocket, Berserk, Moxie, Shield Dust, Bad Dreams) and the moves and items they meet (contact and
-- multi-hit moves, burns, paralyses and poisons, the item moves, Rocky Helmet, Life Orb, the Choice items, an empty hand): their events are
-- rare in uniform random play.
genAbilityTeams :: Dex -> Gen (Team, Team)
genAbilityTeams dx = genTeamsK dx False False False True

-- | 'genTeamsWith', with the slice 8c share of guard abilities, items and moves when 'guardHeavy' is set and the slice 8d share of on-hit
-- abilities when 'abilityHeavy' is.
genTeamsK :: Dex -> Bool -> Bool -> Bool -> Bool -> Gen (Team, Team)
genTeamsK dx pivotHeavy statHeavy guardHeavy abilityHeavy = (,) <$> genTeam <*> genTeam
  where
    -- slice 8c (used only when guardHeavy)
    guardAbilities = [a | a <- ["goodasgold", "waterabsorb", "levitate", "sturdy", "magicguard", "poisonheal", "sapsipper", "soundproof", "bulletproof", "sweetveil", "shellarmor"], a `elem` abilities]
    orbItems = [i | i <- ["toxicorb", "flameorb", "blacksludge", "lifeorb", "rockyhelmet", "leftovers"], i `elem` items]
    guardKit = [m | m <- [ "surf", "scald", "energyball", "gigadrain", "earthquake", "highhorsepower", "hypervoice", "boomburst", "shadowball", "sludgebomb", "seedbomb"
                         , "thunderwave", "toxic", "willowisp", "taunt", "encore", "trick", "partingshot", "stealthrock", "spikes", "sandstorm", "stoneedge", "crosschop"
                         , "closecombat", "bulletseed", "iciclespear", "rockblast", "terrainpulse", "grassyterrain", "electricterrain"], m `elem` moves]
    ability8c = if null guardAbilities then pickAbility else Gen.frequency [(8, Gen.element guardAbilities), (3, pickAbility)]
    item8c = if null orbItems then pickItem else Gen.frequency [(4, Gen.element orbItems), (3, pickItem)]
    move8c = if null guardKit then pickMove else Gen.frequency [(5, Gen.element guardKit), (3, pickMove)]
    species = supportedSpecies dx
    moves = supportedMoves dx
    pivots = [m | m <- ["uturn", "flipturn", "voltswitch", "teleport", "partingshot", "chillyreception"], m `elem` moves]
    -- (slice 6d: Sticky Web and the moves that drop a stat, for Eject Pack: self-drops, secondary drops, Defog and Strength Sap)
    dragKit = [m | m <- ["roar", "whirlwind", "dragontail", "circlethrow", "stealthrock", "spikes", "toxicspikes", "toxic", "willowisp", "thunderwave", "spore"
                        , "stickyweb", "closecombat", "dracometeor", "overheat", "leafstorm", "moonblast", "snarl", "icywind", "defog", "strengthsap"
                        -- slice 7e: the item moves that meet a Choice item
                        , "trick", "switcheroo", "thief", "knockoff"], m `elem` moves]
    pickSpecies
      | pivotHeavy && "shedinja" `elem` species = Gen.frequency [(3, pure "shedinja"), (7, Gen.element species)]
      | otherwise = Gen.element species
    -- slice 7a: the protect family, which the heavy teams carry on many sets (a repeated protect move climbs the stall counter and fails)
    protectKit = [m | m <- ["protect", "spikyshield", "banefulbunker", "endure"], m `elem` moves]
    -- slice 7d: Sucker Punch and Thunderclap, against a mix of status and attacking moves
    punches = [m | m <- ["suckerpunch", "thunderclap"], m `elem` moves]
    -- slice 7c: Encore and Taunt, with the status moves they stop and the pivots that switch an encored or taunted Pokemon out
    encoreKit = [m | m <- ["encore", "taunt"], m `elem` moves]
    -- the step-4 kits (slice 7): each one that is supported joins the pivot-heavy mix with weight 3
    -- slice 7b: Substitute with the moves that meet one (multi-hit, recoil, drain, secondaries and self drops, pivots and forcers are in the others)
    substituteKit = [m | m <- ["substitute", "substitute", "armthrust", "doubleedge", "drainpunch", "closecombat", "rapidspin"], m `elem` moves]
    step4Kits = [(3, Gen.element k) | k <- [protectKit, punches, encoreKit], not (null k)] ++ [(5, Gen.element substituteKit) | not (null substituteKit)] -- (slice 7b: its own weight)
      -- slice 7f: a draw over every kit's moves, so that the kits meet on one set (Encore with a Choice item and Taunt, Substitute with a protect move, ...)
      ++ [(6, Gen.element (concat [protectKit, punches, encoreKit, substituteKit])) | not (null (concat [protectKit, punches, encoreKit, substituteKit]))]
      -- slice 8a: the moves that change stages (self-drops, boosts, drops on the foe) and the contact and Dark moves that Weak Armor, Gooey and Justified answer
      ++ [(8, Gen.element statKit) | statHeavy, not (null statKit)]
      -- slice 8e: the moves that meet Pressure (a move on each kind of target), Prankster (status moves into Dark types), Liquid Voice (sound
      -- moves) and Infiltrator (Substitute and the screens)
      ++ [(4, Gen.element pipelineKit) | not (null pipelineKit)]
    -- slice 8a
    statKit = [m | m <- ["closecombat", "superpower", "dracometeor", "swordsdance", "dragondance", "bulkup", "calmmind", "nastyplot", "snarl", "icywind", "growl", "tailwhip"
                        , "stickyweb", "partingshot", "knockoff", "suckerpunch", "crunch", "bite", "darkpulse", "tackle", "doubleironbash", "bulletseed", "uturn", "roar", "dragontail"
                        , "substitute", "sandattack", "doubleteam", "minimize", "agility", "shellsmash", "curse"], m `elem` moves]
    -- slice 8e
    pipelineKit = [m | m <- ["thunderwave", "willowisp", "toxic", "taunt", "hypervoice", "boomburst", "defog", "reflect", "lightscreen", "auroraveil", "substitute"
                            , "tailwind", "haze", "stealthrock", "spikes", "toxicspikes", "swordsdance", "psychicterrain", "trickroom", "earthquake", "surf"], m `elem` moves]
    pipelineAbilities = [a | a <- ["pressure", "prankster", "libero", "protean", "liquidvoice", "infiltrator"], a `elem` abilities]
    pickMove
      | abilityHeavy && not (null kit8d) = Gen.frequency [(5, Gen.element kit8d), (3, Gen.element moves)]
      | pivotHeavy && not (null step4Kits) && not (null pivots) && not (null dragKit) =
          Gen.frequency ([(3, Gen.element pivots), (3, Gen.element dragKit), (2, Gen.element moves)] ++ step4Kits)
      | pivotHeavy && not (null pivots) && not (null dragKit) = Gen.frequency [(3, Gen.element pivots), (3, Gen.element dragKit), (2, Gen.element moves)]
      | pivotHeavy && not (null pivots) = Gen.frequency [(3, Gen.element pivots), (2, Gen.element moves)]
      | otherwise = Gen.element moves
    -- slice 8d: the moves the abilities of the phase meet
    kit8d = [m | m <- [ "doublehit", "armthrust", "furyswipes", "bulletseed", "pinmissile", "scald", "nuzzle", "flamethrower", "thunderbolt", "sludgebomb"
                      , "toxic", "willowisp", "thunderwave", "toxicspikes", "knockoff", "thief", "trick", "uturn", "recover", "closecombat", "earthquake"
                      , "tackle", "bodyslam", "suckerpunch" ], m `elem` moves]
    -- Rocky Helmet, and the two items that switch their holder out or force the attacker out (slice 6c)
    pickItem
      | abilityHeavy = Gen.frequency [(3, Gen.element (filter (`elem` items) ["rockyhelmet", "lifeorb", "leftovers", "choiceband", "choicescarf", "flameorb", "toxicorb", ""])), (2, Gen.element items)]
      | pivotHeavy && not (null switchItems) && not (null choiceItems) =
          Gen.frequency [(8, Gen.element switchItems), (4, Gen.element ("rockyhelmet" : switchItems)), (4, Gen.element items), (4, Gen.element choiceItems), (2, Gen.element ("assaultvest" : choiceItems))]
      | pivotHeavy && not (null switchItems) = Gen.frequency [(4, Gen.element switchItems), (2, Gen.element ("rockyhelmet" : switchItems)), (2, Gen.element items)]
      | pivotHeavy && "rockyhelmet" `elem` items = Gen.frequency [(2, pure "rockyhelmet"), (3, Gen.element items)]
      | otherwise = Gen.element items
    -- (slice 7e: the Choice items join them, for a lock that Trick, Knock Off, Thief and a pivot meet)
    switchItems = [i | i <- ["ejectbutton", "redcard", "ejectpack"], i `elem` items]
    choiceItems = [i | i <- ["choiceband", "choicespecs", "choicescarf"], i `elem` items]
    -- Regenerator and Natural Cure act at every switch-out
    pickAbility
      -- slice 8a: the stage and switch-in abilities, on most sets of the 'genStatTeams' teams
      | statHeavy && not (null statAbilities) = Gen.frequency [(1, Gen.element abilities), (3, Gen.element statAbilities)]
      | abilityHeavy && not (null abilities8d) = Gen.frequency [(5, Gen.element abilities8d), (1, Gen.element abilities)]
      -- slice 8b: the damage abilities join the pivot-heavy mix, where Supreme Overlord meets switch-ins after faints and Flash Fire meets
      -- the switch-outs that end its volatile
      -- slice 8e: the move-pipeline abilities (Pressure, Prankster, Libero, Protean, Liquid Voice, Infiltrator) join the same mix, with weight 4
      -- STACK NOTE: 8b and 8e each added a guard for this mix with its own kit; stacked, the first would hide the second, so this one takes both
      | pivotHeavy && not (null switchAbilities) && not (null damageAbilities) && not (null pipelineAbilities) =
          Gen.frequency [(3, Gen.element switchAbilities), (3, Gen.element damageAbilities), (4, Gen.element pipelineAbilities), (3, Gen.element abilities)]
      | pivotHeavy && not (null switchAbilities) && not (null damageAbilities) = Gen.frequency [(3, Gen.element switchAbilities), (3, Gen.element damageAbilities), (3, Gen.element abilities)]
      | pivotHeavy && not (null switchAbilities) && not (null pipelineAbilities) =
          Gen.frequency [(3, Gen.element switchAbilities), (4, Gen.element pipelineAbilities), (3, Gen.element abilities)]
      | pivotHeavy && not (null switchAbilities) = Gen.frequency [(3, Gen.element switchAbilities), (3, Gen.element abilities)]
      | otherwise = Gen.element abilities
    -- slice 8d
    abilities8d = [a | a <- ["flamebody", "static", "roughskin", "toxicchain", "synchronize", "magician", "pickpocket", "berserk", "moxie", "shielddust", "baddreams"], a `elem` abilities]
    switchAbilities = [a | a <- ["regenerator", "naturalcure"], a `elem` abilities]
    -- slice 8a
    statAbilities = [a | a <- ["intimidate", "dauntlessshield", "defiant", "contrary", "clearbody", "guarddog", "innerfocus", "bigpecks", "mirrorarmor", "keeneye", "weakarmor", "justified", "gooey", "speedboost"], a `elem` abilities]
    -- slice 8b
    damageAbilities =
      [ a | a <- ["multiscale", "sharpness", "tintedlens", "guts", "transistor", "sniper", "heatproof", "purifyingsalt", "waterbubble", "flashfire", "supremeoverlord", "vesselofruin", "unaware"]
      , a `elem` abilities ]
    -- every ability and item the engine accepts ("noability" and the empty item among them)
    abilities = supportedAbilities dx
    items = supportedItems dx
    natures = M.keys (dxNatureIx dx)
    -- every real type, and Stellar, which a quarter of the sets take (its rules are the rarest to reach at random)
    teraTypes = [t | t <- V.toList (dxTypeNames dx), t `notElem` ["Stellar", "???"]]
    genTeam = do
      n <- if abilityHeavy then Gen.int (Range.linear 1 3) else Gen.int (Range.linear 1 6)
      sets <- Gen.list (Range.singleton n) genSet
      either (\e -> error ("genTeams: parseTeam rejected a generated team: " ++ e)) pure
        (parseTeam dx (toJSONArray sets))
    toJSONArray = Array . V.fromList
    genSet = do
      sp <- pickSpecies
      ms <- nub <$> Gen.list (Range.linear 1 4) (if guardHeavy then move8c else pickMove)
      nat <- Gen.element natures
      -- (slice 8d: mostly level 100 for the ability-heavy teams, so that a Moxie or Berserk holder's hits take foes down)
      lvl <- if abilityHeavy then Gen.frequency [(3, pure 100), (1, Gen.int (Range.linear 1 100))] else Gen.int (Range.linear 1 100)
      evs <- Gen.list (Range.singleton 6) (Gen.int (Range.linear 0 252))
      ab <- if guardHeavy then ability8c else pickAbility
      it <- if guardHeavy then item8c else pickItem
      tera <- Gen.frequency [(3, Gen.element teraTypes), (1, pure "Stellar")]
      pure (object
        [ "species" .= sp, "teraType" .= tera, "moves" .= ms, "nature" .= nat, "level" .= lvl, "ability" .= ab, "item" .= it
        , "evs" .= object [K.fromText k .= v | (k, v) <- zip ["hp", "atk", "def", "spa", "spd", "spe"] evs]
        ])

-- | slice 7e: a Pokemon with a choicelock volatile and a Choice item (Showdown: the lock is live; a lock with no Choice item is stale and is
-- dropped by the next BeforeMove or DisableMove event).
choiceLocked :: Dex -> Mon -> Bool
choiceLocked dx m = any (\(Volatile i _) -> i == fromEnum CChoiceLock) (SV.toList (monVolatiles m)) && itemId dx (monItem m) `elem` ["choiceband", "choicespecs", "choicescarf"]

-- | slice 7f: the disabling effects on a Pokemon's move slots (see 'checkBattle'): Encore (its encored move, when it knows it), Taunt, a live
-- Choice lock (its locked moves) and an Assault Vest.
disablers :: Dex -> Mon -> (Maybe Int, Bool, [Int], Bool)
disablers dx m =
  ( case [esA st | Volatile i st <- vols, i == fromEnum CEncore, any ((== esA st) . msMove) (SV.toList (monMoves m))] of e : _ -> Just e; [] -> Nothing
  , any (\(Volatile i _) -> i == fromEnum CTaunt) vols
  , [esA st | choiceLocked dx m, Volatile i st <- vols, i == fromEnum CChoiceLock]
  , monItem m /= noItem && itemId dx (monItem m) == "assaultvest"
  )
  where vols = SV.toList (monVolatiles m)

-- | Whether a slot has PP and every disabling effect allows it.
slotAllowed :: Dex -> Mon -> MoveSlot -> Bool
slotAllowed dx m ms = msPP ms > 0 && maybe True (== msMove ms) encored && not (taunted && status && mvId mv /= "mefirst")
  && (null locks || msMove ms `elem` locks) && not (vest && status)
  where
    (encored, taunted, locks, vest) = disablers dx m
    mv = dxMoves dx V.! msMove ms
    status = mvStatus mv

-- | slice 7f: a move request with an active Pokemon under at least two of the disabling effects.
multiDisabled :: Dex -> Battle -> Int
multiDisabled dx b = case request b of
  Awaiting MoveRequest _ _ -> length [() | p <- [P1, P2], sHasActive (sideOf p b), let (e, t, l, v) = disablers dx (activeMon p b), length (filter id [e /= Nothing, t, not (null l), v]) >= 2]
  _ -> 0

-- | A battle at team preview.
startBattle :: Dex -> Team -> Team -> Sample Battle
startBattle dx = newBattle dx gen9ou

-- | A uniformly random legal choice for each side being asked.
pickBoth :: Battle -> Bool -> Bool -> Sample (Maybe Choice, Maybe Choice)
pickBoth b a1 a2 = (,) <$> pick P1 a1 <*> pick P2 a2
  where
    pick _ False = pure Nothing
    pick p True = do
      let cs = legalChoices b p
      i <- roll 0 (length cs)
      pure (case drop i cs of (c : _) -> Just c; [] -> Nothing)

-- | What a playout went through, counted by 'checkStep' (the properties' coverage statements read it).
data Seen = Seen
  { seenDrags :: !Int
  -- ^ forced switches (drags)
  , seenRegen :: !Int
  -- ^ a Regenerator holder that was below max HP and chose to switch out
  , seenCure :: !Int
  -- ^ a Natural Cure holder with a status that left the field
  , seenEject :: !Int
  -- ^ an Eject Button used up
  , seenRedCard :: !Int
  -- ^ a Red Card used up
  , seenPack :: !Int
  -- ^ an Eject Pack used up
  , seenPackIn :: !Int
  -- ^ an Eject Pack used up by a Pokemon in the step that switched it in (Sticky Web)
  -- slice 7a
  , seenStall :: !Int
  -- ^ a Pokemon holding a @stall@ volatile after a step: a protect move succeeded
  , seenStallClimb :: !Int
  -- ^ a @stall@ volatile with a counter of 9 or more: a protect move succeeded again with the volatile in place
  -- slice 7e
  , seenLocked :: !Int
  -- ^ a Pokemon with a live choicelock at a move request
  , seenLockStruggle :: !Int
  -- ^ one of those with no usable move left (the locked move out of PP): Struggle
  -- slice 7c
  , seenEncore :: !Int
  -- ^ a step that ends at a move request with an active Pokemon encored
  , seenTaunt :: !Int
  -- ^ the same with an active Pokemon taunted
  -- slice 7b
  , seenSub :: !Int
  -- ^ a Pokemon in play before and after the step that had no Substitute before and has one after
  , seenSubHit :: !Int
  -- ^ a Pokemon in play before and after the step with a Substitute at both ends, whose HP went down
  -- slice 7f
  , seenMultiDisable :: !Int
  -- ^ a move request with an active Pokemon under two or more of the disabling effects (encore, taunt, choicelock, Assault Vest)
  -- slice 8a
  , seenShieldBoost :: !Int
  -- ^ a Pokemon whose Dauntless Shield boost was spent during the step
  , seenIntimidateIn :: !Int
  -- ^ an Intimidate holder that switched in (was benched before the step, is in play with HP after it)
  , seenDropAbility :: !Int
  -- ^ a Pokemon in play before and after the step, holding Clear Body, Guard Dog, Inner Focus, Big Pecks, Mirror Armor, Defiant or Contrary, that ended with a stage other than zero
  -- slice 8c
  , seenGuarded :: !Int
  -- ^ a Magic Guard holder with burn, poison or toxic that went through a step in play
  , seenAbsorbed :: !Int
  -- ^ a Water Absorb holder that is in play before and after the step with more HP
  -- slice 8d
  , seenMoxie :: !Int
  -- ^ a Moxie holder in play before and after the step, with HP after it, whose Attack stage rose in a step where the foe in play before it fainted
  , seenThief :: !Int
  -- ^ a Magician or Pickpocket holder with no item before the step and an item after it
  , seenBerserk :: !Int
  -- ^ a Berserk holder in play before and after the step, with HP after it, that lost HP and whose Special Attack stage rose
  -- slice 8e
  , seenPressure :: !Int
  -- ^ a move slot that lost 2 PP in one step (its move's use, and a foe's Pressure)
  , seenTypeChange :: !Int
  -- ^ an active Libero or Protean holder that has used the ability after a step
  }
  deriving (Eq, Show)

-- (by field name, so that each phase's fields join in a block of their own)
instance Semigroup Seen where
  x <> y =
    Seen
      { seenDrags = f seenDrags
      , seenRegen = f seenRegen
      , seenCure = f seenCure
      , seenEject = f seenEject
      , seenRedCard = f seenRedCard
      , seenPack = f seenPack
      , seenPackIn = f seenPackIn
      -- slice 7a
      , seenStall = f seenStall
      , seenStallClimb = f seenStallClimb
      -- slice 7e
      , seenLocked = f seenLocked
      , seenLockStruggle = f seenLockStruggle
      -- slice 7c
      , seenEncore = f seenEncore
      , seenTaunt = f seenTaunt
      -- slice 7b
      , seenSub = f seenSub
      , seenSubHit = f seenSubHit
      -- slice 7f
      , seenMultiDisable = f seenMultiDisable
      -- slice 8a
      , seenShieldBoost = f seenShieldBoost
      , seenIntimidateIn = f seenIntimidateIn
      , seenDropAbility = f seenDropAbility
      -- slice 8c
      , seenGuarded = f seenGuarded
      , seenAbsorbed = f seenAbsorbed
      -- slice 8d
      , seenMoxie = f seenMoxie
      , seenThief = f seenThief
      , seenBerserk = f seenBerserk
      -- slice 8e
      , seenPressure = f seenPressure
      , seenTypeChange = f seenTypeChange
      }
    where
      f g = g x + g y

instance Monoid Seen where
  mempty =
    Seen
      { seenDrags = 0
      , seenRegen = 0
      , seenCure = 0
      , seenEject = 0
      , seenRedCard = 0
      , seenPack = 0
      , seenPackIn = 0
      -- slice 7a
      , seenStall = 0
      , seenStallClimb = 0
      -- slice 7e
      , seenLocked = 0
      , seenLockStruggle = 0
      -- slice 7c
      , seenEncore = 0
      , seenTaunt = 0
      -- slice 7b
      , seenSub = 0
      , seenSubHit = 0
      -- slice 7f
      , seenMultiDisable = 0
      -- slice 8a
      , seenShieldBoost = 0
      , seenIntimidateIn = 0
      , seenDropAbility = 0
      -- slice 8c
      , seenGuarded = 0
      , seenAbsorbed = 0
      -- slice 8d
      , seenMoxie = 0
      , seenThief = 0
      , seenBerserk = 0
      -- slice 8e
      , seenPressure = 0
      , seenTypeChange = 0
      }

-- | Play with random legal choices, checking every state on the way, until the battle ends (or the cap).
--
-- The result carries what the playout went through ('Seen'), which 'checkStep' finds.
playChecked :: Dex -> Team -> Team -> Sample (Either String (Battle, Seen))
playChecked dx t1 t2 = startBattle dx t1 t2 >>= go 0 mempty
  where
    go :: Int -> Seen -> Battle -> Sample (Either String (Battle, Seen))
    go n seen b
      | n > stepCap = pure (Left ("no result within " ++ show stepCap ++ " decision steps"))
      | Left e <- checkBattle dx b = pure (Left e)
      | otherwise = case request b of
          Finished _ -> pure (Right (b, seen))
          Awaiting _ a1 a2 -> do
            cs <- pickBoth b a1 a2
            r <- step dx b cs
            case r of
              Left e -> pure (Left ("step rejected legal choices " ++ show cs ++ ": " ++ show e))
              Right b' -> either (pure . Left) (\d -> go (n + 1) (seen <> d) b') (checkStep dx b b' cs)

-- | What one decision step may do to a Stellar Pokemon's @stellarBoostedTypes@ (pokemon.ts 504 sets it in the constructor
-- and nothing ever clears or removes from it, so a switch or a faint keeps it): it never loses a type, and a Pokemon that
-- is on the bench before and after the step does not change it.
--
-- Switching (slice 6a): a step that ends at a switch request is in the middle of a turn (or at its end, before @endTurn@), so the
-- turn counter has not advanced; and a Pokemon that was flagged by a self-switch move (alive, 'SwitchMove') and whose side then
-- chose its replacement is benched afterwards with no queued action left, except a @runSwitch@ (@queue.cancelAction@ keeps those,
-- and none can be its own).
--
-- Forced switches (slice 6b): a side whose active Pokemon, after the step, is not the one its choice put there (the one it had, or the
-- target of its @switch@) was dragged out by a Roar, Whirlwind, Dragon Tail or Circle Throw, which no other effect does yet. The one it
-- had is then benched, without a flag or queued action (@queue.cancelAction@), and the one that replaced it was on the bench before the
-- step (or the one the side's own switch benched during it), unfainted and not active (@getRandomSwitchable@ lists only those).
--
-- Switch-out effects (slice 6c), for each Pokemon in the active slot before the step with HP: a Regenerator holder that chose to
-- switch and is benched afterwards has healed exactly a third of its max HP, up to the maximum (nothing hurts it between the
-- choice, which runs first, and the bench); a Natural Cure holder that is benched afterwards with HP, by any path (a choice, a pivot,
-- an item, a drag), has no status; a fainted Pokemon never regains HP. Eject Button and Red Card, in a step where no Pokemon in play
-- knows an item-removal move (Knock Off, Thief, Trick, Switcheroo): an Eject Button that is gone left its holder flagged and still
-- in the active slot (the step ends at the switch request the item made), or the holder fainted; a Red Card that is gone has
-- forced the attacker out, so the attacker is no longer active, in a step where no Pokemon in play knows a forcing move that
-- could bring it back.
-- Returns what the step went through.
checkStep :: Dex -> Battle -> Battle -> (Maybe Choice, Maybe Choice) -> Either String Seen
checkStep dx b b' (c1, c2) = (mappend (stallSeen <> seen8d <> pressureSeen) <$>) $ pressureOk >> mapM_ one [(p, i) | (p, side) <- [(P1, bP1), (P2, bP2)], i <- [0 .. SV.length (sMons (side b)) - 1]] >> turnOk >> itemsNeverCreated >> (((,) <$> drag (P1, c1) <*> drag (P2, c2)) >>= \ds -> pivotsOk ds >> switchOuts >>= \so -> pure (so <> mempty {seenDrags = length (filter id [fst ds, snd ds]), seenEncore = volatileAtRequest CEncore, seenTaunt = volatileAtRequest CTaunt, seenLocked = length lockedNow, seenLockStruggle = length (filter (mustStruggle b) lockedNow), seenMultiDisable = multiDisabled dx b'}))
  where
    -- slice 7c: the step ended at a move request with an active Pokemon that has the volatile
    volatileAtRequest c = case request b' of
      Awaiting MoveRequest _ _ | or [fromEnum c `elem` [i | Volatile i _ <- SV.toList (monVolatiles (monOf b' (activeActor p b')))] | p <- [P1, P2], sHasActive (sideOf p b')] -> 1
      _ -> 0
    chose p = if p == P1 then c1 else c2
    -- slice 8e: Pressure. A move slot loses PP only by a use of its move (1), and a foe's Pressure adds 1 (battle-actions.ts 540-551): no
    -- slot loses more than 2 in a step (a Pokemon moves once; a second Pressure holder cannot exist in singles), none gains PP, and a slot
    -- that loses 2 has a Pressure holder on the other side
    ppDrops = [ (p, msPP ms - msPP ms')
              | p <- [P1, P2]
              , (m0, m1) <- zip (SV.toList (sMons (sideOf p b))) (SV.toList (sMons (sideOf p b')))
              , (ms, ms') <- zip (SV.toList (monMoves m0)) (SV.toList (monMoves m1)) ]
    foeHasPressure p = any (\m -> abilityIs "pressure" m) (SV.toList (sMons (sideOf (otherP p) b)))
    pressureOk = case [(p, d) | (p, d) <- ppDrops, d < 0 || d > 2 || (d == 2 && not (foeHasPressure p))] of
      [] -> Right ()
      (p, d) : _ -> Left (show p ++ ": a move slot lost " ++ show d ++ " PP in one step (more than 2, a gain, or 2 with no Pressure holder on the other side)")
    pressureSeen = mempty {seenPressure = length [() | (_, 2) <- ppDrops], seenTypeChange = length [() | p <- [P1, P2], sHasActive (sideOf p b'), let m = activeMon p b', abilityIs "libero" m || abilityIs "protean" m, esA (monAbilityState m) == 1]}
    -- slice 7a: the stall volatiles after the step (the protect volatiles themselves end with the turn, so only stall shows)
    stalls = [st | p <- [P1, P2], m <- SV.toList (sMons (sideOf p b')), Volatile i st <- SV.toList (monVolatiles m), i == fromEnum CStall]
    stallSeen = mempty {seenStall = length stalls, seenStallClimb = length [() | st <- stalls, esA st >= 9]}
    -- slice 7e: the sides whose active Pokemon holds a live choicelock at a move request (the step starts from one)
    lockedNow = [p | Awaiting MoveRequest _ _ <- [request b], p <- [P1, P2], sHasActive (sideOf p b), choiceLocked dx (activeMon p b)]
    abilityIs name m = abilityId dx (monAbility m) == name
    itemIs name m = monItem m /= noItem && itemId dx (monItem m) == name
    knownMoves m = [mvId (dxMoves dx V.! msMove ms) | ms <- SV.toList (monMoves m)]
    inPlay = [monOf b (activeActor p b) | p <- [P1, P2], sHasActive (sideOf p b)]
    removers = ["knockoff", "thief", "trick", "switcheroo"]
    -- (slice 8d: Magician and Pickpocket take an item too, but only a holder that was in play during the step: active at its start or at its
    -- end, which includes one that came in during it (a pivot's replacement or a drag). A Pokemon that came in and left again inside one step
    -- cannot be told from the two states, but a switch-in during a step ends it at the next request, and a drag leaves the dragged one in play.)
    stealing = any (any (`elem` removers) . knownMoves) inPlay
      || or [ abilityIs "magician" m0 || abilityIs "pickpocket" m0 || abilityIs "magician" m1 || abilityIs "pickpocket" m1
            | p <- [P1, P2], (m0, m1) <- zip (SV.toList (sMons (sideOf p b))) (SV.toList (sMons (sideOf p b'))), monActive m0 || monActive m1 ]
    -- (a later forced switch in the same step, a Roar after a Red Card, can bring the attacker back in)
    forcing = any (any (`elem` ["roar", "whirlwind", "dragontail", "circlethrow"]) . knownMoves) inPlay
    switchOuts = mconcat <$> mapM switchOut [(p, i) | p <- [P1, P2], i <- [0 .. SV.length (sMons (sideOf p b)) - 1]]
    switchOut (p, i) = case (sMons (sideOf p b) SV.!? i, sMons (sideOf p b') SV.!? i) of
      (Just m0, Just m1) -> do
        let who = show p ++ " " ++ show (monSpecies m0) ++ ": "
            wasOn = monActive m0 && monHp m0 > 0
            inPlay2 = wasOn && monActive m1 && monHp m1 > 0 -- slice 8c
            benched = not (monActive m1) && monHp m1 > 0
            foe = otherP p
            fainted0 = monFainted m0
            foeBefore = if sHasActive (sideOf foe b) then U.head (sSlots (sideOf foe b)) else (-1)
            foeAfter = if sHasActive (sideOf foe b') then monActive (monOf b' (Actor foe foeBefore)) else False
            healed = min (monMaxHp m0) (monHp m0 + monMaxHp m0 `quot` 3)
        if fainted0 && monHp m1 /= 0 then Left (who ++ "a fainted Pokemon regained HP") else Right ()
        let regen = wasOn && benched && abilityIs "regenerator" m0 && isSwitch (chose p)
        if regen && monHp m1 /= healed
          then Left (who ++ "Regenerator holder switched out from " ++ show (monHp m0) ++ " HP and has " ++ show (monHp m1) ++ ", not " ++ show healed)
          else Right ()
        -- (the ability can change within the step: a Terastallizing Ogerpon takes Embody Aspect before it pivots out, so the
        -- holder must still have Natural Cure after the step)
        let cure = wasOn && benched && abilityIs "naturalcure" m0 && abilityIs "naturalcure" m1
        if cure && monStatus m1 /= StatusNone then Left (who ++ "Natural Cure holder switched out with the status " ++ show (monStatus m1)) else Right ()
        let ejected = not stealing && wasOn && itemIs "ejectbutton" m0 && monItem m1 == noItem
            -- (slice 8a: a Guard Dog attacker is not forced out: its DragOut returns null, while the Red Card is still used up)
            guardDogFoe = sHasActive (sideOf foe b) && abilityIs "guarddog" (monOf b (Actor foe foeBefore))
            carded = not stealing && not forcing && wasOn && itemIs "redcard" m0 && monItem m1 == noItem
            -- an Eject Pack that is gone (slice 6d) was used because its holder dropped a stage, and the use flagged the holder: it is
            -- still in the active slot with the flag, or it was benched by the switch (a Red Card drag clears the flag) or fainted
            packed = not stealing && monHp m0 > 0 && itemIs "ejectpack" m0 && monItem m1 == noItem
            -- (a Red Card on the foe drags the holder out in the action tail, which clears the flag, and a Roar or the like later in the
            -- same turn can drag it back in: the pack is gone, and the holder is active and unflagged, as in Showdown, where a switch-out
            -- clears both flags. That needs a forcing move and a Red Card in play)
            draggedBack = forcing && any (itemIs "redcard") inPlay
            packedStillIn = monActive m1 && monHp m1 > 0 && monSwitchFlag m1 /= SwitchForced && not draggedBack
        if ejected && not ((monActive m1 && monSwitchFlag m1 == SwitchForced && monHp m1 > 0) || monFainted m1)
          then Left (who ++ "Eject Button used up, but its holder is neither flagged to switch nor fainted") else Right ()
        if carded && foeAfter && not guardDogFoe then Left (who ++ "Red Card used up, but the attacker was not forced out") else Right ()
        if packed && packedStillIn then Left (who ++ "Eject Pack used up, but its holder is neither flagged to switch, nor benched, nor fainted") else Right ()
        -- slice 8a: pokemon.shieldBoost is set once and never reset (not by a switch-out nor by a faint)
        if monShieldBoost m0 && not (monShieldBoost m1) then Left (who ++ "lost its Dauntless Shield boost") else Right ()
        pure mempty
          { seenRegen = fromEnum (regen && monHp m0 < monMaxHp m0)
          , seenCure = fromEnum (cure && monStatus m0 /= StatusNone)
          , seenEject = fromEnum ejected
          , seenRedCard = fromEnum carded
          , seenPack = fromEnum packed
          , seenPackIn = fromEnum (packed && not (monActive m0))
          -- slice 7b
          , seenSub = fromEnum (monActive m0 && monActive m1 && monHp m1 > 0 && not (subOf m0) && subOf m1)
          , seenSubHit = fromEnum (monActive m0 && monActive m1 && subOf m0 && subOf m1 && subHp m1 < subHp m0)
          -- slice 8a
          , seenShieldBoost = fromEnum (not (monShieldBoost m0) && monShieldBoost m1)
          , seenIntimidateIn = fromEnum (not (monActive m0) && monActive m1 && monHp m1 > 0 && abilityIs "intimidate" m1)
          , seenDropAbility = fromEnum (monActive m0 && monActive m1 && monHp m1 > 0 && any (`abilityIs` m1) ["clearbody", "guarddog", "innerfocus", "bigpecks", "mirrorarmor", "defiant", "contrary"] && monBoosts m1 /= noBoosts)
          -- slice 8c (the ability can change within the step: it is read at both ends)
          , seenGuarded = fromEnum (inPlay2 && abilityIs "magicguard" m0 && abilityIs "magicguard" m1 && monStatus m0 `elem` [StatusBrn, StatusPsn, StatusTox])
          , seenAbsorbed = fromEnum (inPlay2 && abilityIs "waterabsorb" m0 && abilityIs "waterabsorb" m1 && monHp m1 > monHp m0)
          }
      _ -> Right mempty
    isSwitch c = case c of Just (Switch _) -> True; _ -> False
    -- slice 8d. Nothing supported creates an item: an item that a Pokemon holds after a step it held before the step, or a Pokemon of the
    -- other side did (Trick, Thief, Magician and Pickpocket move items between the sides; Knock Off and the consumed items only remove them).
    itemsNeverCreated = mapM_ itemOk [(p, i) | p <- [P1, P2], i <- [0 .. SV.length (sMons (side' p)) - 1]]
    itemOk (p, i) = case (sMons (side0 p) SV.!? i, sMons (side' p) SV.!? i) of
      (Just m0, Just m1)
        | monItem m1 /= noItem && monItem m1 /= monItem m0 && monItem m1 `notElem` [monItem x | x <- SV.toList (sMons (side0 (otherP p)))] ->
            Left (show p ++ " member " ++ show i ++ " holds " ++ show (itemId dx (monItem m1)) ++ " after a step in which neither it nor the other side had it")
      _ -> Right ()
    -- slice 8d: the three events the ability phase's covers count (see 'Seen')
    seen8d = mempty
      { seenMoxie = length [() | p <- [P1, P2], (m0, m1) <- pairs p, abilityIs "moxie" m0, monActive m0, monActive m1, monHp m1 > 0, bsAtk (monBoosts m1) > bsAtk (monBoosts m0), foeDown p]
      , seenThief = length [() | p <- [P1, P2], (m0, m1) <- pairs p, abilityIs "magician" m0 || abilityIs "pickpocket" m0, monItem m0 == noItem, monItem m1 /= noItem]
      , seenBerserk = length [() | p <- [P1, P2], (m0, m1) <- pairs p, abilityIs "berserk" m0, monActive m0, monActive m1, monHp m1 > 0, monHp m1 < monHp m0, bsSpa (monBoosts m1) > bsSpa (monBoosts m0)]
      }
    pairs p = zip (SV.toList (sMons (side0 p))) (SV.toList (sMons (side' p)))
    -- the foe's Pokemon in play before the step had HP and has fainted after it
    foeDown p = sHasActive (sideOf (otherP p) b) && let a = activeActor (otherP p) b in monHp (monOf b a) > 0 && monFainted (monOf b' a)
    subOf m = any (\(Volatile i _) -> i == fromEnum CSubstitute) (SV.toList (monVolatiles m))
    subHp m = sum [esA st | Volatile i st <- SV.toList (monVolatiles m), i == fromEnum CSubstitute]
    otherP p = if p == P1 then P2 else P1
    drag (p, c) = do
      let s = sideOf p b
          s' = sideOf p b'
          expected = case c of
            Just (Switch k) -> sSlots s U.!? (k - 1)
            Just _ | sHasActive s -> sSlots s U.!? 0
            _ -> Nothing
          actual = if sHasActive s' then sSlots s' U.!? 0 else Nothing
      case (expected, actual) of
        (Just e, Just a) | e /= a -> do
          let who = show p ++ " dragged-out member " ++ show e ++ ": "
              out = monOf b' (Actor p e)
              queued = [x | x <- SV.toList (bQueue b'), acActor x == Just (Actor p e), acKind x /= KRunSwitch]
              inn = monOf b (Actor p a)
          if monActive out then Left (who ++ "still active") else Right ()
          if monForceSwitchFlag out then Left (who ++ "still has its force-switch flag") else Right ()
          if monHp out <= 0 then Left (who ++ "has no HP") else Right ()
          if null queued then Right () else Left (who ++ "has queued actions: " ++ show queued)
          -- (a side that chose a switch had its old active benched during the step, and the drag may bring that one back)
          let switched = case c of Just (Switch _) -> True; _ -> False
          if monFainted inn || monHp inn <= 0 || (monActive inn && not switched)
            then Left (show p ++ " member " ++ show a ++ " was dragged in but was active or fainted before the step")
            else Right ()
          pure True
        _ -> pure False
    turnOk = case request b' of
      Awaiting SwitchRequest _ _ | bTurn b' /= bTurn b -> Left ("the turn counter went from " ++ show (bTurn b) ++ " to " ++ show (bTurn b') ++ " across a switch request")
      _ -> Right ()
    pivotsOk ds = case request b of
      Awaiting SwitchRequest _ _ -> mapM_ (pivoted ds) [(p, o) | p <- [P1, P2], o <- activeOrigs p]
      _ -> Right ()
    activeOrigs p = [i | (i, m) <- zip [0 ..] (SV.toList (sMons (sideOf p b))), monActive m, monSwitchFlag m == SwitchMove, monHp m > 0]
    -- (a drag later in the step can bring the pivoted Pokemon back, a bench member like any other, and a hazard can then faint it)
    pivoted ds (p, o) =
      let m' = monOf b' (Actor p o)
          queued = [a | a <- SV.toList (bQueue b'), acActor a == Just (Actor p o), acKind a /= KRunSwitch]
          dragged = (if p == P1 then fst else snd) ds
       in if monActive m' && not dragged then Left (show p ++ " member " ++ show o ++ " is still active after its self-switch")
          else if monHp m' <= 0 && not dragged then Left (show p ++ " member " ++ show o ++ " has no HP after its self-switch")
          else if not (null queued) then Left (show p ++ " member " ++ show o ++ " has queued actions after its self-switch: " ++ show queued)
          else Right ()
    side' p = if p == P1 then bP1 b' else bP2 b'
    side0 p = if p == P1 then bP1 b else bP2 b
    one (p, i) = case (sMons (side0 p) SV.!? i, sMons (side' p) SV.!? i) of
      (Just m0, Just m1)
        | monStellarBoosted m0 .&. complement (monStellarBoosted m1) /= 0 -> Left (show p ++ " member " ++ show i ++ " lost a boosted type")
        | not (monActive m0) && not (monActive m1) && monStellarBoosted m0 /= monStellarBoosted m1 ->
            Left (show p ++ " member " ++ show i ++ " changed its boosted types on the bench")
      _ -> Right ()

-- | Advance with random legal choices for at least @n@ steps and until the predicate holds, or the end.
advanceWhile :: Dex -> Int -> (Battle -> Bool) -> Battle -> Sample (Either String Battle)
advanceWhile dx n p b = case request b of
  Finished _ -> pure (Right b)
  Awaiting _ a1 a2
    | n <= 0 && p b -> pure (Right b)
    | n < negate stepCap -> pure (Right b)
    | otherwise -> do
        cs <- pickBoth b a1 a2
        r <- step dx b cs
        case r of
          Left e -> pure (Left ("step rejected legal choices " ++ show cs ++ ": " ++ show e))
          Right b' -> advanceWhile dx (n - 1) p b'

-- | State invariants and the decision-point contract: each side asked has legal choices and a side not asked
-- has none; HP in 0..maxHp (so no heal passes the maximum), PP in 0..maxPP, stages in -6..6, fainted means HP 0; getTypes is the species' own, less Flying under a Roost.
checkBattle :: Dex -> Battle -> Either String ()
checkBattle dx b = do
  case request b of
    Awaiting _ a1 a2 -> do
      ask P1 a1
      ask P2 a2
    Finished _ -> mapM_ (\p -> if null (legalChoices b p) then Right () else Left ("choices after the end for " ++ show p)) [P1, P2]
  mapM_ side [(P1, bP1 b), (P2, bP2 b)]
  switchFlags
  -- the force-switch flag (slice 6b) is set by a forceSwitch move and consumed by the action tail of the same action
  mapM_ (\(p, s) -> if any monForceSwitchFlag (SV.toList (sMons s)) then Left (show p ++ ": a force-switch flag at a decision point") else Right ())
    [(P1, bP1 b), (P2, bP2 b)]
  -- slice 8 base: battle.activeMove is set while a move is used and cleared by the action tail (battle.ts 2875), so no decision point
  -- has one
  case bActiveMove b of
    Nothing -> Right ()
    Just mu -> Left ("a move in use at a decision point: " ++ show mu)
  -- slice 8d: the faint queue is resolved by the action tail, so it is empty at every decision point
  if SV.null (bFaintQueue b) then Right () else Left ("a faint queue at a decision point: " ++ show (bFaintQueue b))
  conditions
  choiceLocks
  disableRules
  where
    -- slice 7f. At a move request the moves offered are exactly the slots that every disabling effect allows at once, or Struggle (move 1)
    -- when there are none. The effects are the four whose onDisableMove disables slots (the rule of each, from its Showdown handler):
    -- an encore (every slot but the encored move, when the Pokemon knows it: data/conditions.ts encore.onDisableMove), a taunt (Status
    -- moves but Me First: taunt.onDisableMove), a live choicelock (every slot but the locked move) and an Assault Vest (Status moves);
    -- and a slot with no PP is unusable. Both directions are checked, so an effect that is missed and one that is applied twice both fail.
    disableRules = case request b of
      Awaiting MoveRequest _ _ -> mapM_ offerOk [p | p <- [P1, P2], sHasActive (sideOf p b), not (monFainted (activeMon p b))]
      _ -> Right ()
    offerOk p = do
      let m = activeMon p b
          slots = zip [1 :: Int ..] (SV.toList (monMoves m))
          allowed = [n | (n, ms) <- slots, slotAllowed dx m ms]
          expected = if null allowed then [1] else allowed
          offered = nub [n | c <- legalChoices b p, n <- case c of Move k -> [k]; MoveTera k -> [k]; _ -> []]
      if sort offered == sort expected then Right ()
        else Left (show p ++ ": offered move slots " ++ show (sort offered) ++ " where the disabling effects allow " ++ show expected)
    -- slice 7e. At a move request every Pokemon in play has had endTurn's DisableMove event (which drops a stale lock), so a live choicelock
    -- (1) is on a Pokemon holding a Choice item, with the locked move among its slots; and (2) leaves it no other move in 'legalChoices':
    -- the Move and MoveTera choices name the locked slot only, or the single Struggle choice when that slot has no PP left. (Switches stay.)
    choiceLocks = case request b of
      Awaiting MoveRequest _ _ -> mapM_ lockOk [p | p <- [P1, P2], sHasActive (sideOf p b), choiceLocked dx (activeMon p b)]
      _ -> Right ()
    lockOk p = do
      let m = activeMon p b
          locked = [esA st | Volatile i st <- SV.toList (monVolatiles m), i == fromEnum CChoiceLock]
          who = show p ++ ": a choicelock "
          slotOf n = monMoves m SV.!? (n - 1)
          named c = case c of Move n -> Just n; MoveTera n -> Just n; _ -> Nothing
          offered = [n | Just n <- map named (legalChoices b p)]
      if any (\l -> any ((== l) . msMove) (SV.toList (monMoves m))) locked then Right () else Left (who ++ "with the locked move not among the move slots")
      if mustStruggle b p
        then if all (== 1) offered then Right () else Left (who ++ "with no usable move offers more than Struggle")
        else if all (\n -> fmap msMove (slotOf n) `elem` map Just locked) offered then Right () else Left (who ++ "that offers a move other than the locked one")
    -- The switch flag (slice 6a): only the Pokemon in the active slot has one (a fainted one is no longer 'isActive' there: faintMessages
    -- sets `isActive = false`); a self-switch move's flag (SwitchMove) is on an active Pokemon with HP, a faint's (SwitchForced) on a
    -- fainted one, an Eject Button's on an active one with HP and no item. A flag outlives no decision point except the switch request that asks for it, and a switch request asks exactly
    -- the sides whose active-slot Pokemon is flagged (and that have a Pokemon left).
    switchFlags = do
      let flagged p = [(i, m) | (i, m) <- zip [0 :: Int ..] (SV.toList (sMons (sideOf p b))), monSwitchFlag m /= NoSwitch]
      mapM_ (\(p, (i, m)) -> do
        if U.head (sSlots (sideOf p b)) == i then Right () else Left (show p ++ ": a switch flag on member " ++ show i ++ ", not the one in the active slot")
        case monSwitchFlag m of
          SwitchMove | monHp m <= 0 || not (monActive m) -> Left (show p ++ ": a self-switch flag on a Pokemon with no HP or off the field")
          -- (an Eject Button's flag, slice 6c: set on a live active holder together with its use of the item, so the holder has no item)
          SwitchForced | not (monFainted m) && not (monActive m && monHp m > 0 && monItem m == noItem) ->
            Left (show p ++ ": a forced-switch flag on a Pokemon that has not fainted and is not an Eject Button holder that used its item")
          _ -> Right ())
        [(p, im) | p <- [P1, P2], im <- flagged p]
      case request b of
        Awaiting SwitchRequest a1 a2 -> do
          let want p = sLeft (sideOf p b) > 0 && not (null (flagged p))
          if (a1, a2) == (want P1, want P2) then Right () else Left ("a switch request asks " ++ show (a1, a2) ++ " but the flagged sides are " ++ show (want P1, want P2))
          if a1 || a2 then Right () else Left "a switch request that asks no side"
          mapM_ (\p -> if all isSwitch (legalChoices b p) then Right () else Left (show p ++ ": a choice other than a switch at a switch request")) [P1, P2]
        Awaiting _ _ _ -> if null (flagged P1) && null (flagged P2) then Right () else Left "a switch flag at a team preview or a move request"
        Finished _ -> Right ()
    isSwitch c = case c of Switch _ -> True; _ -> False
    -- Side conditions and the field. Showdown's @sideConditions@ is an object keyed by id, so an id appears once, in
    -- insertion order; each state took its @effectOrder@ from a counter that only grows, so the orders along the
    -- insertion order are strictly increasing, and the battle's counter is ahead of every state's order. Court Change keeps this: it
    -- moves the states themselves (data/moves.ts 3070-3093), and every side condition the engine has is in its list, so each side
    -- ends with the other's whole sequence (a state left behind would be followed by arrivals of lower order). Layers
    -- (Spikes 3, Toxic Spikes 2: the @onSideRestart@ guards, data/moves.ts) are in 1..max for exactly those
    -- conditions; other conditions have none. A weather or terrain that is absent has the empty state.
    conditions = do
      mapM_ sideConds [(P1, bP1 b), (P2, bP2 b)]
      let f = bField b
      pseudo <- pure (SV.toList (fPseudo f))
      if distinct [i | CondEntry i _ <- pseudo] then Right () else Left "a pseudo-weather appears twice"
      mapM_ (\(CondEntry i st) -> durationOk "pseudo-weather " (maybe "" cnId (dxFieldConds dx V.!? i)) i st) pseudo
      -- (clearWeather resets the state but keeps its target, so the empty state may have the bit set)
      if fWeather f == noCond && (fWeatherState f) {esHasTarget = False} /= noEffectState then Left "no weather but a weather state" else Right ()
      -- a weather in place has 1..8 turns left (5, or 8 with the setter's rock) and the countdown has not reached 0
      if fWeather f /= noCond then durationOk "weather " (maybe "" cnId (dxFieldConds dx V.!? fWeather f)) (fWeather f) (fWeatherState f) else Right ()
      if fTerrain f == noCond && (fTerrainState f) {esHasTarget = False} /= noEffectState then Left "no terrain but a terrain state" else Right ()
      -- a terrain in place has 1..8 turns left (5, or 8 with Terrain Extender)
      if fTerrain f /= noCond then durationOk "terrain " (maybe "" cnId (dxFieldConds dx V.!? fTerrain f)) (fTerrain f) (fTerrainState f) else Right ()
      let orders = [esOrder st | (_, s) <- [(P1, bP1 b), (P2, bP2 b)], CondEntry _ st <- SV.toList (sConds s)]
            ++ [esOrder st | CondEntry _ st <- pseudo]
            ++ [esOrder st | (_, s) <- [(P1, bP1 b), (P2, bP2 b)], m <- SV.toList (sMons s)
                           , st <- [monStatusState m, monAbilityState m, monItemState m] ++ [v | Volatile _ v <- SV.toList (monVolatiles m)]]
      -- the first state takes order 0, so before any state exists the counter 0 is ahead of the default order 0
      if all (< max 1 (bEffectOrder b)) orders then Right () else Left ("an effect order reaches the battle's counter " ++ show (bEffectOrder b))
    sideConds (p, s) = do
      let es = SV.toList (sConds s)
          who = show p ++ " side condition "
      if distinct [i | CondEntry i _ <- es] then Right () else Left (who ++ "appears twice")
      let orders = [esOrder st | CondEntry _ st <- es]
      if and (zipWith (<) orders (drop 1 orders))
        then Right ()
        else Left (who ++ "effect orders " ++ show orders ++ " are not strictly increasing")
      mapM_ (\(CondEntry i st) -> do
        let name = maybe "" cnId (dxFieldConds dx V.!? i)
            layers = esA st
            bound = lookup name [("spikes", 3), ("toxicspikes", 2)]
        case bound of
          Just hi | layers < 1 || layers > hi -> Left (who ++ show name ++ " has " ++ show layers ++ " layers, outside 1.." ++ show hi)
          Nothing | layers /= -1 -> Left (who ++ show name ++ " has layers " ++ show layers)
          _ -> Right ()
        durationOk who name i st) es
    distinct xs = length xs == length (nub xs)
    -- A condition with a duration (condition.duration, data/moves.ts) is in place only with 1 or more turns left: the
    -- residual countdown ends it at 0. Its duration is at most the static one, except the screens, whose
    -- durationCallback gives 8 with a Light Clay user (the weathers: 8 with their rock, the terrains with Terrain Extender). A condition with no duration has none.
    durationOk who name i st = do
      let base = maybe 0 cnDuration (dxFieldConds dx V.!? i)
          hi = if name `elem` ["reflect", "lightscreen", "auroraveil", "sunnyday", "raindance", "sandstorm", "snowscape", "electricterrain", "grassyterrain", "mistyterrain", "psychicterrain"] then 8 else base
          d = esDuration st
      if (base == 0 && d /= 0) || (base /= 0 && (d < 1 || d > hi))
        then Left (who ++ show name ++ " has duration " ++ show d ++ ", outside its bounds (static " ++ show base ++ ")")
        else Right ()
    ask p asked = case (asked, null (legalChoices b p)) of
      (True, True) -> Left (show p ++ " is asked but has no legal choices")
      (False, False) -> Left (show p ++ " is not asked but has legal choices")
      _ -> Right ()
    side (p, s) = do
      mapM_ (mon p (sTeraUsed s)) (SV.toList (sMons s))
      -- Terastallization: at most one member is terastallized, and none unless the side has used its Tera (a member that
      -- fainted lost `terastallized` but the side keeps having used it)
      let terad = length (filter monTerastallized (SV.toList (sMons s)))
      if terad <= 1 && (sTeraUsed s || terad == 0)
        then Right ()
        else Left (show p ++ ": " ++ show terad ++ " terastallized members with sTeraUsed " ++ show (sTeraUsed s))
      -- slice 8b: side.totalFainted counts the faints (battle.ts 2601), and a faint is final, so it is the number of fainted members
      let fallen = length (filter monFainted (SV.toList (sMons s)))
      if sTotalFainted s == fallen then Right () else Left (show p ++ ": totalFainted " ++ show (sTotalFainted s) ++ " with " ++ show fallen ++ " fainted members")
      -- a Supreme Overlord holder's count is the faints at its switch-in: at most 5, and at most the faints so far
      mapM_ (\m -> if abilityId dx (monAbility m) == "supremeoverlord" && esA (monAbilityState m) > min 5 (sTotalFainted s)
                     then Left (show p ++ ": Supreme Overlord with fallen " ++ show (esA (monAbilityState m)) ++ " and totalFainted " ++ show (sTotalFainted s)) else Right ())
        (SV.toList (sMons s))
      -- a Tera choice is legal only together with the move of the same slot
      let cs = legalChoices b p
      if and [Move n `elem` cs | MoveTera n <- cs] then Right () else Left (show p ++ ": a Tera choice without its move")
    mon p sTeraUsed' m = do
      let who = show p ++ " " ++ show (monSpecies m) ++ ": "
          bad what = Left (who ++ what)
      if monHp m < 0 || monHp m > monMaxHp m then bad ("hp " ++ show (monHp m) ++ " outside 0.." ++ show (monMaxHp m)) else Right ()
      mapM_ (\ms -> if msPP ms < 0 || msPP ms > msMaxPP ms then bad ("pp " ++ show (msPP ms) ++ " outside 0.." ++ show (msMaxPP ms)) else Right ())
        (SV.toList (monMoves m))
      let bs = monBoosts m
          stages = [bsAtk bs, bsDef bs, bsSpa bs, bsSpd bs, bsSpe bs, bsAccuracy bs, bsEvasion bs]
      if any (\x -> x < -6 || x > 6) stages then bad ("stage outside -6..6: " ++ show stages) else Right ()
      -- slice 8a: only a Dauntless Shield holder has spent the boost
      if monShieldBoost m && abilityId dx (monSetAbility m) /= "dauntlessshield" then bad "a Dauntless Shield boost on another ability" else Right ()
      if monFainted m && monHp m /= 0 then bad "fainted with hp > 0" else Right ()
      -- Only while the battle goes on. Once it has ended, `resolveFaints` returns at once (`if (this.ended) return;`,
      -- battle.ts 2563), but the action's phazing loop still drags a Pokemon in (2866-2873), with no check of `ended`. A Pokemon
      -- dragged onto Stealth Rock after the last foe fainted (a Rocky Helmet against Dragon Tail) stays at 0 HP, not fainted,
      -- and Showdown's final state shows it so.
      if monHp m == 0 && not (monFainted m) && bResult b == Ongoing then bad "hp 0 but not fainted" else Right ()
      if monStatus m == StatusFainted && monHp m /= 0 then bad "status fnt with hp > 0" else Right ()
      -- statuses: toxic's stage is in 0..15 exactly while badly poisoned (its onStart sets 0, residual stops at
      -- 15); the other statuses and a cured Pokemon have a fresh statusState (no stage); only a Pokemon that
      -- fainted badly poisoned keeps a stale stage (faint and the fnt / '' assignments leave statusState alone)
      let stage = esA (monStatusState m)
      -- the volatiles: flinch (duration 1) and the volatiles of Protosynthesis, Quark Drive and Unburden (no duration).
      -- A volatile of an ability lives only on a holder of that ability; the Paradox ones have a best stat in 0..4 and
      -- a fromBooster flag that is 1 only after Booster Energy was used. Trick or Thief can then give the holder another item,
      -- but never a Booster Energy if it has the Paradox tag (boosterenergy.onTakeItem refuses that receiver). The four
      -- untagged holders (Gouging Fire, Raging Bolt, Iron Boulder, Iron Crown) can receive one and keep it in sun or
      -- Electric Terrain, where it is not used.
      let flinched = any (\(Volatile i _) -> i == fromEnum CFlinch) (SV.toList (monVolatiles m))
          chilled = any (\(Volatile i _) -> i == fromEnum CChillyReception) (SV.toList (monVolatiles m))
          volatileOk (Volatile i st)
            | i == fromEnum CFlinch = esDuration st == 1
            | i == fromEnum CProtosynthesis = ability "protosynthesis" && esDuration st == 0 && esA st >= 0 && esA st <= 4 && boosterOk st
            | i == fromEnum CQuarkDrive = ability "quarkdrive" && esDuration st == 0 && esA st >= 0 && esA st <= 4 && boosterOk st
            | i == fromEnum CUnburden = ability "unburden" && esDuration st == 0
            -- roost: one turn (duration 1), and the state keeps the species' types as they were (typeWas, esA and esB)
            | i == fromEnum CRoost = esDuration st == 1
            -- chilly reception: one turn (duration 1); its priorityChargeMove action adds it
            | i == fromEnum CChillyReception = esDuration st == 1
            -- slice 7a: stall (a protect move's counter) is 3 tripled up to 729, and lasts one or two turns (duration 2, 1 after a
            -- residual). Protect, Spiky Shield, Baneful Bunker and Endure last the turn they are added (duration 1) and end at its
            -- Residual, so no decision point but a mid-turn switch request, or the end of the battle, sees one
            | i == fromEnum CStall = esA st `elem` [3, 9, 27, 81, 243, 729] && esDuration st `elem` [1, 2]
            -- (a pivot or an Eject Button that switches its holder out mid-turn leaves a request while the foe's protect volatile, or
            -- Endure, is still up: the move that triggered it was not blocked by it, as it hit the Endure holder or went around)
            | i `elem` map fromEnum [CProtect, CSpikyShield, CBanefulBunker, CEndure] = esDuration st == 1 && (midTurnRequest || finished)
            -- slice 7e: choicelock has no duration; its state is the locked move (a Dex move index, or -1 for Struggle)
            | i == fromEnum CChoiceLock = esDuration st == 0 && esA st >= (-1) && esA st < V.length (dxMoves dx)
            -- slice 7c. encore: 3 turns, or 4 when the target had no move queued, counting down; esA is the encored move, one of
            -- the Pokemon's own slots. taunt: 3 or 4 turns.
            | i == fromEnum CEncore = esDuration st >= 1 && esDuration st <= 4 && any ((== esA st) . msMove) (monMoves m)
            | i == fromEnum CTaunt = esDuration st >= 1 && esDuration st <= 4
            -- substitute (slice 7b): only an active Pokemon with HP has one, it has no duration, and its HP is in 1..floor(maxhp / 4): it
            -- starts at the quarter (onStart) and the cost of making it leaves the holder with more than that, and a hit takes it down
            -- to 0, where it goes
            | i == fromEnum CSubstitute = monActive m && not (monFainted m) && monHp m > 0 && esDuration st == 0 && esA st >= 1 && esA st <= monMaxHp m `quot` 4
            -- slice 8b: flashfire has no duration and lives only on a holder of the ability (the ability's End removes it on a switch-out and a faint)
            | i == fromEnum CFlashFire = ability "flashfire" && esDuration st == 0
            | otherwise = False
          -- a live Pokemon flagged to switch out (a pivot's or an Eject Button's switch request) means the turn is not over
          -- (a battle that ends mid-turn, on Spiky Shield's damage for one, never reaches the Residual that removes them)
          finished = case request b of Finished _ -> True; _ -> False
          midTurnRequest = or [monSwitchFlag x /= NoSwitch && monHp x > 0 | p' <- [P1, P2], x <- SV.toList (sMons (sideOf p' b))]
          sp = speciesData dx (monSpecies m)
          ability name = abilityId dx (monAbility m) == name
          boosterOk st = esB st == 0 || (esB st == 1 && (not (spParadox sp) || itemId dx (monItem m) /= "boosterenergy"))
          ids = [i | Volatile i _ <- SV.toList (monVolatiles m)]
      if all volatileOk (SV.toList (monVolatiles m)) then Right () else bad ("a volatile that breaks its invariant (id, esA, esB, duration): " ++ show [(i, esA st, esB st, esDuration st) | Volatile i st <- SV.toList (monVolatiles m)])
      if length ids == length (nub ids) then Right () else bad "a volatile twice"
      -- a Pokemon with no item has a default item state (useItem clears the state with the item)
      if monItem m == noItem && monItemState m /= noEffectState then bad "no item but an item state" else Right ()
      -- Eject Pack (slice 6d): its `eject` (the item state's esA, 1 when set) is set by a drop and cleared by the next AfterMove, SwitchIn
      -- or Residual event of the same action, used or not, and by the item's End on switch-out and faint. So no Pokemon, on the field or
      -- off it, has it set at a decision point
      if monItem m /= noItem && itemId dx (monItem m) == "ejectpack" && esA (monItemState m) /= -1 then bad "an Eject Pack with eject set at a decision point" else Right ()
      -- slice 8d: Berserk's checkedBerserk (the ability state's esA) is a flag: -1 before its first Damage or AfterMoveSecondary event, then 0 or 1
      if abilityId dx (monAbility m) == "berserk" && esA (monAbilityState m) `notElem` [-1, 0, 1]
        then bad ("Berserk's checkedBerserk is " ++ show (esA (monAbilityState m))) else Right ()
      if monStatus m == StatusTox && (stage < 0 || stage > 15) then bad ("badly poisoned with stage " ++ show stage) else Right ()
      if monStatus m /= StatusTox && stage /= -1 && not (monFainted m && stage >= 0 && stage <= 15)
        then bad ("stage " ++ show stage ++ " with status " ++ show (monStatus m) ++ " on a live Pokemon") else Right ()
      -- the stored types are the species' own (nothing supported changes them); @getTypes()@ ('monTypes') is those, less Flying
      -- under a Roost (Normal when that leaves none), so a Roosted Pokemon never reports the Flying type
      -- slice 8e: a Libero or Protean holder in play that has used the ability (its ability state's esA is 1) holds the one type it took
      -- (setType); every other Pokemon's stored types are its species', and a Pokemon that leaves the field gets them back (clearVolatile)
      let liberoUsed = monActive m && (ability "libero" || ability "protean") && esA (monAbilityState m) == 1
          baseTypes = if liberoUsed then storedTypes else [t | t <- [spType1 sp, spType2 sp], t /= noType]
          storedTypes = [t | t <- [monType1 m, monType2 m], t /= noType]
          (g1, g2) = monTypes dx m
          gotTypes = [t | t <- [g1, g2], t /= noType]
          flyingId = V.elemIndex "Flying" (dxTypeNames dx)
          normalId = V.elemIndex "Normal" (dxTypeNames dx)
          roosted = fromEnum CRoost `elem` ids
          without = [t | t <- baseTypes, Just t /= flyingId]
          expectedTypes = if roosted then (if null without then maybe [] pure normalId else without) else baseTypes
      -- a terastallized Pokemon's types are exactly its Tera type, except for Stellar, which keeps the types it had; a fainted
      -- one is not terastallized
      let stellar = dxTypeNames dx V.!? monTeraType m == Just "Stellar"
          expected' = if monTerastallized m && not stellar then [monTeraType m] else expectedTypes
      if monTerastallized m && (monFainted m || roosted) then bad "terastallized and fainted, or with a Roost" else Right ()
      -- the base ability is the set's, except in a Tera forme, where the forme change made it Embody Aspect (Teal)
      let teraForme = spBaseSpecies sp == "Ogerpon" && spBattleOnly sp /= ""
      if (if teraForme then abilityId dx (monBaseAbility m) == "embodyaspectteal" else monBaseAbility m == monSetAbility m)
        then Right () else bad "the base ability is not the set's, outside a Tera forme"
      -- the types a Stellar boost was spent on: only a Pokemon whose Tera type is Stellar has any, once its side has used its Tera,
      -- and each is a real type. They are spent while terastallized and stay when it faints, so a Pokemon with any is
      -- terastallized or fainted.
      let boosted = monStellarBoosted m
          realTypes = foldr (\(i, t) acc -> if t `notElem` ["Stellar", "???"] then acc + 2 ^ i else acc) (0 :: Int) (zip [0 :: Int ..] (V.toList (dxTypeNames dx)))
      if boosted == 0 || (stellar && sTeraUsed' && (monTerastallized m || monFainted m) && boosted .&. complement realTypes == 0)
        then Right () else bad ("boosted types " ++ show boosted ++ " without a Stellar Tera type or the side's Tera, or with a type that is not real")
      if liberoUsed && length storedTypes /= 1 then bad "a Libero or Protean holder that changed type holds other than one type" else Right ()
      if storedTypes == baseTypes && gotTypes == expected' && (not roosted || all (\t -> Just t /= flyingId) gotTypes)
        then Right ()
        else bad ("stored types " ++ show storedTypes ++ ", getTypes " ++ show gotTypes ++ " but " ++ show expected')
      -- a Roost never survives to a move request (the Residual of its turn removes it); the exception, as for flinch, is
      -- a battle that ends mid-turn
      case request b of
        Awaiting MoveRequest _ _ | roosted -> bad "Roost at a move request"
        _ -> Right ()
      if roosted && (not (monActive m) || monFainted m || monHp m <= 0) then bad "Roost on a Pokemon that is not active or has no HP" else Right ()
      -- chilly reception's volatile (duration 1) is the same: an active Pokemon with HP, removed by the Residual of its turn (or by the
      -- switch, since its user pivots out)
      if chilled && (not (monActive m) || monFainted m || monHp m <= 0) then bad "chilly reception on a Pokemon that is not active or has no HP" else Right ()
      case request b of
        Awaiting MoveRequest _ _ | chilled -> bad "chilly reception at a move request"
        _ -> Right ()
      -- slice 7c: lastMove, Encore and Taunt. (3) A Pokemon that is not active has no last move and neither volatile (switching out and
      -- fainting are clearVolatile); a last move, when there is one, is one of the dex's moves; activeTurns counts the turns on the field.
      let encored = [st | Volatile i st <- SV.toList (monVolatiles m), i == fromEnum CEncore]
          taunted = not (null [() | Volatile i _ <- SV.toList (monVolatiles m), i == fromEnum CTaunt])
          legalSlots = [n | c <- legalChoices b p, n <- case c of Move k -> [k]; MoveTera k -> [k]; _ -> []]
          slotMove n = msMove <$> (monMoves m SV.!? (n - 1))
      if not (monActive m) && (monLastMove m /= -1 || not (null encored) || taunted)
        then bad "a Pokemon that is not active with a last move, an encore or a taunt" else Right ()
      if monLastMove m < -1 || monLastMove m >= V.length (dxMoves dx) then bad ("last move " ++ show (monLastMove m) ++ " is no move") else Right ()
      if monActiveTurns m < 0 || monActiveTurns m > bTurn b then bad ("active turns " ++ show (monActiveTurns m)) else Right ()
      case request b of
        Awaiting MoveRequest _ _ | monActive m, not (monFainted m) -> do
          -- (1) an encored Pokemon whose encored move can be used is offered no other move (a Tera variant of it aside, and switches
          -- are not moves)
          case encored of
            st : _ | any (\ms -> msMove ms == esA st && msPP ms > 0 && not (msDisabled ms)) (monMoves m)
                   , any (\n -> slotMove n /= Just (esA st)) legalSlots -> bad "an encored Pokemon is offered a move other than the encored one"
            _ -> Right ()
          -- (2) a taunted Pokemon is offered no Status move other than Me First (unless every move is disabled: Struggle)
          if taunted && not (mustStruggle b p)
               && any (\n -> maybe False (\mv -> mvStatus mv && mvId mv /= "mefirst") ((dxMoves dx V.!?) =<< slotMove n)) legalSlots
            then bad "a taunted Pokemon is offered a Status move" else Right ()
        _ -> Right ()
      -- flinch (duration 1) lives only on an active Pokemon with HP, and never survives to a move request
      -- (Residual removes it); the one exception is a battle that ends mid-residual
      if flinched && (not (monActive m) || monFainted m || monHp m <= 0) then bad "flinch on a Pokemon that is not active or has no HP" else Right ()
      case request b of
        Awaiting MoveRequest _ _ | flinched -> bad "flinch at a move request"
        _ -> Right ()

-- | A 'MonadChance' that counts its draws (always returning the lower bound).
newtype Counter a = Counter (State Int a)
  deriving (Functor, Applicative, Monad)

instance MonadChance Counter where
  roll lo _ = Counter (modify' (+ 1) >> pure lo)

-- | Run, returning the result and the number of draws made.
runCounter :: Counter a -> (a, Int)
runCounter (Counter m) = runState m 0
