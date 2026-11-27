{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Shared machinery for the property suite: random legal teams, random playouts under the sampling
-- interpreter, and invariant checks over a battle state. Nothing here knows an expected battle outcome.
module Playout
  ( genTeams
  , genPivotTeams
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
import Data.List (nub)
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
genTeams dx = genTeamsWith dx False

-- | 'genTeams' weighted toward Regenerator, Natural Cure, Eject Button and Red Card (slice 6c) and the status moves that give Natural Cure
-- something to cure, and toward the moves that switch their user out (U-turn, Flip Turn, Volt Switch, Teleport, Parting Shot, Chilly
-- Reception) and the moves that force the target out (Roar, Whirlwind, Dragon Tail, Circle Throw), the hazards they drag into, Rocky
-- Helmet and Shedinja (1 HP: a contact pivot into a Rocky Helmet faints its user after the flag is set), so that the playouts reach the
-- mid-turn switch requests and the drags that a uniform draw over every move seldom does.
genPivotTeams :: Dex -> Gen (Team, Team)
genPivotTeams dx = genTeamsWith dx True

genTeamsWith :: Dex -> Bool -> Gen (Team, Team)
genTeamsWith dx pivotHeavy = (,) <$> genTeam <*> genTeam
  where
    species = supportedSpecies dx
    moves = supportedMoves dx
    pivots = [m | m <- ["uturn", "flipturn", "voltswitch", "teleport", "partingshot", "chillyreception"], m `elem` moves]
    -- (slice 6d: Sticky Web and the moves that drop a stat, for Eject Pack: self-drops, secondary drops, Defog and Strength Sap)
    dragKit = [m | m <- ["roar", "whirlwind", "dragontail", "circlethrow", "stealthrock", "spikes", "toxicspikes", "toxic", "willowisp", "thunderwave", "spore"
                        , "stickyweb", "closecombat", "dracometeor", "overheat", "leafstorm", "moonblast", "snarl", "icywind", "defog", "strengthsap"], m `elem` moves]
    pickSpecies
      | pivotHeavy && "shedinja" `elem` species = Gen.frequency [(3, pure "shedinja"), (7, Gen.element species)]
      | otherwise = Gen.element species
    -- slice 7a: the protect family, which the heavy teams carry on many sets (a repeated protect move climbs the stall counter and fails)
    protectKit = [m | m <- ["protect", "spikyshield", "banefulbunker", "endure"], m `elem` moves]
    -- slice 7d: Sucker Punch and Thunderclap, against a mix of status and attacking moves
    punches = [m | m <- ["suckerpunch", "thunderclap"], m `elem` moves]
    -- the step-4 kits (slice 7): each one that is supported joins the pivot-heavy mix with weight 3
    step4Kits = [(3, Gen.element k) | k <- [protectKit, punches], not (null k)]
    pickMove
      | pivotHeavy && not (null step4Kits) && not (null pivots) && not (null dragKit) =
          Gen.frequency ([(3, Gen.element pivots), (3, Gen.element dragKit), (2, Gen.element moves)] ++ step4Kits)
      | pivotHeavy && not (null pivots) && not (null dragKit) = Gen.frequency [(3, Gen.element pivots), (3, Gen.element dragKit), (2, Gen.element moves)]
      | pivotHeavy && not (null pivots) = Gen.frequency [(3, Gen.element pivots), (2, Gen.element moves)]
      | otherwise = Gen.element moves
    -- Rocky Helmet, and the two items that switch their holder out or force the attacker out (slice 6c)
    pickItem
      | pivotHeavy && not (null switchItems) = Gen.frequency [(4, Gen.element switchItems), (2, Gen.element ("rockyhelmet" : switchItems)), (2, Gen.element items)]
      | pivotHeavy && "rockyhelmet" `elem` items = Gen.frequency [(2, pure "rockyhelmet"), (3, Gen.element items)]
      | otherwise = Gen.element items
    switchItems = [i | i <- ["ejectbutton", "redcard", "ejectpack"], i `elem` items]
    -- Regenerator and Natural Cure act at every switch-out
    pickAbility
      | pivotHeavy && not (null switchAbilities) = Gen.frequency [(3, Gen.element switchAbilities), (3, Gen.element abilities)]
      | otherwise = Gen.element abilities
    switchAbilities = [a | a <- ["regenerator", "naturalcure"], a `elem` abilities]
    -- every ability and item the engine accepts ("noability" and the empty item among them)
    abilities = supportedAbilities dx
    items = supportedItems dx
    natures = M.keys (dxNatureIx dx)
    -- every real type, and Stellar, which a quarter of the sets take (its rules are the rarest to reach at random)
    teraTypes = [t | t <- V.toList (dxTypeNames dx), t `notElem` ["Stellar", "???"]]
    genTeam = do
      n <- Gen.int (Range.linear 1 6)
      sets <- Gen.list (Range.singleton n) genSet
      either (\e -> error ("genTeams: parseTeam rejected a generated team: " ++ e)) pure
        (parseTeam dx (toJSONArray sets))
    toJSONArray = Array . V.fromList
    genSet = do
      sp <- pickSpecies
      ms <- nub <$> Gen.list (Range.linear 1 4) pickMove
      nat <- Gen.element natures
      lvl <- Gen.int (Range.linear 1 100)
      evs <- Gen.list (Range.singleton 6) (Gen.int (Range.linear 0 252))
      ab <- pickAbility
      it <- pickItem
      tera <- Gen.frequency [(3, Gen.element teraTypes), (1, pure "Stellar")]
      pure (object
        [ "species" .= sp, "teraType" .= tera, "moves" .= ms, "nature" .= nat, "level" .= lvl, "ability" .= ab, "item" .= it
        , "evs" .= object [K.fromText k .= v | (k, v) <- zip ["hp", "atk", "def", "spa", "spd", "spe"] evs]
        ])

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
  }
  deriving (Eq, Show)

instance Semigroup Seen where
  Seen a b c d e f g h i <> Seen a' b' c' d' e' f' g' h' i' = Seen (a + a') (b + b') (c + c') (d + d') (e + e') (f + f') (g + g') (h + h') (i + i')

instance Monoid Seen where
  mempty = Seen 0 0 0 0 0 0 0 0 0

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
checkStep dx b b' (c1, c2) = (mappend stallSeen <$>) $ mapM_ one [(p, i) | (p, side) <- [(P1, bP1), (P2, bP2)], i <- [0 .. SV.length (sMons (side b)) - 1]] >> turnOk >> (((,) <$> drag (P1, c1) <*> drag (P2, c2)) >>= \ds -> pivotsOk ds >> switchOuts >>= \so -> pure (so <> mempty {seenDrags = length (filter id [fst ds, snd ds])}))
  where
    chose p = if p == P1 then c1 else c2
    -- slice 7a: the stall volatiles after the step (the protect volatiles themselves end with the turn, so only stall shows)
    stalls = [st | p <- [P1, P2], m <- SV.toList (sMons (sideOf p b')), Volatile i st <- SV.toList (monVolatiles m), i == fromEnum CStall]
    stallSeen = mempty {seenStall = length stalls, seenStallClimb = length [() | st <- stalls, esA st >= 9]}
    abilityIs name m = abilityId dx (monAbility m) == name
    itemIs name m = monItem m /= noItem && itemId dx (monItem m) == name
    knownMoves m = [mvId (dxMoves dx V.! msMove ms) | ms <- SV.toList (monMoves m)]
    inPlay = [monOf b (activeActor p b) | p <- [P1, P2], sHasActive (sideOf p b)]
    removers = ["knockoff", "thief", "trick", "switcheroo"]
    stealing = any (any (`elem` removers) . knownMoves) inPlay
    -- (a later forced switch in the same step, a Roar after a Red Card, can bring the attacker back in)
    forcing = any (any (`elem` ["roar", "whirlwind", "dragontail", "circlethrow"]) . knownMoves) inPlay
    switchOuts = mconcat <$> mapM switchOut [(p, i) | p <- [P1, P2], i <- [0 .. SV.length (sMons (sideOf p b)) - 1]]
    switchOut (p, i) = case (sMons (sideOf p b) SV.!? i, sMons (sideOf p b') SV.!? i) of
      (Just m0, Just m1) -> do
        let who = show p ++ " " ++ show (monSpecies m0) ++ ": "
            wasOn = monActive m0 && monHp m0 > 0
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
        if carded && foeAfter then Left (who ++ "Red Card used up, but the attacker was not forced out") else Right ()
        if packed && packedStillIn then Left (who ++ "Eject Pack used up, but its holder is neither flagged to switch, nor benched, nor fainted") else Right ()
        pure mempty
          { seenRegen = fromEnum (regen && monHp m0 < monMaxHp m0)
          , seenCure = fromEnum (cure && monStatus m0 /= StatusNone)
          , seenEject = fromEnum ejected
          , seenRedCard = fromEnum carded
          , seenPack = fromEnum packed
          , seenPackIn = fromEnum (packed && not (monActive m0))
          }
      _ -> Right mempty
    isSwitch c = case c of Just (Switch _) -> True; _ -> False
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
  conditions
  where
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
      if monFainted m && monHp m /= 0 then bad "fainted with hp > 0" else Right ()
      if monHp m == 0 && not (monFainted m) then bad "hp 0 but not fainted" else Right ()
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
      if monStatus m == StatusTox && (stage < 0 || stage > 15) then bad ("badly poisoned with stage " ++ show stage) else Right ()
      if monStatus m /= StatusTox && stage /= -1 && not (monFainted m && stage >= 0 && stage <= 15)
        then bad ("stage " ++ show stage ++ " with status " ++ show (monStatus m) ++ " on a live Pokemon") else Right ()
      -- the stored types are the species' own (nothing supported changes them); @getTypes()@ ('monTypes') is those, less Flying
      -- under a Roost (Normal when that leaves none), so a Roosted Pokemon never reports the Flying type
      let baseTypes = [t | t <- [spType1 sp, spType2 sp], t /= noType]
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
