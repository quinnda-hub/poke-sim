{-# LANGUAGE OverloadedStrings #-}
-- | Cross-check of the typed comparison ("Compare") against the JSON-based one it replaced ("OldCompare").
--
-- At a snapshot, both run on the engine's battle: the typed one on the 'Snap', the old one on the JSON that 'snapToV1'
-- rebuilds from it. They must return identical difference lists. A battle without differences proves little, so every
-- snapshot is also compared against mutated copies of the engine's battle: a fixed list of single-field mutations,
-- each applied to a few Pokemon. Every mutation must give a non-empty list, and the same one from both.
module CrossCheck
  ( MutStats
  , newMutStats
  , crossState
  , crossMutations
  , mutationNames
  ) where

import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import qualified Compare as New
import Engine.Damage (monTypes)
import Engine.Dex
import Engine.Event (CondId, condByName)
import Engine.State
import qualified OldCompare as Old
import Trace

-- | How many times each mutation was applied (and agreed with a non-empty difference).
type MutStats = IORef (M.Map String Int)

newMutStats :: IO MutStats
newMutStats = newIORef (M.fromList [(n, 0) | n <- mutationNames])

-- | Both comparisons of one state, on the JSON rebuilt from the snapshot.
bothDiffs :: Dex -> Battle -> Snap -> ([New.Diff], [New.Diff])
bothDiffs dex b snap = (New.diffState dex b snap ++ New.unmodelled snap, Old.diffState dex b v ++ Old.unmodelled v)
  where
    v = snapToV1 snap

-- | The two comparisons must agree (on an unmutated battle, usually with no differences).
crossState :: Dex -> Battle -> Snap -> Either String ()
crossState dex b snap = case bothDiffs dex b snap of
  (n, o) | n == o -> Right ()
         | otherwise -> Left ("typed and JSON comparisons differ: typed " ++ show n ++ ", JSON " ++ show o)

-- | Apply every mutation to the battle (and the snapshot mutation to the snapshot); each must give the same non-empty
-- difference list from both comparisons. Returns the names of the mutations applied, or the first failure.
crossMutations :: Dex -> Battle -> Snap -> MutStats -> IO (Either String ())
crossMutations dex b snap stats = go (mutants dex b snap)
  where
    go [] = pure (Right ())
    go ((name, b', snap') : rest) = case bothDiffs dex b' snap' of
      (n, o)
        | null n -> pure (Left ("mutation " ++ name ++ " is a no-op: the typed comparison finds no difference"))
        | n /= o -> pure (Left ("mutation " ++ name ++ ": typed " ++ show n ++ ", JSON " ++ show o))
        | otherwise -> atomicModifyIORef' stats (\m -> (M.insertWith (+) name 1 m, ())) >> go rest

-- | Each mutation applied where it can be: the battle with one field changed, and the snapshot to compare it with.
mutants :: Dex -> Battle -> Snap -> [(String, Battle, Snap)]
mutants dex b snap =
  [(name, b', snap) | (name, f) <- battleMutations dex, Just b' <- [f b]]
    ++ [(name, b', snap) | (name, f) <- monMutations dex, (p, o) <- targets, Just b' <- [onMon p o f]]
    ++ [(name, b', snap) | (name, f) <- sideMutations dex, p <- [P1, P2], Just b' <- [onSide p f]]
    ++ [("snapshot statusState extra key", b, s') | Just s' <- [extraStatusKey snap]]
  where
    onMon p o f = let m = monOf b (Actor p o) in (\m' -> modifyMon (Actor p o) (const m') b) <$> f m
    onSide p f = (\s' -> modifySide p (const s') b) <$> f (sideOf p b)
    -- the lead and the last slot of each side
    targets = [(p, o) | p <- [P1, P2], o <- dedup [slotOrig p 0, slotOrig p (U.length (sSlots (sideOf p b)) - 1)]]
    slotOrig p i = sSlots (sideOf p b) U.! i
    dedup (x : xs) = x : [y | y <- xs, y /= x]
    dedup [] = []

-- | The snapshot, with a key in the first Pokemon's statusState that the engine does not model.
extraStatusKey :: Snap -> Maybe Snap
extraStatusKey snap = case ssMons (snP1 snap) of
  m : rest -> Just snap {snP1 = (snP1 snap) {ssMons = m {mStatusState = M.insert "duration" (SInt 1) (mStatusState m)} : rest}}
  [] -> Nothing

battleMutations :: Dex -> [(String, Battle -> Maybe Battle)]
battleMutations dex =
  [ ("turn", \b -> Just b {bTurn = bTurn b + 1})
  , ("request kind", \b -> case bRequest b of
      ReqMove | ongoing b -> Just b {bRequest = ReqSwitch}
      ReqSwitch | ongoing b -> Just b {bRequest = ReqMove}
      ReqTeamPreview | ongoing b -> Just b {bRequest = ReqMove}
      _ -> Nothing)
  , ("ended", \b -> if ongoing b then Just b {bResult = Tied} else Nothing)
  , ("winner", \b -> case bResult b of
      Won P1 -> Just b {bResult = Won P2}
      Won P2 -> Just b {bResult = Tied}
      Tied -> Just b {bResult = Won P1}
      Ongoing -> Nothing)
  , ("weather", \b -> Just (onField (\f -> f {fWeather = if fWeather f == noCond then 0 else noCond}) b))
  , ("weather duration", \b -> Just (onField (\f -> f {fWeatherState = bump (fWeatherState f)}) b))
  , ("terrain", \b -> Just (onField (\f -> f {fTerrain = if fTerrain f == noCond then 0 else noCond}) b))
  , ("terrain duration", \b -> Just (onField (\f -> f {fTerrainState = bump (fTerrainState f)}) b))
  , ("pseudo-weather added", \b -> if hasCond 0 (fPseudo (bField b)) then Nothing else Just (onField (\f -> f {fPseudo = SV.snoc (fPseudo f) (CondEntry 0 noEffectState)}) b))
  , ("pseudo-weather removed", \b -> if SV.null (fPseudo (bField b)) then Nothing else Just (onField (\f -> f {fPseudo = SV.drop 1 (fPseudo f)}) b))
  , ("pseudo-weather duration", \b -> onFirst (\(CondEntry i s) -> CondEntry i (bump s)) (fPseudo (bField b)) >>= \v -> Just (onField (\f -> f {fPseudo = v}) b))
  , ("pseudo-weather layers", \b -> onFirst (\(CondEntry i s) -> CondEntry i (bumpA s)) (fPseudo (bField b)) >>= \v -> Just (onField (\f -> f {fPseudo = v}) b))
  ]
  where
    _ = dex
    ongoing b = bResult b == Ongoing
    onField f b = b {bField = f (bField b)}

-- | Side mutations: pokemonLeft, side conditions, the slot swap.
sideMutations :: Dex -> [(String, Side -> Maybe Side)]
sideMutations _ =
  [ ("pokemonLeft", \s -> Just s {sLeft = sLeft s + 1})
  , ("side condition added", \s -> if hasCond 0 (sConds s) then Nothing else Just s {sConds = SV.snoc (sConds s) (CondEntry 0 noEffectState)})
  , ("side condition removed", \s -> if SV.null (sConds s) then Nothing else Just s {sConds = SV.drop 1 (sConds s)})
  , ("side condition duration", \s -> (\v -> s {sConds = v}) <$> onFirst (\(CondEntry i st) -> CondEntry i (bump st)) (sConds s))
  , ("side condition layers", \s -> (\v -> s {sConds = v}) <$> onFirst (\(CondEntry i st) -> CondEntry i (bumpA st)) (sConds s))
  , ("slot swap", \s -> case U.toList (sSlots s) of
      a : c : rest -> Just s {sSlots = U.fromList (c : a : rest)}
      _ -> Nothing)
  ]

hasCond :: Int -> SV.Vector CondEntry -> Bool
hasCond i = SV.any (\(CondEntry j _) -> i == j)

onFirst :: (CondEntry -> CondEntry) -> SV.Vector CondEntry -> Maybe (SV.Vector CondEntry)
onFirst f v = case SV.toList v of
  x : xs -> Just (SV.fromList (f x : xs))
  [] -> Nothing

bump :: EffectState -> EffectState
bump s = s {esDuration = esDuration s + 1}

-- | A layer count (or a counter, hp, a move index): changed to another value, present if it was absent.
bumpA :: EffectState -> EffectState
bumpA s = s {esA = if esA s < 0 then 1 else esA s + 1}

-- | One field of a Pokemon changed, where it can be.
monMutations :: Dex -> [(String, Mon -> Maybe Mon)]
monMutations dex =
  [ ("hp", \m -> Just m {monHp = monHp m + 1})
  , ("maxhp", \m -> Just m {monMaxHp = monMaxHp m + 1})
  , ("status", \m -> Just m {monStatus = if monStatus m == StatusNone then StatusBrn else StatusNone})
  , ("toxic stage", \m -> Just m {monStatusState = bumpA (monStatusState m)})
  , ("toxic stage removed", \m -> if esA (monStatusState m) >= 0 then Just m {monStatusState = (monStatusState m) {esA = -1}} else Nothing)
  , ("volatile added", \m -> case [i | c <- [minBound .. maxBound :: CondId], let i = fromEnum c, not (SV.any (\(Volatile j _) -> j == i) (monVolatiles m))] of
      i : _ -> Just m {monVolatiles = SV.snoc (monVolatiles m) (Volatile i noEffectState)}
      [] -> Nothing)
  , ("volatile removed", \m -> if SV.null (monVolatiles m) then Nothing else Just m {monVolatiles = SV.drop 1 (monVolatiles m)})
  , ("volatile duration", \m -> case SV.toList (monVolatiles m) of
      Volatile i s : rest -> Just m {monVolatiles = SV.fromList (Volatile i (bump s) : rest)}
      [] -> Nothing)
  , ("volatile counter (stall)", volatileA "stall" bumpA)
  , ("volatile hp (substitute)", volatileA "substitute" bumpA)
  , ("volatile move (encore)", volatileA "encore" moveA)
  , ("volatile move (choicelock)", volatileA "choicelock" moveA)
  , ("ability", \m -> Just m {monAbility = (monAbility m + 1) `mod` V.length (dxAbilities dex)})
  , ("item", \m -> Just m {monItem = if monItem m < 0 then 0 else (monItem m + 1) `mod` V.length (dxItems dex)})
  , ("fainted", \m -> Just m {monFainted = not (monFainted m)})
  , ("active", \m -> Just m {monActive = not (monActive m)})
  , ("stats.atk", \m -> Just m {monStats = (monStats m) {stAtk = stAtk (monStats m) + 1}})
  , ("stats.def", \m -> Just m {monStats = (monStats m) {stDef = stDef (monStats m) + 1}})
  , ("stats.spa", \m -> Just m {monStats = (monStats m) {stSpa = stSpa (monStats m) + 1}})
  , ("stats.spd", \m -> Just m {monStats = (monStats m) {stSpd = stSpd (monStats m) + 1}})
  , ("stats.spe", \m -> Just m {monStats = (monStats m) {stSpe = stSpe (monStats m) + 1}})
  , ("speed", \m -> Just m {monSpeed = monSpeed m + 1})
  , ("boosts.atk", \m -> Just m {monBoosts = (monBoosts m) {bsAtk = bsAtk (monBoosts m) + 1}})
  , ("boosts.def", \m -> Just m {monBoosts = (monBoosts m) {bsDef = bsDef (monBoosts m) + 1}})
  , ("boosts.spa", \m -> Just m {monBoosts = (monBoosts m) {bsSpa = bsSpa (monBoosts m) + 1}})
  , ("boosts.spd", \m -> Just m {monBoosts = (monBoosts m) {bsSpd = bsSpd (monBoosts m) + 1}})
  , ("boosts.spe", \m -> Just m {monBoosts = (monBoosts m) {bsSpe = bsSpe (monBoosts m) + 1}})
  , ("boosts.accuracy", \m -> Just m {monBoosts = (monBoosts m) {bsAccuracy = bsAccuracy (monBoosts m) + 1}})
  , ("boosts.evasion", \m -> Just m {monBoosts = (monBoosts m) {bsEvasion = bsEvasion (monBoosts m) + 1}})
  -- a type change counts only where it changes getTypes (a Roosted Flying type, for one, can hide it)
  , ("types (first, not terastallized)", visibleTypes (\m -> if monTerastallized m then Nothing else Just m {monType1 = (monType1 m + 1) `mod` nTypes}))
  , ("types (second, not terastallized)", visibleTypes (\m -> if monTerastallized m then Nothing else Just m {monType2 = if monType2 m == noType then 0 else noType}))
  , ("types (through Tera)", visibleTypes (\m -> if monTerastallized m then Just m {monTeraType = (monTeraType m + 1) `mod` nTypes} else Nothing))
  , ("teraType", \m -> Just m {monTeraType = (monTeraType m + 1) `mod` nTypes})
  , ("terastallized", \m -> Just m {monTerastallized = not (monTerastallized m)})
  , ("species", \m -> Just m {monSpecies = (monSpecies m + 1) `mod` V.length (dxSpecies dex)})
  , ("level", \m -> Just m {monLevel = monLevel m + 1})
  , ("gender", \m -> Just m {monGender = case monGender m of GenderM -> GenderF; GenderF -> GenderNone; GenderNone -> GenderM})
  , ("move id", moveSlot (\ms -> ms {msMove = (msMove ms + 1) `mod` V.length (dxMoves dex)}))
  , ("move pp", moveSlot (\ms -> ms {msPP = msPP ms + 1}))
  , ("move maxpp", moveSlot (\ms -> ms {msMaxPP = msMaxPP ms + 1}))
  , ("move disabled", moveSlot (\ms -> ms {msDisabled = not (msDisabled ms)}))
  , ("move slot removed", \m -> if SV.null (monMoves m) then Nothing else Just m {monMoves = SV.drop 1 (monMoves m)})
  ]
  where
    nTypes = V.length (dxTypeNames dex)
    moveSlot f m = case SV.toList (monMoves m) of
      ms : rest -> Just m {monMoves = SV.fromList (f ms : rest)}
      [] -> Nothing
    visibleTypes f m = case f m of
      Just m' | monTypes dex m' /= monTypes dex m -> Just m'
      _ -> Nothing
    moveA s = s {esA = if esA s == 0 then 1 else 0}
    -- the volatile with this name (a new one if the Pokemon has none), its state changed
    volatileA :: String -> (EffectState -> EffectState) -> Mon -> Maybe Mon
    volatileA name f m = case condByName (T.pack name) of
      Nothing -> Nothing
      Just c ->
        let i = fromEnum (c :: CondId)
            vs = SV.toList (monVolatiles m)
         in Just m {monVolatiles = SV.fromList (case break (\(Volatile j _) -> j == i) vs of
              (pre, Volatile _ s : post) -> pre ++ Volatile i (f s) : post
              (_, []) -> vs ++ [Volatile i (f noEffectState {esA = 0})])}

-- | The names of all mutations (for the report).
mutationNames :: [String]
mutationNames =
  map fst (battleMutations undefinedDex) ++ map fst (monMutations undefinedDex) ++ map fst (sideMutations undefinedDex)
    ++ ["snapshot statusState extra key"]
  where
    undefinedDex = error "mutationNames: the names do not depend on the dex"
