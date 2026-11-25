{-# LANGUAGE OverloadedStrings #-}
-- | Validated team descriptions (Showdown @PokemonSet@) and construction of Pokemon.
--
-- A 'Team' can only be obtained from 'parseTeam', which rejects anything outside slice 0, so the
-- engine never sees an unknown id, an empty team or an unsupported move.
module Engine.Team
  ( Team
  , SetSpec
  , parseTeam
  , teamSets
  , buildSide
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Data.List (nub)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine.Chance (MonadChance (..))
import Engine.Dex
import Engine.Format (Format)
import Engine.State
import Engine.Stats (hpStat, otherStat)

-- | One validated team member (ids index the 'Dex').
data SetSpec = SetSpec
  { ssSpecies :: !Int
  , ssMoves :: [Int]
  , ssNature :: !Int
  , ssEvs :: !(U.Vector Int)
  -- ^ hp, atk, def, spa, spd, spe.
  , ssIvs :: !(U.Vector Int)
  -- ^ hp, atk, def, spa, spd, spe.
  , ssLevel :: !Int
  , ssGender :: !(Maybe Gender)
  -- ^ 'Nothing' lets the engine draw it, as Showdown does.
  }
  deriving (Eq, Show)

-- | A validated team (1 to 6 members) in team-preview order.
newtype Team = Team [SetSpec]
  deriving (Eq, Show)

-- | The members of a team.
teamSets :: Team -> [SetSpec]
teamSets (Team xs) = xs

-- | Parse and validate a Showdown @PokemonSet@ JSON array.
parseTeam :: Dex -> Value -> Either String Team
parseTeam dx (Array xs)
  | n < 1 = Left "team is empty"
  | n > 6 = Left ("team has " ++ show n ++ " Pokemon (at most 6)")
  | otherwise = Team <$> traverse (\(i, v) -> either (Left . ctx i) Right (parseSet dx v)) (zip [1 :: Int ..] (V.toList xs))
  where
    n = V.length xs
    ctx i e = "Pokemon " ++ show i ++ ": " ++ e
parseTeam _ _ = Left "team is not an array"

statKeys :: [T.Text]
statKeys = ["hp", "atk", "def", "spa", "spd", "spe"]

parseSet :: Dex -> Value -> Either String SetSpec
parseSet dx (Object o) = do
  speciesName <- str "species"
  species <- lookupIx "species" (dxSpeciesIx dx) (toId speciesName)
  if spSupported (speciesData dx species) then Right () else Left ("unsupported species " ++ T.unpack speciesName)
  moveNames <- case KM.lookup "moves" o of
    Just (Array ms) -> traverse (\m -> case m of String t -> Right t; _ -> Left "move is not a string") (V.toList ms)
    _ -> Left "missing moves"
  moves <- traverse (\t -> lookupIx "move" (dxMoveIx dx) (toId t)) moveNames
  let nm = length moves
  if nm < 1 || nm > 4 then Left ("has " ++ show nm ++ " moves (need 1 to 4)") else Right ()
  if length (nub moves) /= nm then Left "duplicate moves" else Right ()
  mapM_ (\(t, m) -> if mvSupported (moveData dx m) then Right () else Left ("unsupported move " ++ T.unpack t))
        (zip moveNames moves)
  nature <- str "nature" >>= \t -> lookupIx "nature" (dxNatureIx dx) (toId t)
  level <- case KM.lookup "level" o of
    Nothing -> Right 100
    Just (Number n) | Just l <- toBoundedInteger n, l >= 1, l <= 100 -> Right l
    Just _ -> Left "level must be an integer in 1..100"
  evs <- statTable "evs" 0 252
  ivs <- statTable "ivs" 31 31
  case KM.lookup "ability" o of
    Nothing -> Right ()
    Just (String t) | toId t `elem` ["", "noability"] -> Right ()
    Just _ -> Left "ability must be \"No Ability\" or empty"
  case KM.lookup "item" o of
    Nothing -> Right ()
    Just (String "") -> Right ()
    Just _ -> Left "items are not supported"
  gender <- case KM.lookup "gender" o of
    Nothing -> Right Nothing
    Just (String "") -> Right Nothing
    Just (String "M") -> Right (Just GenderM)
    Just (String "F") -> Right (Just GenderF)
    Just (String "N") -> Right (Just GenderNone)
    Just _ -> Left "gender must be M, F, N or empty"
  pure (SetSpec species moves nature evs ivs level gender)
  where
    str k = case KM.lookup k o of
      Just (String t) -> Right t
      _ -> Left ("missing string field " ++ show k)
    -- evs / ivs: an object over the six stat keys; missing keys take the default (Showdown), others are rejected
    statTable k dflt hi = case KM.lookup k o of
      Nothing -> Right (U.replicate 6 dflt)
      Just (Object e) -> do
        case [K.toText x | x <- KM.keys e, K.toText x `notElem` statKeys] of
          [] -> Right ()
          (bad : _) -> Left (K.toString k ++ " has unknown stat " ++ T.unpack bad)
        U.fromList <$> traverse (\s -> case KM.lookup (K.fromText s) e of
          Nothing -> Right dflt
          Just (Number n) | Just v <- toBoundedInteger n, v >= 0, v <= hi -> Right v
          Just _ -> Left (K.toString k ++ "." ++ T.unpack s ++ " must be an integer in 0.." ++ show (hi :: Int))) statKeys
      Just _ -> Left (K.toString k ++ " must be an object")
parseSet _ _ = Left "set is not an object"

lookupIx :: String -> M.Map T.Text Int -> T.Text -> Either String Int
lookupIx what m k = maybe (Left ("unknown " ++ what ++ " " ++ T.unpack k)) Right (M.lookup k m)

-- | Construct one side's Pokemon (Showdown @new Side@ / @new Pokemon@), drawing genders where unset.
-- The side starts with every Pokemon in original order and nobody active.
{-# INLINABLE buildSide #-}
buildSide :: MonadChance m => Format -> Dex -> Team -> m Side
buildSide fmt dx (Team team) = do
  mons <- traverse (buildMon fmt dx) team
  pure Side
    { sMons = SV.fromList mons
    , sSlots = U.fromList [0 .. length mons - 1]
    , sLeft = length mons
    , sHasActive = False
    }

{-# INLINABLE buildMon #-}
buildMon :: MonadChance m => Format -> Dex -> SetSpec -> m Mon
buildMon fmt dx s = do
  let sp = speciesData dx (ssSpecies s)
  gender <- case ssGender s of
    Just g -> pure g
    Nothing -> case spGender sp of
      FixedMale -> pure GenderM
      FixedFemale -> pure GenderF
      FixedGenderless -> pure GenderNone
      GenderFree -> (\r -> if r == 0 then GenderM else GenderF) <$> roll 0 2
  let level = ssLevel s
      base i = spBase sp U.! i
      iv i = ssIvs s U.! i
      ev i = ssEvs s U.! i
      nat = dxNatures dx V.! ssNature s
      other' i = otherStat fmt (base i) (iv i) (ev i) level
                   (if natPlus nat == i - 1 then 1 else if natMinus nat == i - 1 then -1 else 0)
      stats = Stats (other' 1) (other' 2) (other' 3) (other' 4) (other' 5)
      -- pokemon.ts setSpecies: `if (this.species.maxHP) stats.hp = this.species.maxHP`
      maxHp = if spMaxHp sp /= 0 then spMaxHp sp else hpStat fmt (base 0) (iv 0) (ev 0) level
      slot m =
        let md = moveData dx m
            pp = if mvNoPPBoosts md || mvId md == "trumpcard" then mvPP md else mvPP md * 8 `quot` 5
         in MoveSlot m pp pp
  pure Mon
    { monSpecies = ssSpecies s
    , monLevel = level
    , monGender = gender
    , monMaxHp = maxHp
    , monHp = maxHp
    , monStats = stats
    , monBoosts = noBoosts
    , monType1 = spType1 sp
    , monType2 = spType2 sp
    , monMoves = SV.fromList (map slot (ssMoves s))
    , monSpeed = stSpe stats
    , monFainted = False
    , monFaintQueued = False
    , monActive = False
    , monSwitchFlag = False
    , monStatus = StatusNone
    }
