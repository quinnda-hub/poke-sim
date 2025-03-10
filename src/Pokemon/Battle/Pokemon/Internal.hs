{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE Strict                #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Pokemon.Battle.Pokemon.Internal
  ( Pokemon(..)
  , PokemonBoosts(..)
  , MoveSet(..)
  , Nature(..)
  , Stat(..)
  , IVs
  , EVs
  , isFainted
  , mkPokemon
  , defaultBoosts
  , mkIVs
  , mkEVs
  ) where

import           Data.List.NonEmpty            (NonEmpty ((:|)))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)

import           Pokemon.Data.Move.Internal    (Move, Status)
import           Pokemon.Data.Pokedex.Internal (BaseStats (..), PokedexMon (..))
import           Pokemon.Mechanics.PType       (PType)

----------------------------------------------------------------
-- Data Types
----------------------------------------------------------------

-- | Represents an in-battle Pokemon instance with fully determined stats.
data Pokemon = Pokemon
  { name    :: Text        -- ^ Name of the Pokemon.
  , type1   :: PType       -- ^ Primary type of the Pokemon.
  , type2   :: Maybe PType -- ^ Secondary type of the Pokemon (if any).
  , level   :: Int         -- ^ Level of the Pokemon.
  , ability :: Text        -- ^ Ability name of the Pokemon.
  , moves   :: MoveSet     -- ^ Moves the Pokemon knows.
  , nature  :: Nature      -- ^ Nature of the Pokemon.
  , ivs     :: IVs         -- ^ Individual Values.
  , evs     :: EVs         -- ^ Effort Values.

  -- Status effects
  , pstatus :: Maybe Status  -- ^ Primary status effect.
  -- Stat modifiers.
  , boosts  :: PokemonBoosts -- ^ Stat boosts from moves such as Swords Dance.

    -- Calculated stats
  , maxHP   :: Int         -- ^ The max HP value.
  , hp      :: Int         -- ^ Hit Points.
  , atk     :: Int         -- ^ Attack.
  , def     :: Int         -- ^ Defense.
  , spa     :: Int         -- ^ Special Attack.
  , spd     :: Int         -- ^ Special Defense.
  , spe     :: Int         -- ^ Speed.
  } deriving (Show, Eq)

-- | Represents a set of moves a Pokémon can have (up to four).
data MoveSet = MoveSet
  { move1 :: Move
  , move2 :: Maybe Move
  , move3 :: Maybe Move
  , move4 :: Maybe Move
  } deriving (Show, Eq)

data PokemonBoosts = PokemonBoosts
  { acc :: Int -- ^ Accuracy boost.
  , atk :: Int -- ^ Attack boost.
  , def :: Int -- ^ Defense boost.
  , eva :: Int -- ^ Evasion boost.
  , spa :: Int -- ^ Special attack boost.
  , spd :: Int -- ^ Spedical defense boost.
  , spe :: Int -- ^ Speed boost.
  } deriving (Show, Eq)

type Level          = Int
type NatureModifier = Double
type AbilityName    = Text

-- | Natures that affect a Pokémon's stats.
data Nature
  = Hardy   | Brave | Adamant | Naughty | Bold  | Docile  | Relaxed | Impish | Lax
  | Lonely  | Timid | Hasty   | Serious | Jolly | Naive   | Modest  | Mild   | Quiet
  | Bashful | Rash  | Calm    | Gentle  | Sassy | Careful | Quirky
  deriving (Show, Read, Eq, Ord)

-- | Represents the effects of a nature on a Pokémon's stats.
-- Some natures are neutral and don't modify any stats.
data NatureEffect = NatureEffect
  { increaseStat :: Maybe Stat
  , decreaseStat :: Maybe Stat
  } deriving (Show, Eq)

-- | Represents each stat a Pokémon can have.
data Stat
  = HP
  | Attack
  | Defense
  | SpecialAttack
  | SpecialDefense
  | Speed
  deriving (Show, Read, Eq, Ord)

-- | Individual Values (IVs) for a Pokémon (0-31).
data IVs = IVs
  { ivHP  :: Int
  , ivAtk :: Int
  , ivDef :: Int
  , ivSpa :: Int
  , ivSpd :: Int
  , ivSpe :: Int
  } deriving (Show, Eq)

-- | Effort Values (EVs) for a Pokémon (0-255 each, total ≤ 510).
data EVs = EVs
  { evHP  :: Int
  , evAtk :: Int
  , evDef :: Int
  , evSpa :: Int
  , evSpd :: Int
  , evSpe :: Int
  } deriving (Show, Eq)

----------------------------------------------------------------
-- Nature Effects
----------------------------------------------------------------

-- | Mapping of Natures to their effects on stats.
natureEffects :: Map Nature NatureEffect
natureEffects = Map.fromList
  [ (Hardy,   NatureEffect Nothing Nothing)
  , (Lonely,  NatureEffect (Just Attack) (Just Defense))
  , (Brave,   NatureEffect (Just Attack) (Just Speed))
  , (Adamant, NatureEffect (Just Attack) (Just SpecialAttack))
  , (Naughty, NatureEffect (Just Attack) (Just SpecialDefense))
  , (Bold,    NatureEffect (Just Defense) (Just Attack))
  , (Docile,  NatureEffect Nothing Nothing)
  , (Relaxed, NatureEffect (Just Defense) (Just Speed))
  , (Impish,  NatureEffect (Just Defense) (Just SpecialAttack))
  , (Lax,     NatureEffect (Just Defense) (Just SpecialDefense))
  , (Timid,   NatureEffect (Just Speed) (Just Attack))
  , (Hasty,   NatureEffect (Just Speed) (Just Defense))
  , (Serious, NatureEffect Nothing Nothing)
  , (Jolly,   NatureEffect (Just Speed) (Just SpecialAttack))
  , (Naive,   NatureEffect (Just Speed) (Just SpecialDefense))
  , (Modest,  NatureEffect (Just SpecialAttack) (Just Attack))
  , (Mild,    NatureEffect (Just SpecialAttack) (Just Defense))
  , (Quiet,   NatureEffect (Just SpecialAttack) (Just Speed))
  , (Bashful, NatureEffect Nothing Nothing)
  , (Rash,    NatureEffect (Just SpecialAttack) (Just SpecialDefense))
  , (Calm,    NatureEffect (Just SpecialDefense) (Just Attack))
  , (Gentle,  NatureEffect (Just SpecialDefense) (Just Defense))
  , (Sassy,   NatureEffect (Just SpecialDefense) (Just Speed))
  , (Careful, NatureEffect (Just SpecialDefense) (Just SpecialAttack))
  , (Quirky,  NatureEffect Nothing Nothing)
  ]

----------------------------------------------------------------
-- Pokemon Construction
----------------------------------------------------------------

-- | Construct a 'Pokemon' from a 'PokedexMon' and additional parameters.
mkPokemon
  :: PokedexMon
  -> Level
  -> AbilityName
  -> MoveSet
  -> Nature
  -> IVs
  -> EVs
  -> Pokemon
mkPokemon pmon lvl aname moveset nature ivs evs =
  let (t1 :| rest) = pmon.types
      t2 = case rest of
             []    -> Nothing
             (x:_) -> Just x

      bs         = pmon.baseStats
      natEffect  = natureEffects Map.! nature
      incStat    = increaseStat natEffect
      decStat    = decreaseStat natEffect

      -- Determine the nature-based modifier for each stat.
      natureBoost :: Stat -> Double
      natureBoost s
        | Just s == incStat = 1.1
        | Just s == decStat = 0.9
        | otherwise         = 1.0

      hpVal  = calculateStat HP             bs lvl ivs evs (natureBoost HP)
      atkVal = calculateStat Attack         bs lvl ivs evs (natureBoost Attack)
      defVal = calculateStat Defense        bs lvl ivs evs (natureBoost Defense)
      spaVal = calculateStat SpecialAttack  bs lvl ivs evs (natureBoost SpecialAttack)
      spdVal = calculateStat SpecialDefense bs lvl ivs evs (natureBoost SpecialDefense)
      speVal = calculateStat Speed          bs lvl ivs evs (natureBoost Speed)
  in
    Pokemon
      { name     = pmon.name
      , type1    = t1
      , type2    = t2
      , level    = lvl
      , ability  = aname
      , moves    = moveset
      , nature   = nature
      , ivs      = ivs
      , evs      = evs
      , pstatus  = Nothing
      , boosts   = defaultBoosts
      , maxHP    = hpVal
      , hp       = hpVal
      , atk      = atkVal
      , def      = defVal
      , spa      = spaVal
      , spd      = spdVal
      , spe      = speVal
      }

----------------------------------------------------------------
-- Stat Calculation
----------------------------------------------------------------

-- | Calculate the final value of a given stat for a Pokémon.
calculateStat
  :: Stat           -- ^ The stat to calculate (HP or another stat)
  -> BaseStats      -- ^ The Pokémon's base stats
  -> Level          -- ^ The Pokémon's level
  -> IVs            -- ^ Individual values
  -> EVs            -- ^ Effort values
  -> NatureModifier -- ^ Nature modifier (for non-HP stats)
  -> Int
calculateStat stat bs level ivs evs modifier =
  case stat of
    HP ->
      let base = baseHP bs
          iv   = ivHP ivs
          ev   = evHP evs
      in (((2 * base + iv + ev `div` 4) * level) `div` 100) + level + 10

    Attack ->
      calcNonHP (baseAtk bs) (ivAtk ivs) (evAtk evs)
    Defense ->
      calcNonHP (baseDef bs) (ivDef ivs) (evDef evs)
    SpecialAttack ->
      calcNonHP (baseSpa bs) (ivSpa ivs) (evSpa evs)
    SpecialDefense ->
      calcNonHP (baseSpd bs) (ivSpd ivs) (evSpd evs)
    Speed ->
      calcNonHP (baseSpe bs) (ivSpe ivs) (evSpe evs)
  where
    calcNonHP baseStat iv ev =
      floor $
        ((((2 * fromIntegral baseStat + fromIntegral iv + fromIntegral (ev `div` 4))
           * fromIntegral level) / 100) + 5) * modifier

defaultBoosts :: PokemonBoosts
defaultBoosts = PokemonBoosts
                  { acc = 0
                  , atk = 0
                  , def = 0
                  , eva = 0
                  , spa = 0
                  , spd = 0
                  , spe = 0
                  }

----------------------------------------------------------------
-- Validation
----------------------------------------------------------------

-- | Validates IVs, ensuring each value is between 0 and 31.
validateIVs :: IVs -> Maybe IVs
validateIVs ivs
  | all (`elem` [0..31]) [ivHP ivs, ivAtk ivs, ivDef ivs, ivSpa ivs, ivSpd ivs, ivSpe ivs]
  = Just ivs
  | otherwise = Nothing

-- | Validates EVs, ensuring each is between 0 and 255 and total ≤ 510.
validateEVs :: EVs -> Maybe EVs
validateEVs evs
  | all (`elem` [0..255]) [evHP evs, evAtk evs, evDef evs, evSpa evs, evSpd evs, evSpe evs]
    && totalEVs <= 510
  = Just evs
  | otherwise
  = Nothing
  where
    totalEVs =
      evHP evs + evAtk evs + evDef evs + evSpa evs + evSpd evs + evSpe evs

-- | Smart constructor for IVs.
mkIVs :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe IVs
mkIVs hp atk def spA spD spe = validateIVs (IVs hp atk def spA spD spe)

-- | Smart constructor for EVs.
mkEVs :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe EVs
mkEVs hp atk def spA spD spe = validateEVs (EVs hp atk def spA spD spe)

-- | Check if Pokemon has fainted.
isFainted :: Pokemon -> Bool
isFainted poke
    | poke.hp <= 0 = True
    | otherwise    = False
