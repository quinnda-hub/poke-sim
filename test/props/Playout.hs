{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Shared machinery for the property suite: random legal teams, random playouts under the sampling
-- interpreter, and invariant checks over a battle state. Nothing here knows an expected battle outcome.
module Playout
  ( genTeams
  , playChecked
  , advanceWhile
  , startBattle
  , checkBattle
  , Counter
  , runCounter
  , stepCap
  ) where

import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as M
import Data.List (nub)
import qualified Data.Vector as V
import qualified Data.Vector.Strict as SV
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Engine
import Engine.Chance (Sample)
import Engine.Dex (Dex (..), supportedMoves, supportedSpecies)
import Engine.Event (CondId (..))
import Engine.State

-- | Decision steps after which a playout counts as not having ended.
stepCap :: Int
stepCap = 5000

-- | A pair of random legal teams (1 to 6 Pokemon each) built through 'parseTeam', the path the trace teams
-- and the benchmark use.
genTeams :: Dex -> Gen (Team, Team)
genTeams dx = (,) <$> genTeam <*> genTeam
  where
    species = supportedSpecies dx
    moves = supportedMoves dx
    natures = M.keys (dxNatureIx dx)
    genTeam = do
      n <- Gen.int (Range.linear 1 6)
      sets <- Gen.list (Range.singleton n) genSet
      either (\e -> error ("genTeams: parseTeam rejected a generated team: " ++ e)) pure
        (parseTeam dx (toJSONArray sets))
    toJSONArray = Array . V.fromList
    genSet = do
      sp <- Gen.element species
      ms <- nub <$> Gen.list (Range.linear 1 4) (Gen.element moves)
      nat <- Gen.element natures
      lvl <- Gen.int (Range.linear 1 100)
      evs <- Gen.list (Range.singleton 6) (Gen.int (Range.linear 0 252))
      pure (object
        [ "species" .= sp, "moves" .= ms, "nature" .= nat, "level" .= lvl
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

-- | Play with random legal choices, checking every state on the way, until the battle ends (or the cap).
playChecked :: Dex -> Team -> Team -> Sample (Either String Battle)
playChecked dx t1 t2 = startBattle dx t1 t2 >>= go 0
  where
    go :: Int -> Battle -> Sample (Either String Battle)
    go n b
      | n > stepCap = pure (Left ("no result within " ++ show stepCap ++ " decision steps"))
      | Left e <- checkBattle b = pure (Left e)
      | otherwise = case request b of
          Finished _ -> pure (Right b)
          Awaiting _ a1 a2 -> do
            cs <- pickBoth b a1 a2
            r <- step dx b cs
            case r of
              Left e -> pure (Left ("step rejected legal choices " ++ show cs ++ ": " ++ show e))
              Right b' -> go (n + 1) b'

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
-- has none; HP in 0..maxHp, PP in 0..maxPP, stages in -6..6, fainted means HP 0.
checkBattle :: Battle -> Either String ()
checkBattle b = do
  case request b of
    Awaiting _ a1 a2 -> do
      ask P1 a1
      ask P2 a2
    Finished _ -> mapM_ (\p -> if null (legalChoices b p) then Right () else Left ("choices after the end for " ++ show p)) [P1, P2]
  mapM_ side [(P1, bP1 b), (P2, bP2 b)]
  where
    ask p asked = case (asked, null (legalChoices b p)) of
      (True, True) -> Left (show p ++ " is asked but has no legal choices")
      (False, False) -> Left (show p ++ " is not asked but has legal choices")
      _ -> Right ()
    side (p, s) = mapM_ (mon p) (SV.toList (sMons s))
    mon p m = do
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
      -- flinch is the only volatile: present exactly when the volatile vector is non-empty
      let flinched = not (SV.null (monVolatiles m))
      if all (\(Volatile i st) -> i == fromEnum CFlinch && esDuration st == 1) (SV.toList (monVolatiles m)) then Right () else bad "a volatile other than flinch with duration 1"
      if monStatus m == StatusTox && (stage < 0 || stage > 15) then bad ("badly poisoned with stage " ++ show stage) else Right ()
      if monStatus m /= StatusTox && stage /= -1 && not (monFainted m && stage >= 0 && stage <= 15)
        then bad ("stage " ++ show stage ++ " with status " ++ show (monStatus m) ++ " on a live Pokemon") else Right ()
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
