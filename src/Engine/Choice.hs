-- | Player choices, requests and legality (Showdown sim/side.ts and sim/pokemon.ts request data).
module Engine.Choice
  ( Choice (..)
  , renderChoice
  , Request (..)
  , RequestKind (..)
  , Winner (..)
  , StepError (..)
  , isLegal
  , request
  , legalChoices
  , mustStruggle
  ) where

import Data.List (sort)
import qualified Data.Vector.Strict as SV
import qualified Data.Vector.Unboxed as U

import Engine.State

-- | A choice for one side.
data Choice
  = -- | Team preview order, 1-based original positions (@team 526314@).
    Team [Int]
  | -- | @move N@, 1-based slot.
    Move !Int
  | -- | @switch N@, 1-based slot.
    Switch !Int
  deriving (Eq, Show)

-- | Showdown's choice string.
renderChoice :: Choice -> String
renderChoice (Team xs) = "team " ++ concatMap show xs
renderChoice (Move n) = "move " ++ show n
renderChoice (Switch n) = "switch " ++ show n

-- | What a request asks for.
data RequestKind = TeamPreview | MoveRequest | SwitchRequest
  deriving (Eq, Show)

-- | The result of a finished battle.
data Winner = Winner !Player | Tie
  deriving (Eq, Show)

-- | Why 'Engine.step' rejected its input (nothing was drawn or changed).
data StepError
  = -- | The battle is already over.
    BattleFinished
  | -- | A side that must choose gave no choice.
    MissingChoice !Player
  | -- | A side gave a choice that is not legal.
    IllegalChoice !Player Choice
  | -- | A side that is not being asked gave a choice.
    UnexpectedChoice !Player Choice
  deriving (Eq, Show)

-- | The current decision point.
data Request
  = -- | The kind, and whether player 1 / player 2 must choose.
    Awaiting RequestKind !Bool !Bool
  | Finished !Winner
  deriving (Eq, Show)

-- | The current request (Showdown @requestState@ plus which sides received a non-wait request).
request :: Battle -> Request
request b = case bResult b of
  Won p -> Finished (Winner p)
  Tied -> Finished Tie
  Ongoing -> case bRequest b of
    ReqTeamPreview -> Awaiting TeamPreview True True
    ReqMove -> Awaiting MoveRequest True True
    ReqSwitch -> Awaiting SwitchRequest (flagged P1) (flagged P2)
    -- not reachable while ongoing: every stopping point makes a request
    ReqNone -> Awaiting MoveRequest False False
  where
    flagged p = sLeft (sideOf p b) > 0 && switchPending (sideOf p b)
    switchPending s = case slotMon s 0 of
      Just m -> monSwitchFlag m
      Nothing -> False

-- | Whether the active Pokemon has no usable move and must use Struggle (@getMoves@ returns no move when every
-- slot is disabled, or out of PP, which counts as disabled).
mustStruggle :: Battle -> Player -> Bool
mustStruggle b p = not (any usable (monMoves (activeMon p b)))
  where usable ms = msPP ms > 0 && not (msDisabled ms)

-- | Whether a choice is legal for a side now: in 'legalChoices', or, at team preview, any permutation of
-- 1..n (Showdown accepts any order; 'legalChoices' lists only the n lead choices).
isLegal :: Battle -> Player -> Choice -> Bool
isLegal b p c = case (request b, c) of
  (Awaiting TeamPreview _ _, Team xs) -> sort xs == [1 .. SV.length (sMons (sideOf p b))]
  _ -> c `elem` legalChoices b p

-- | Legal choices in Showdown's order: moves with PP (or the single Struggle choice), then switches.
-- At team preview: the n lead choices, each listing the chosen lead first and the rest ascending.
legalChoices :: Battle -> Player -> [Choice]
legalChoices b p = case request b of
  Finished _ -> []
  Awaiting TeamPreview _ _ ->
    let n = SV.length (sMons s) in [Team (k : [i | i <- [1 .. n], i /= k]) | k <- [1 .. n]]
  Awaiting MoveRequest _ _ -> moves ++ switches
  Awaiting SwitchRequest a1 a2 -> if asked a1 a2 then switches else []
  where
    s = sideOf p b
    asked a1 a2 = if p == P1 then a1 else a2
    moves
      | mustStruggle b p = [Move 1]
      | otherwise = [Move i | (i, ms) <- zip [1 ..] (SV.toList (monMoves (activeMon p b))), msPP ms > 0, not (msDisabled ms)]
    switches =
      [ Switch (i + 1)
      | i <- [1 .. U.length (sSlots s) - 1]
      , Just m <- [slotMon s i]
      , monHp m > 0
      ]
