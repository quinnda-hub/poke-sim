-- | Public API of the pure battle engine (slice 0: plain damaging moves, gen9ou singles).
--
-- Typical use: 'loadDex', build two 'Team's ('parseTeam'), 'newBattle' (ends at team preview),
-- then repeatedly ask 'request' / 'legalChoices' and apply choices with 'step'.
module Engine
  ( -- * Static data
    Dex
  , loadDex
    -- * Formats
  , Format
  , gen9ou
    -- * Teams
  , Team
  , Gender (..)
  , parseTeam
    -- * Battle
  , Battle
  , Player (..)
  , Choice (..)
  , renderChoice
  , Request (..)
  , RequestKind (..)
  , Winner (..)
  , StepError (..)
  , isLegal
  , newBattle
  , request
  , legalChoices
  , mustStruggle
  , step
    -- * Chance
  , MonadChance (..)
  ) where

import Engine.Chance (MonadChance (..))
import Engine.Choice
import Engine.Dex (Dex, loadDex)
import Engine.Format (Format, gen9ou)
import Engine.State (Battle, Gender (..), Player (..))
import Engine.Team (Team, parseTeam)
import Engine.Turn (newBattle, step)
