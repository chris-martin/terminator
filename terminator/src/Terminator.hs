module Terminator
  (
  -- * Terminated types
    Terminated
  , TerminatedMaybe (..)

  -- * Terminals
  , Open
  , LeftTerminal
  , RightTerminal

  ) where

import Terminator.Terminals
import Terminator.Terminated (Terminated (..))
import Terminator.TerminatedMaybe (TerminatedMaybe (..))
