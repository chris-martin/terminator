{-# LANGUAGE LambdaCase, FlexibleInstances, GADTs, MultiParamTypeClasses #-}

module Terminator
  (
  -- * The TerminatedMaybe type
    TerminatedMaybe (..)

  -- * The terminals
  , Open, LeftTerminal, RightTerminal

  ) where

import Terminator.Terminals
import Terminator.TerminatedMaybe (TerminatedMaybe (..))
