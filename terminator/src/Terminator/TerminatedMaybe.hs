{-# LANGUAGE FlexibleInstances, GADTs #-}

module Terminator.TerminatedMaybe
  ( TerminatedMaybe (..)
  , (.)
  ) where

import Terminator.Terminals

-- base
import qualified Control.Category as Cat
import Data.Semigroup (Semigroup ((<>)))
import Prelude ()

data TerminatedMaybe a l r
  where
    Nil        ::                TerminatedMaybe a x                x
    OpenEnded  :: a           -> TerminatedMaybe a Open             Open
    CloseEnded :: a -> l -> r -> TerminatedMaybe a (LeftTerminal l) (RightTerminal r)
    LeftOpen   :: a      -> r -> TerminatedMaybe a Open             (RightTerminal r)
    RightOpen  :: a -> l      -> TerminatedMaybe a (LeftTerminal l) Open

instance Semigroup a => Cat.Category (TerminatedMaybe a)
  where
    id = Nil
    (.) = (.)

instance Semigroup a => Semigroup (TerminatedMaybe a Open Open)
  where
    (<>) = (.)

(.) :: Semigroup x
    => TerminatedMaybe x b c
    -> TerminatedMaybe x a b
    -> TerminatedMaybe x a c

Nil . l = l
r . Nil = r

OpenEnded ra    . OpenEnded la    = OpenEnded  (la <> ra)
LeftOpen  ra rt . OpenEnded la    = LeftOpen   (la <> ra)    rt
OpenEnded ra    . RightOpen la lt = RightOpen  (la <> ra) lt
LeftOpen  ra rt . RightOpen la lt = CloseEnded (la <> ra) lt rt
