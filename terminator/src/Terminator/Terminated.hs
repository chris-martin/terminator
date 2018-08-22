{-# LANGUAGE FlexibleInstances, GADTs #-}

module Terminator.Terminated
  ( Terminated (..)
  , (.)
  ) where

import Terminator.Terminals

-- base
import Data.Semigroup (Semigroup ((<>)))
import Prelude ()

data Terminated a l r
  where
    OpenEnded  :: a           -> Terminated a Open             Open
    CloseEnded :: a -> l -> r -> Terminated a (LeftTerminal l) (RightTerminal r)
    LeftOpen   :: a      -> r -> Terminated a Open             (RightTerminal r)
    RightOpen  :: a -> l      -> Terminated a (LeftTerminal l) Open

instance Semigroup a => Semigroup (Terminated a Open Open)
  where
    (<>) = (.)

(.) :: Semigroup x
    => Terminated x b c
    -> Terminated x a b
    -> Terminated x a c

OpenEnded ra    . OpenEnded la    = OpenEnded  (la <> ra)
LeftOpen  ra rt . OpenEnded la    = LeftOpen   (la <> ra)    rt
OpenEnded ra    . RightOpen la lt = RightOpen  (la <> ra) lt
LeftOpen  ra rt . RightOpen la lt = CloseEnded (la <> ra) lt rt
