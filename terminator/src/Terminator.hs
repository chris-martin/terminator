{-# LANGUAGE LambdaCase, FlexibleInstances, GADTs, MultiParamTypeClasses #-}

module Terminator
  (
  -- * The TerminatedMaybe type
    TerminatedMaybe (..)

  -- * The terminals
  , Open, LeftTerminal, RightTerminal

  -- * Functor
  , terminatedMap, EndedFunctor (..)

  ) where

import Terminator.Terminals

-- base
import Control.Category
import Data.Semigroup
import Prelude hiding ((.), id)

data TerminatedMaybe a l r
  where
    Nil        ::                TerminatedMaybe a x                x
    OpenEnded  :: a           -> TerminatedMaybe a Open             Open
    CloseEnded :: a -> l -> r -> TerminatedMaybe a (LeftTerminal l) (RightTerminal r)
    LeftOpen   :: a      -> r -> TerminatedMaybe a Open             (RightTerminal r)
    RightOpen  :: a -> l      -> TerminatedMaybe a (LeftTerminal l) Open

instance Semigroup a => Category (TerminatedMaybe a) where

  id = Nil

  Nil . l = l
  r . Nil = r

  OpenEnded ra    . OpenEnded la    = OpenEnded  (la <> ra)
  LeftOpen  ra rt . OpenEnded la    = LeftOpen   (la <> ra)    rt
  OpenEnded ra    . RightOpen la lt = RightOpen  (la <> ra) lt
  LeftOpen  ra rt . RightOpen la lt = CloseEnded (la <> ra) lt rt

instance Semigroup a => Semigroup (TerminatedMaybe a Open Open)
  where
    (<>) = (.)

newtype EndedFunctor l r a = EndedFunctor (TerminatedMaybe a l r)

instance Functor (EndedFunctor l r) where
  fmap f (EndedFunctor e) = EndedFunctor (terminatedMap f e)

instance Semigroup a => Semigroup (EndedFunctor Open Open a)
  where
    EndedFunctor a <> EndedFunctor b = EndedFunctor (a <> b)

terminatedMap :: (a -> b) -> TerminatedMaybe a l r -> TerminatedMaybe b l r
terminatedMap f = \case
  Nil                -> Nil
  OpenEnded  x       -> OpenEnded  (f x)
  CloseEnded x lt rt -> CloseEnded (f x) lt rt
  LeftOpen   x    rt -> LeftOpen   (f x)    rt
  RightOpen  x lt    -> RightOpen  (f x) lt
