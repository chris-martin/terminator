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
import Terminator.TerminatedMaybe

-- base
import Data.Semigroup
import Prelude hiding ((.), id)

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
