{-# LANGUAGE LambdaCase, FlexibleInstances, GADTs, MultiParamTypeClasses #-}

module Terminator
  (
  -- * The Ended type
    Ended (..)

  -- * The terminators
  , Open, L, R

  -- * Functor
  , endedMap, EndedFunctor (..)

  ) where

-- base
import Control.Category
import Data.Semigroup
import Prelude hiding ((.), id)

data Open
data L l
data R r

data Ended a l r
  where
    Nil        ::                Ended a x     x
    OpenEnded  :: a           -> Ended a Open  Open
    CloseEnded :: a -> l -> r -> Ended a (L l) (R r)
    LeftOpen   :: a      -> r -> Ended a Open  (R r)
    RightOpen  :: a -> l      -> Ended a (L l) Open

instance Semigroup a => Category (Ended a) where

  id = Nil

  Nil . l = l
  r . Nil = r

  OpenEnded ra    . OpenEnded la    = OpenEnded  (la <> ra)
  LeftOpen  ra rt . OpenEnded la    = LeftOpen   (la <> ra)    rt
  OpenEnded ra    . RightOpen la lt = RightOpen  (la <> ra) lt
  LeftOpen  ra rt . RightOpen la lt = CloseEnded (la <> ra) lt rt

instance Semigroup a => Semigroup (Ended a Open Open)
  where
    (<>) = (.)

newtype EndedFunctor l r a = EndedFunctor (Ended a l r)

instance Functor (EndedFunctor l r) where
  fmap f (EndedFunctor e) = EndedFunctor (endedMap f e)

instance Semigroup a => Semigroup (EndedFunctor Open Open a)
  where
    EndedFunctor a <> EndedFunctor b = EndedFunctor (a <> b)

endedMap :: (a -> b) -> Ended a l r -> Ended b l r
endedMap f = \case
  Nil                -> Nil
  OpenEnded  x       -> OpenEnded  (f x)
  CloseEnded x lt rt -> CloseEnded (f x) lt rt
  LeftOpen   x    rt -> LeftOpen   (f x)    rt
  RightOpen  x lt    -> RightOpen  (f x) lt
