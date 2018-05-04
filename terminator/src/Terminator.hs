{-# LANGUAGE LambdaCase, GADTs, MultiParamTypeClasses #-}

module Terminator
  (
  -- * The Ended type
    Ended (..)

  -- * The terminators
  , Open, ClosedL, ClosedR

  -- * Functor
  , endedMap, EndedFunctor (..)

  ) where

-- base
import Control.Category
import Data.Semigroup
import Prelude hiding ((.), id)

data Open
data ClosedL l
data ClosedR r

data Ended a l r
  where
    Nil        ::                Ended a x           x
    OpenEnded  :: a           -> Ended a Open        Open
    CloseEnded :: a -> l -> r -> Ended a (ClosedL l) (ClosedR r)
    LeftOpen   :: a      -> r -> Ended a Open        (ClosedR r)
    RightOpen  :: a -> l      -> Ended a (ClosedL l) Open

instance Semigroup a => Category (Ended a)
  where
    id = Nil

    Nil . l = l
    r . Nil = r

    OpenEnded ra    . OpenEnded la    = OpenEnded  (la <> ra)
    LeftOpen  ra rt . OpenEnded la    = LeftOpen   (la <> ra)    rt
    OpenEnded ra    . RightOpen la lt = RightOpen  (la <> ra) lt
    LeftOpen  ra rt . RightOpen la lt = CloseEnded (la <> ra) lt rt

newtype EndedFunctor l r a = EndedFunctor (Ended a l r)

instance Functor (EndedFunctor l r)
  where
    fmap f (EndedFunctor e) = EndedFunctor (endedMap f e)

endedMap :: (a -> b) -> Ended a l r -> Ended b l r
endedMap f = \case
  Nil                -> Nil
  OpenEnded  x       -> OpenEnded  (f x)
  CloseEnded x lt rt -> CloseEnded (f x) lt rt
  LeftOpen   x    rt -> LeftOpen   (f x)    rt
  RightOpen  x lt    -> RightOpen  (f x) lt
