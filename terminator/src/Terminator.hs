{-# LANGUAGE LambdaCase, GADTs, MultiParamTypeClasses #-}

module Terminator
  ( Ended (..), Open, ClosedL, ClosedR, endedMap
  ) where

-- base
import Control.Category
import Data.Semigroup
import Prelude hiding ((.), id)

data Open
data ClosedL
data ClosedR

data Ended a l r
  where
    Nil        ::      Ended a x       x
    OpenEnded  :: a -> Ended a Open    Open
    CloseEnded :: a -> Ended a ClosedL ClosedR
    LeftOpen   :: a -> Ended a Open    ClosedR
    RightOpen  :: a -> Ended a ClosedL Open

instance Semigroup a => Category (Ended a)
  where
    id = Nil

    Nil . l = l
    r . Nil = r

    OpenEnded r . OpenEnded l = OpenEnded  (l <> r)
    LeftOpen  r . OpenEnded l = LeftOpen   (l <> r)
    OpenEnded r . RightOpen l = RightOpen  (l <> r)
    LeftOpen  r . RightOpen l = CloseEnded (l <> r)

newtype EndedFunctor l r a = EndedFunctor (Ended a l r)

instance Functor (EndedFunctor l r)
  where
    fmap f (EndedFunctor e) = EndedFunctor (endedMap f e)

endedMap :: (a -> b) -> Ended a l r -> Ended b l r
endedMap f = \case
  Nil          -> Nil
  OpenEnded  x -> OpenEnded  (f x)
  CloseEnded x -> CloseEnded (f x)
  LeftOpen   x -> LeftOpen   (f x)
  RightOpen  x -> RightOpen  (f x)
