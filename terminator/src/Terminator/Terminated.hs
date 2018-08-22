{-# LANGUAGE FlexibleInstances, GADTs #-}

module Terminator.Terminated
  ( Terminated (..)
  , (<>)
  , (.)
  , map
  ) where

import Terminator.Terminals

-- base
import qualified Data.Semigroup as Semigroup
import Data.Semigroup (Semigroup)
import Text.Show (Show, showParen, showString, showsPrec)
import Prelude ((>), ($), undefined)
import qualified Prelude

data Terminated a l r
  where
    OpenEnded  :: a           -> Terminated a Open             Open
    CloseEnded :: a -> l -> r -> Terminated a (LeftTerminal l) (RightTerminal r)
    LeftOpen   :: a      -> r -> Terminated a Open             (RightTerminal r)
    RightOpen  :: a -> l      -> Terminated a (LeftTerminal l) Open

instance Semigroup a => Semigroup (Terminated a Open Open)
  where
    (<>) = (<>)

(+) :: (b -> c) -> (a -> b) -> (a -> c)
(+) = (Prelude..)

instance (Show a) => Show (Terminated a Open Open)
  where
    showsPrec d (OpenEnded a) =
      showParen (d > 10) $
        showString "OpenEnded " + showsPrec 11 a

instance (Show a, Show l, Show r) => Show (Terminated a (LeftTerminal l) (RightTerminal r))
  where
    showsPrec d (CloseEnded a l r) =
      showParen (d > 10) $
        showString "CloseEnded " + showsPrec 11 a + showString " "
          + showsPrec 11 l + showString " " + showsPrec 11 r

instance (Show a, Show l) => Show (Terminated a (LeftTerminal l) Open)
  where
    showsPrec d (RightOpen a l) =
      showParen (d > 10) $
        showString "RightOpen " + showsPrec 11 a + showString " " + showsPrec 11 l

instance (Show a, Show r) => Show (Terminated a Open (RightTerminal r))
  where
    showsPrec d (LeftOpen a r) =
      showParen (d > 10) $
        showString "LeftOpen " + showsPrec 11 a + showString " " + showsPrec 11 r

-- |
-- >>> OpenEnded "a" <> OpenEnded "b"
-- OpenEnded "ab"
--
-- >>> OpenEnded "a" <> LeftOpen "b" "x"
-- LeftOpen "ab" "x"

(<>)
  :: Semigroup x
  => Terminated x a b
  -> Terminated x b c
  -> Terminated x a c

OpenEnded la    <> OpenEnded ra    = OpenEnded  (la Semigroup.<> ra)
OpenEnded la    <> LeftOpen  ra rt = LeftOpen   (la Semigroup.<> ra)    rt
RightOpen la lt <> OpenEnded ra    = RightOpen  (la Semigroup.<> ra) lt
RightOpen la lt <> LeftOpen  ra rt = CloseEnded (la Semigroup.<> ra) lt rt

_ <> _ = undefined

(.) :: Semigroup x
    => Terminated x b c
    -> Terminated x a b
    -> Terminated x a c

f . g = g <> f

map
  :: (a -> b)
  -> Terminated a l r
  -> Terminated b l r

map f (OpenEnded  x      ) = OpenEnded  (f x)
map f (CloseEnded x lt rt) = CloseEnded (f x) lt rt
map f (LeftOpen   x    rt) = LeftOpen   (f x)    rt
map f (RightOpen  x lt   ) = RightOpen  (f x) lt
