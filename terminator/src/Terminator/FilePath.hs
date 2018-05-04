{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings, TypeApplications #-}

module Terminator.FilePath
  ( FilePath, Abs, Rel, File, Dir, (/)

  -- * Conversion to Text
  , absText, relText

  ) where

import Terminator

-- base
import Control.Category
import Data.Foldable (toList)
import Prelude hiding (FilePath, id, (.), (/))

-- containers
import Data.Sequence (Seq)

-- text
import Data.Text (Text)
import qualified Data.Text as Text

type FilePath b t = Ended (Seq Text) b t

type Abs  = ClosedL
type Rel  = Open
type File = ClosedR
type Dir  = Open

(/) :: Category cat => cat a b -> cat b c -> cat a c
(/) = flip (.)

absText :: FilePath Abs t -> Text
absText =
  \case
    CloseEnded xs -> f xs
    RightOpen  xs -> f xs
    Nil -> "/"
  where
    f = Text.concat . ((\x -> ["/", x]) =<<) . toList @Seq

relText :: FilePath Rel t -> Text
relText =
  \case
    LeftOpen  xs -> f xs
    OpenEnded xs -> f xs
    Nil -> "."
  where
    f = Text.intercalate "/" . toList @Seq
