{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings, TypeApplications #-}

module Terminator.FilePath
  (
  -- The FilePath type
    FilePath

  -- * The terminators
  , Abs, Rel, File, Dir

  -- * Constructing file paths
  , (/), emptyPath

  -- * Conversion to Text
  , absFileText, absDirText, relFileText, relDirText

  ) where

import Terminator

-- base
import Control.Category
import Data.Foldable (toList)
import Prelude hiding (FilePath, id, (.), (/))

-- containers
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- text
import Data.Text (Text)
import qualified Data.Text as Text

type FilePath b t = Ended (Seq Text) b t

type Abs  = ClosedL ()
type Rel  = Open
type File = ClosedR Text
type Dir  = Open

(/) :: FilePath a b -> FilePath b c -> FilePath a c
(/) = flip (.)

emptyPath :: FilePath Rel Dir
emptyPath = OpenEnded Seq.empty

absFileText :: FilePath Abs File -> Text
absFileText (CloseEnded xs () basename) = concatAbs (xs |> basename)

relFileText :: FilePath Rel File -> Text
relFileText (LeftOpen xs basename) = concatRel (xs |> basename)

absDirText :: FilePath Abs Dir -> Text
absDirText (RightOpen xs ()) = concatAbs xs

relDirText :: FilePath Rel Dir -> Text
relDirText = \case { Nil -> "."; OpenEnded xs -> concatRel xs }

concatAbs, concatRel :: Seq Text -> Text
concatAbs = Text.concat . ((\x -> ["/", x]) =<<) . toList @Seq
concatRel = Text.intercalate "/" . toList @Seq
