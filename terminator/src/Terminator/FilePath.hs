{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings, TypeApplications #-}

module Terminator.FilePath
  (
  -- The FilePath type
    FilePath, DirName, FileName

  -- * The terminators
  , Abs, Rel, File, Dir

  -- * Constructing file paths
  , (/), root, emptyPath, dir, file

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

type FilePath b t = Ended (Seq DirName) b t

type Abs  = LeftTerminal ()
type Rel  = Open
type File = RightTerminal FileName
type Dir  = Open

newtype DirName = DirName PathSegment

newtype FileName = FileName PathSegment

newtype PathSegment = PathSegment Text

textPathSegmentMaybe :: Text -> Maybe PathSegment
textPathSegmentMaybe = \case
  "." -> Nothing
  ".." -> Nothing
  x -> Just (PathSegment x)

pathSegmentText :: PathSegment -> Text
pathSegmentText (PathSegment x) = x

dirNameText :: DirName -> Text
dirNameText (DirName (PathSegment x)) = x

fileNameText :: FileName -> Text
fileNameText (FileName (PathSegment x)) = x

(/) :: FilePath a b -> FilePath b c -> FilePath a c
(/) = flip (.)

root :: FilePath Abs Dir
root = RightOpen Seq.empty ()

emptyPath :: FilePath Rel Dir
emptyPath = OpenEnded Seq.empty

dir :: DirName -> FilePath Rel Dir
dir n = OpenEnded (Seq.singleton n)

file :: FileName -> FilePath Rel File
file n = LeftOpen Seq.empty n

absFileText :: FilePath Abs File -> Text
absFileText (CloseEnded xs () n) =
  concatAbs ((dirNameText <$> xs) |> (fileNameText n))

relFileText :: FilePath Rel File -> Text
relFileText (LeftOpen xs n) =
  concatRel ((dirNameText <$> xs) |> (fileNameText n))

absDirText :: FilePath Abs Dir -> Text
absDirText (RightOpen xs ()) =
  concatAbs (dirNameText <$> xs)

relDirText :: FilePath Rel Dir -> Text
relDirText = \case
  Nil -> "."
  OpenEnded xs -> concatRel (dirNameText <$> xs)

concatAbs, concatRel :: Seq Text -> Text
concatAbs = Text.concat . ((\x -> ["/", x]) =<<) . toList @Seq
concatRel = Text.intercalate "/" . toList @Seq
