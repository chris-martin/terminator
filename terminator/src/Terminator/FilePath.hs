{-# LANGUAGE GADTs, TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- todo

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

import qualified Terminator.Terminals as T
import qualified Terminator.Terminated as T

-- base
import Data.Foldable (toList)
import Prelude hiding (FilePath, id, (/))

-- containers
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- text
import Data.Text (Text)
import qualified Data.Text as Text

type FilePath b t = T.Terminated (Seq DirName) b t

type Abs  = T.LeftTerminal ()
type Rel  = T.Open
type File = T.RightTerminal FileName
type Dir  = T.Open

newtype DirName = DirName PathSegment

newtype FileName = FileName PathSegment

newtype PathSegment = PathSegment Text

textPathSegmentMaybe :: Text -> Maybe PathSegment
textPathSegmentMaybe x = case (Text.unpack x) of
  "."  -> Nothing
  ".." -> Nothing
  _    -> Just (PathSegment x)

pathSegmentText :: PathSegment -> Text
pathSegmentText (PathSegment x) = x

dirNameText :: DirName -> Text
dirNameText (DirName (PathSegment x)) = x

fileNameText :: FileName -> Text
fileNameText (FileName (PathSegment x)) = x

(/) :: FilePath a b -> FilePath b c -> FilePath a c
(/) = flip (T..)

root :: FilePath Abs Dir
root = T.RightOpen Seq.empty ()

emptyPath :: FilePath Rel Dir
emptyPath = T.OpenEnded Seq.empty

dir :: DirName -> FilePath Rel Dir
dir n = T.OpenEnded (Seq.singleton n)

file :: FileName -> FilePath Rel File
file n = T.LeftOpen Seq.empty n

absFileText :: FilePath Abs File -> Text
absFileText (T.CloseEnded xs () n) =
  concatAbs ((dirNameText <$> xs) |> (fileNameText n))

relFileText :: FilePath Rel File -> Text
relFileText (T.LeftOpen xs n) =
  concatRel ((dirNameText <$> xs) |> (fileNameText n))

absDirText :: FilePath Abs Dir -> Text
absDirText (T.RightOpen xs ()) =
  concatAbs (dirNameText <$> xs)

relDirText :: FilePath Rel Dir -> Text
relDirText (T.OpenEnded xs) =
  if (Seq.null xs) then Text.pack "." else concatRel (dirNameText <$> xs)

concatAbs :: Seq Text -> Text
concatAbs = Text.concat . ((\x -> [Text.pack "/", x]) =<<) . toList @Seq

concatRel :: Seq Text -> Text
concatRel = Text.intercalate (Text.pack "/") . toList @Seq
