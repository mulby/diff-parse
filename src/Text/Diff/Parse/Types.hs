module Text.Diff.Parse.Types where

import Data.Text (Text)

data Annotation = Added | Removed | Context deriving (Show, Eq)

data Line = Line {
    lineAnnotation :: Annotation
  , lineContent    :: Text
} deriving (Show, Eq)

data Range = Range {
    rangeStartingLineNumber :: Int
  , rangeNumberOfLines      :: Int
} deriving (Show, Eq)

data Hunk = Hunk {
    hunkSourceRange :: Range
  , hunkDestRange   :: Range
  , hunkLines       :: [Line]
} deriving (Show, Eq)

data Content = Binary | Hunks [Hunk] deriving (Show, Eq)

data FileStatus = Created | Deleted | Modified | Renamed deriving (Show, Eq)

data FileDelta = FileDelta {
    fileDeltaStatus     :: FileStatus
  , fileDeltaSourceFile :: Text
  , fileDeltaDestFile   :: Text
  , fileDeltaContent    :: Content
} deriving (Show, Eq)

type FileDeltas = [FileDelta]
