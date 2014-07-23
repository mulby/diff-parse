{-# LANGUAGE OverloadedStrings #-}

module Text.Diff.Parse.Internal 
    ( parseDiff
    , diff
    , annotation
    , annotatedLine
    , hunk
    , fileDelta
    , fileDeltaHeader
    ) where

import Text.Diff.Parse.Types

import Control.Applicative ((<|>), (*>), (<*), (<$>), (<*>), many)
import Data.Char (isSpace)
import Data.Text (Text)

import Data.Attoparsec.Text
    ( Parser
    , parseOnly
    , many1
    , takeTill
    , char
    , string
    , option
    , isEndOfLine
    , endOfLine
    , decimal
    , endOfInput
    , letter
    , space
    )


parseDiff :: Text -> Either String FileDeltas
parseDiff input = parseOnly diff input

diff :: Parser FileDeltas
diff = many1 fileDelta <* endOfInput

fileDelta :: Parser FileDelta
fileDelta = do
    (status, source, dest) <- fileDeltaHeader
    hunks  <- many hunk
    return $ FileDelta status source dest hunks

fileDeltaHeader :: Parser (FileStatus, Text, Text)
fileDeltaHeader = do
    _      <- string "diff --git "
    source <- path <* space
    dest   <- path <* endOfLine
    status <- fileStatus
    _      <- option "" (string "index" >> takeLine)
    _      <- option "" (string "--- " >> takeLine)
    _      <- option "" (string "+++ " >> takeLine)
    return $ (status, source, dest)

takeLine :: Parser Text
takeLine = takeTill isEndOfLine <* endOfLine

fileStatus :: Parser FileStatus
fileStatus = option Modified $ ((string "new" *> return Created) <|> (string "deleted" *> return Deleted)) <* string " file mode" <* takeLine

path :: Parser Text
path = option "" (letter >> string "/") *> takeTill (\c -> (isSpace c) || (isEndOfLine c))

hunk :: Parser Hunk
hunk = Hunk <$> ("@@ -" *> range)
            <*> (" +" *> range <* " @@" <* takeLine)
            <*> (many annotatedLine)

range :: Parser Range
range = Range <$> decimal <*> (option 1 ("," *> decimal))

annotatedLine :: Parser Line
annotatedLine = Line <$> annotation <*> (takeLine <* (option "" (string "\\ No newline at end of file" <* endOfLine)))

annotation :: Parser Annotation
annotation = (char '+' >> return Added)
         <|> (char '-' >> return Removed)
         <|> (char ' ' >> return Context)
