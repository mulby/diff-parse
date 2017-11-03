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
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text (Text)

import Data.Attoparsec.Text
    ( Parser
    , parseOnly
    , many1
    , takeTill
    , char
    , choice
    , string
    , option
    , isEndOfLine
    , endOfLine
    , decimal
    , endOfInput
    , letter
    , space
    , try
    )


parseDiff :: Text -> Either String FileDeltas
parseDiff input = parseOnly diff input

diff :: Parser FileDeltas
diff = many1 fileDelta <* endOfInput

fileDelta :: Parser FileDelta
fileDelta = do
    (status, source, dest) <- fileDeltaHeader
    content <- try binary <|> hunks
    return $ FileDelta status source dest content

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
fileStatus = do
    _ <- option "" (string "old mode " >> takeLine)
    _ <- option "" (string "new mode " >> takeLine)
    _ <- option "" (string "similarity index " >> takeLine)
    _ <- option "" (string "rename from " >> takeLine)
    choice
        [ (string "new file mode" >> takeLine *> return Created)
        , (string "deleted file mode" >> takeLine *> return Deleted)
        , (string "rename to " >> takeLine *> return Renamed)
        , return Modified
        ]

path :: Parser Text
path = option "" (letter >> string "/") *> takeTill (\c -> (isSpace c) || (isEndOfLine c))

hunks :: Parser Content
hunks = Hunks <$> many hunk

hunk :: Parser Hunk
hunk = Hunk <$> ("@@ -" *> range)
            <*> (" +" *> range <* " @@" <* takeLine)
            <*> (many annotatedLine)

binary :: Parser Content
binary = do
    void $ string "Binary files "
        <* path <* string " and "
        <* path <* string " differ"
        <* endOfLine

    return $ Binary

range :: Parser Range
range = Range <$> decimal <*> (option 1 ("," *> decimal))

annotatedLine :: Parser Line
annotatedLine = Line <$> annotation <*> (takeLine <* (option "" (string "\\ No newline at end of file" <* endOfLine)))

annotation :: Parser Annotation
annotation = (char '+' >> return Added)
         <|> (char '-' >> return Removed)
         <|> (char ' ' >> return Context)
