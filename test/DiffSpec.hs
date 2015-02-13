{-# LANGUAGE OverloadedStrings #-}

module DiffSpec (main, spec) where

import Data.Text (pack)

import Data.Attoparsec.Text (parseOnly)
import Test.Hspec

import Text.Diff.Parse.Types
import Text.Diff.Parse.Internal
    ( annotation
    , annotatedLine
    , hunk
    , fileDelta
    , fileDeltaHeader
    , diff
    , parseDiff
    )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "annotation" $ do
        it "should parse added lines" $ do
            (parseOnly annotation $ pack "+") `shouldBe` Right Added

        it "should parse removed lines" $ do
            (parseOnly annotation $ pack "-") `shouldBe` Right Removed

        it "should parse context" $ do
            (parseOnly annotation $ pack " ") `shouldBe` Right Context

    describe "annotatedLine" $ do
        it "should parse a complete line" $ do
            (parseOnly annotatedLine $ pack "+Foo bar baz\n") `shouldBe` Right (Line Added "Foo bar baz")

    describe "hunk" $ do
        it "should parse a hunk" $ do
            let testText = unlines [ "@@ -1,2 +4,3 @@",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z\n"]
            let expectedHunk = Hunk (Range 1 2) (Range 4 3) [ (Line Removed "x")
                                                            , (Line Added "y")
                                                            , (Line Added "w")
                                                            , (Line Context "z")
                                                            ]
            (parseOnly hunk $ pack testText) `shouldBe` Right expectedHunk

        it "should parse a hunk with header context" $ do
            let testText = unlines [ "@@ -1,2 +4,3 @@ foo bar baz",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z\n"]
            let expectedHunk = Hunk (Range 1 2) (Range 4 3) [ (Line Removed "x")
                                                            , (Line Added "y")
                                                            , (Line Added "w")
                                                            , (Line Context "z")
                                                            ]
            (parseOnly hunk $ pack testText) `shouldBe` Right expectedHunk
    
    describe "fileDeltaHeader" $ do
        it "should parse file delta header" $ do
            let testText = unlines [ "diff --git a/foo.txt b/foo.txt",
                                     "index 8a19e19..99f4dc4 100644",
                                     "--- a/foo.txt",
                                     "+++ b/foo.txt\n" ]
            (parseOnly fileDeltaHeader $ pack testText) `shouldBe` Right (Modified, "foo.txt", "foo.txt")

        it "should parse added file delta header" $ do
            let testText = unlines [ "diff --git a/foo.txt b/foo.txt",
                                    "new file mode 100644",
                                    "index 0000000..c698226",
                                     "--- /dev/null",
                                     "+++ b/foo.txt\n" ]
            (parseOnly fileDeltaHeader $ pack testText) `shouldBe` Right (Created, "foo.txt", "foo.txt")

        it "should parse removed file delta header" $ do
            let testText = unlines [ "diff --git a/foo.txt b/foo.txt",
                                    "deleted file mode 100644",
                                    "index 80ef287..0000000",
                                     "--- a/foo.txt",
                                     "+++ /dev/null\n" ]
            (parseOnly fileDeltaHeader $ pack testText) `shouldBe` Right (Deleted, "foo.txt", "foo.txt")

    describe "fileDelta" $ do
        it "should parse a file delta with multiple hunks" $ do
            let testText = unlines [ "diff --git a/foo.txt b/bar.txt",
                                     "index 8a19e19..99f4dc4 100644",
                                     "--- a/foo.txt",
                                     "+++ b/bar.txt",
                                     "@@ -1,2 +4,3 @@",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z",
                                     "@@ -1,2 +4,3 @@",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z"]
            let expectedHunk = Hunk (Range 1 2) (Range 4 3) [ (Line Removed "x")
                                                            , (Line Added "y")
                                                            , (Line Added "w")
                                                            , (Line Context "z")
                                                            ]
            (parseOnly fileDelta $ pack testText) `shouldBe` Right (FileDelta Modified "foo.txt" "bar.txt" $ Hunks [expectedHunk, expectedHunk])

        it "should parse a diff with multiple file deltas" $ do
            let testText = unlines [ "diff --git a/foo.txt b/bar.txt",
                                     "index 8a19e19..99f4dc4 100644",
                                     "--- a/foo.txt",
                                     "+++ b/bar.txt",
                                     "@@ -1,2 +4,3 @@",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z",
                                     "@@ -1,2 +4,3 @@",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z",
                                     "diff --git a/foo.txt b/bar.txt",
                                     "index 8a19e19..99f4dc4 100644",
                                     "--- a/foo.txt",
                                     "+++ b/bar.txt",
                                     "@@ -1,2 +4,3 @@",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z",
                                     "@@ -1,2 +4,3 @@",
                                     "-x",
                                     "+y",
                                     "+w",
                                     " z"]
            let expectedHunk = Hunk (Range 1 2) (Range 4 3) [ (Line Removed "x")
                                                            , (Line Added "y")
                                                            , (Line Added "w")
                                                            , (Line Context "z")
                                                            ]
            let expectedFileDelta = FileDelta Modified "foo.txt" "bar.txt" $ Hunks [expectedHunk, expectedHunk]
            (parseOnly diff $ pack testText) `shouldBe` Right ([expectedFileDelta, expectedFileDelta])

    describe "parseDiff" $ do

        it "should parse a complex diff" $ do
            let testText = unlines ["diff --git a/bar.txt b/bar.txt",
                                    "deleted file mode 100644",
                                    "index 363a6c1..0000000",
                                    "--- a/bar.txt",
                                    "+++ /dev/null",
                                    "@@ -1 +0,0 @@",
                                    "-bar 1",
                                    "diff --git a/baz.txt b/baz.txt",
                                    "deleted file mode 100644",
                                    "index 80ef287..0000000",
                                    "--- a/baz.txt",
                                    "+++ /dev/null",
                                    "@@ -1,2 +0,0 @@",
                                    "-baz 1",
                                    "-baz 2",
                                    "diff --git a/foo.txt b/foo.txt",
                                    "index 9c2a709..9254400 100644",
                                    "--- a/foo.txt",
                                    "+++ b/foo.txt",
                                    "@@ -1,4 +1,5 @@",
                                    "+line 0",
                                    " line 1",
                                    "-line 2",
                                    " line 3",
                                    "+line 3.5",
                                    " line 4",
                                    "\\ No newline at end of file",
                                    "diff --git a/empty.txt b/empty.txt",
                                    "new file mode 100644",
                                    "index 0000000..c698226",
                                    "diff --git a/renamed.txt b/renamed.txt",
                                    "new file mode 100644",
                                    "index 0000000..c698226",
                                    "--- /dev/null",
                                    "+++ b/renamed.txt",
                                    "@@ -0,0 +1,3 @@",
                                    "+baz 1",
                                    "\\ No newline at end of file",
                                    "+baz 10",
                                    "+baz 12"]

            let barDiff = FileDelta Deleted "bar.txt" "bar.txt" (Hunks [Hunk (Range 1 1) (Range 0 0) [Line Removed "bar 1"]])
                bazDiff = FileDelta Deleted "baz.txt" "baz.txt" (Hunks [Hunk (Range 1 2) (Range 0 0) [ Line Removed "baz 1"
                                                                                                , Line Removed "baz 2"
                                                                                                ]])
                fooDiff = FileDelta Modified "foo.txt" "foo.txt" (Hunks [Hunk (Range 1 4) (Range 1 5) [ Line Added "line 0"
                                                                                               , Line Context "line 1"
                                                                                               , Line Removed "line 2"
                                                                                               , Line Context "line 3"
                                                                                               , Line Added "line 3.5"
                                                                                               , Line Context "line 4"
                                                                                               ]])
                renamedDiff = FileDelta Created "renamed.txt" "renamed.txt" (Hunks [Hunk (Range 0 0) (Range 1 3) [ Line Added "baz 1"
                                                                                                        , Line Added "baz 10"
                                                                                                        , Line Added "baz 12"
                                                                                                        ]])
                emptyDiff = FileDelta Created "empty.txt" "empty.txt" $ Hunks []
            (parseDiff $ pack testText) `shouldBe` Right [barDiff, bazDiff, fooDiff, emptyDiff, renamedDiff]

        it "should parse binary diffs" $ do
            let testText = unlines ["diff --git a/binary.png b/binary.png",
                                    "new file mode 100644",
                                    "index 0000000..363a6c1",
                                    "Binary files /dev/null and b/binary.png differ"]

                binaryDiff = FileDelta Created "binary.png" "binary.png" Binary

            (parseDiff $ pack testText) `shouldBe` Right [binaryDiff]

        it "should parse a file delta with a mode change" $ do
            let testText = unlines [ "diff --git a/foo.txt b/bar.txt",
                                     "old mode 100644",
                                     "new mode 100755",
                                     "index fcd15ac..f4ca464",
                                     "--- a/foo.txt",
                                     "+++ b/bar.txt",
                                     "@@ -1 +1 @@",
                                     "-abc",
                                     "+abcd"]

                hunklessDiff = FileDelta Modified "foo.txt" "bar.txt" $ Hunks [Hunk (Range 1 1) (Range 1 1) [Line Removed "abc", Line Added "abcd"]]

            (parseDiff $ pack testText) `shouldBe` Right [hunklessDiff]
