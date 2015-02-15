# diff-parse

Simple Haskell library used to parse diff files. Tested with diff files produced by `git diff`.

## Usage

```haskell

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

```

## How to run tests

Master build status: [![Build Status](https://travis-ci.org/mulby/diff-parse.svg?branch=master)](https://travis-ci.org/mulby/diff-parse)

```
cabal configure --enable-tests && cabal build && cabal test
```
