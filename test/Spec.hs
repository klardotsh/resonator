-- This source code is part of the resonator project, licensed under the CC0-1.0
-- license found in the COPYING file in the root directory of this source tree,
-- or at https://creativecommons.org/publicdomain/zero/1.0/
module Main
    ( main
    ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Config           ( ConfigurationLibraryGrouping (..) )
import           Library          ( groupableTag )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [libraryGrouping]

-- most of these tests verify the claims made in the example config.toml and
-- elsewhere in the documentation, though there's a few extra cases thrown in
-- for good measure
libraryGrouping :: TestTree
libraryGrouping =
    testGroup
        "Library.groupableTag"
        [ testCase "does nothing unless opted in (unanimous opt-out)" $
          (@?=)
              (groupableTag allFalseConfig "Panic! At The Disco")
              "Panic! At The Disco"
        , testCase "does nothing unless opted in (groupingEnabled override)" $
          (@?=)
              (groupableTag optedInButOverridden "Panic! At The Disco")
              "Panic! At The Disco"
        , testCase "articles can be ignored (1/2)" $
          (@?=)
              (groupableTag optedInIgnoreArticles "The Dirty Heads")
              (groupableTag optedInIgnoreArticles "Dirty Heads")
        , testCase "articles can be ignored (2/2)" $
          (@?=)
              (groupableTag optedInIgnoreArticles "The Beatles")
              (groupableTag optedInIgnoreArticles "Beatles The")
        , testCase
              "quirk: ignoring articles implicitly removes duplicate spaces (1/2)" $
          (@?=)
              (groupableTag optedInIgnoreArticles "The  Dirty Heads")
              (groupableTag optedInIgnoreArticles "Dirty Heads")
        , testCase
              "quirk: ignoring articles implicitly removes duplicate spaces (2/2)" $
          (@?=)
              (groupableTag optedInIgnoreArticles "The Dirty       Heads")
              (groupableTag optedInIgnoreArticles "Dirty Heads")
        , testCase "can be case-insensitive" $
          (@?=)
              (groupableTag optedInIgnoreCase "Bullet For My Valentine")
              (groupableTag optedInIgnoreCase "Bullet for my Valentine")
        , testCase "can ignore spaces (1/2)" $
          (@?=)
              (groupableTag optedInIgnoreSpaces "Purity Ring")
              (groupableTag optedInIgnoreSpaces "PurityRing")
        , testCase "can ignore spaces (2/2)" $
          (@?=)
              (groupableTag optedInIgnoreSpaces "Purity Ring")
              (groupableTag optedInIgnoreSpaces "Purity       Ring")
        , testCase "can ignore special characters (1/3)" $
          (@?=)
              (groupableTag optedInIgnoreSpecialChars "ACDC")
              (groupableTag optedInIgnoreSpecialChars "AC/DC")
        , testCase "can ignore special characters (2/3)" $
          (@?=)
              (groupableTag optedInIgnoreSpecialChars "Panic at the Disco")
              (groupableTag optedInIgnoreSpecialChars "Panic! at the Disco")
        , testCase "can ignore special characters (3/3)" $
          (@?=)
              (groupableTag optedInIgnoreSpecialChars "Hello - World")
              (groupableTag optedInIgnoreSpecialChars "Hello + World")
        , testCase "can combine all transformations to parse crazy stuff" $
          (@?=)
              (groupableTag
                   allTrueConfig
                   "The Big Bad Band & A Really Cool Friend!")
              (groupableTag
                   allTrueConfig
                   "A Big Bad Band / The Really Cool Friend")
        , testCase
              "... but won't do so unless asked - transformations are independent (1/2)" $
          (@?=) (groupableTag optedInIgnoreArticles "The Panic!") "Panic!"
        , testCase
              "... but won't do so unless asked - transformations are independent (2/2)" $
          (@?=) (groupableTag optedInIgnoreSpaces "The    Panic!") "ThePanic!"
        ]
  where
    allFalseConfig =
        ConfigurationLibraryGrouping
            { groupingEnabled = False
            , ignoreArticles = False
            , ignoreCase = False
            , ignoreSpaces = False
            , ignoreSpecialCharacters = False
            , groupByAlbumArtist = False
            }
    allTrueConfig =
        ConfigurationLibraryGrouping
            { groupingEnabled = True
            , ignoreArticles = True
            , ignoreCase = True
            , ignoreSpaces = True
            , ignoreSpecialCharacters = True
            , groupByAlbumArtist = True
            }
    optedInButOverridden = allFalseConfig {ignoreSpaces = True}
    optedInIgnoreArticles =
        allFalseConfig {groupingEnabled = True, ignoreArticles = True}
    optedInIgnoreCase =
        allFalseConfig {groupingEnabled = True, ignoreCase = True}
    optedInIgnoreSpaces =
        allFalseConfig {groupingEnabled = True, ignoreSpaces = True}
    optedInIgnoreSpecialChars =
        allFalseConfig {groupingEnabled = True, ignoreSpecialCharacters = True}
