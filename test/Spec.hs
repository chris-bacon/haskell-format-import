module Main where

import Test.Hspec
import HaskellFormatImport.Pad

main = hspec $ do
  describe "padContent" $ do
    it "if input has no qualification or explicit imports, it is handled correctly" $ do
      let content        = "import Data.Text"
          qualification  = NotPresent
          longestImport  = 20
          longestModName = 10

      let expectedOutput = "import Data.Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if imports need to be padded for qualification, then that is successful" $ do
      let content        = "import Data.Text"
          qualification  = Present
          longestImport  = 20
          longestModName = 10

      let expectedOutput = "import           Data.Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if imports need to be padded for qualification, but the input string is already contained qualified, then nothing happens" $ do
      let content        = "import qualified Data.Text"
          qualification  = Present
          longestImport  = 20
          longestModName = 10

      let expectedOutput = "import qualified Data.Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains as import, then that is handled" $ do
      let content        = "import Data.Text as Text"
          qualification  = NotPresent
          longestImport  = 10
          longestModName = 2

      let expectedOutput = "import Data.Text as Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains as import, and is missing a qualified but requires qualified padding, then that is handled" $ do
      let content        = "import Data.Text as Text"
          qualification  = Present
          longestImport  = 10
          longestModName = 2

      let expectedOutput = "import           Data.Text as Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains as import, and is missing a qualified but requires qualified padding, and the longest module name is 10 long, then that is handled" $ do
      let content        = "import Data.Text as Text"
          qualification  = Present
          longestImport  = 20
          longestModName = 10

      let expectedOutput = "import           Data.Text  as Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains as import, and is missing a qualified but requires qualified padding, and the longest module name is 30 long, then that is handled" $ do
      let content        = "import Data.Text as Text"
          qualification  = Present
          longestImport  = 20
          longestModName = 30

      let expectedOutput = "import           Data.Text                      as Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains as import, and the longest module name is 30 long, then that is handled" $ do
      let content        = "import Data.Text as Text"
          qualification  = NotPresent
          longestImport  = 20
          longestModName = 30

      let expectedOutput = "import Data.Text                      as Text"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains explicit import (x) then that is handled correctly" $ do
      let content        = "import Data.Text (pack)"
          qualification  = NotPresent
          longestImport  = 20
          longestModName = 9

      let expectedOutput = "import Data.Text (pack)"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains explicit import (x) and requires qualification then that is handled correctly" $ do
      let content        = "import Data.Text (pack)"
          qualification  = Present
          longestImport  = 20
          longestModName = 9

      let expectedOutput = "import           Data.Text (pack)"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains explicit import (x) and contains qualification then that is handled correctly" $ do
      let content        = "import qualified Data.Text (pack)"
          qualification  = Present
          longestImport  = 20
          longestModName = 9

      let expectedOutput = "import qualified Data.Text (pack)"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains explicit import (x) and contains qualification and there is a longer module name than itself then that is handled correctly" $ do
      let content        = "import qualified Data.Text (pack)"
          qualification  = Present
          longestImport  = 20
          longestModName = 11

      let expectedOutput = "import qualified Data.Text   (pack)"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

    it "if input contains explicit import (x) with whitespace and contains qualification and there is a longer module name than itself then that is handled correctly" $ do
      let content        = "import qualified Data.Text ( pack )"
          qualification  = Present
          longestImport  = 20
          longestModName = 11

      let expectedOutput = "import qualified Data.Text   ( pack )"
      padContent content qualification longestImport longestModName `shouldBe` expectedOutput

  describe "isImportStatement for statements that are imports" $ do
    it "import Data.Text" $ isImportStatement (LineNumber 1, "import Data.Text") `shouldBe` True
    it "import qualified Data.Text" $ isImportStatement (LineNumber 1, "import qualified Data.Text") `shouldBe` True
    it "import qualified Data.Text as Text" $ isImportStatement (LineNumber 1, "import qualified Data.Text as Text") `shouldBe` True
    it "import Data.Text as T" $ isImportStatement (LineNumber 1, "import Data.Text as T") `shouldBe` True
    it "import Data.Text (split)" $ isImportStatement (LineNumber 1, "import Data.Text (split)") `shouldBe` True

  describe "isImportStatement for statements that are not imports" $ do
    it "" $ isImportStatement (LineNumber 1, "") `shouldBe` False
    it "f :: Int -> Int" $ isImportStatement (LineNumber 1, "f :: Int -> Int") `shouldBe` False
    it "f = length" $ isImportStatement (LineNumber 1, "f = length") `shouldBe` False
    it "--f = length" $ isImportStatement (LineNumber 1, "--f = length") `shouldBe` False
    it "data A = A" $ isImportStatement (LineNumber 1, "data A = A") `shouldBe` False
    it "-- this is a qualified import comment" $ isImportStatement (LineNumber 1, "-- this is a qualified import comment") `shouldBe` False
    it "-- this is an import comment about qualification" $ isImportStatement (LineNumber 1, "-- this is a an import comment about qualification") `shouldBe` False
      
