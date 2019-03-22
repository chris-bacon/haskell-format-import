module Main where

import Test.Hspec
import HaskellFormatImport.Format

main = hspec $
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

