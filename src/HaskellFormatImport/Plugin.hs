module HaskellFormatImport.Plugin (haskellFormatImport) where

import Basement.IntegralConv   (intToInt64)
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe              (maybe, fromMaybe)
import HaskellFormatImport.Pad
import Neovim
import Neovim.API.String
import Text.Regex

haskellFormatImport :: CommandArguments -> Neovim env ()
haskellFormatImport (CommandArguments _ range _ _) = do
  let (startOfRange, endOfRange) = fromMaybe (0,0) range
  buff     <- vim_get_current_buffer
  allLines <- nvim_buf_get_lines buff (intToInt64 startOfRange) (intToInt64 endOfRange) False

  let allImportLines
        = sortImports
        . fmap (\(l,s) -> (l, ImportStatement s)) 
        . filter isImportStatement 
        . zip [LineNumber 1..LineNumber endOfRange] 
        $ allLines

      anyImportIsQualified = getQualification allImportLines
      longestModuleName    = LongestModuleName $ getLongestModuleName allImportLines

  mapM_ (formatImportLine buff anyImportIsQualified longestModuleName) allImportLines >> return ()

formatImportLine :: Buffer -> Qualification -> LongestModuleName -> (LineNumber, ImportStatement) -> Neovim env ()
formatImportLine buff qualifiedImports longestModuleName (LineNumber lineNo, ImportStatement lineContent) 
  = buffer_set_line buff (intToInt64 lineNo) $ padContent lineContent qualifiedImports longestModuleName
