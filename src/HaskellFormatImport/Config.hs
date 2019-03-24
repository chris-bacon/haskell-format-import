module HaskellFormatImport.Config where

import Text.Regex (Regex, mkRegex)

moduleNameRegex, importRegex :: Regex
moduleNameRegex = mkRegex "^import\\s[qualified]*\\s*([[:alpha:][:punct:]]+)"
importRegex     = mkRegex "^import\\s"
qualifiedRegex  = mkRegex "\\squalified\\s"

regexErrorMsg :: String -> String
regexErrorMsg s = s ++ " does not match the import regex! Please raise an issue on the github page quoting what statement it failed on"

qualifiedPadLength :: Int
qualifiedPadLength = 10
