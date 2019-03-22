{-# LANGUAGE OverloadedStrings #-}

module HaskellFormatImport.Format where

import Basement.IntegralConv (intToInt64)
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe            (maybe, fromMaybe)
import Text.Regex

-- | Qualification in this context means that one of the imports is a qualified import
-- It is a property that applies to the whole buffer, however.
data Qualification = Present | NotPresent

newtype MaxLineLength = MaxLineLength Int

newtype LineNumber = LineNumber Int

newtype ImportStatement = ImportStatement { unImportStatement :: String } deriving (Eq, Ord)

instance Enum LineNumber where
  toEnum                  = LineNumber
  fromEnum (LineNumber a) = fromEnum a

moduleNameRegex, importRegex :: Regex
moduleNameRegex = mkRegex "^import\\s[qualified]*\\s*([[:alpha:][:punct:]]+)"
importRegex     = mkRegex "^import\\s"
qualifiedRegex  = mkRegex "\\squalified\\s"

regexErrorMsg :: String -> String
regexErrorMsg s = s ++ " does not match the import regex! Please raise an issue on the github page quoting what statement it failed on"

qualifiedPadLength :: Int
qualifiedPadLength = 10

emptyQualified :: String
emptyQualified = replicate qualifiedPadLength ' '

getLongestModuleName :: [(LineNumber, ImportStatement)] -> Int
getLongestModuleName xs 
  = maximum $ fmap (fromMaybe 0 . getLengthOfModuleName . unImportStatement . snd) xs

getQualification :: [(LineNumber, ImportStatement)] -> Qualification
getQualification xs = go $ filter isQualified xs where
  go [] = NotPresent
  go _  = Present

padContent :: String -> Qualification -> Int -> Int -> String
padContent content NotPresent longestImport longestModuleName = padAsOrBrackets longestModuleName content
padContent content Present longestImport longestModuleName =
  if "qualified" `isInfixOf` content || ("import" ++ emptyQualified) `isInfixOf` content
     then padAsOrBrackets longestModuleName content
     else concat $ ("import" ++ emptyQualified) : splitOn "import" (padAsOrBrackets longestModuleName content)

getLengthOfModuleName :: String -> Maybe Int
getLengthOfModuleName s = go $ matchRegex moduleNameRegex s where
  go (Just m ) = return $ length .concat $ m
  go Nothing   = error $ regexErrorMsg s 

padAsOrBrackets :: Int -> String -> String
padAsOrBrackets n = padAs n . padBrackets n

padAs :: Int -> String -> String
padAs n s =
    let lenModName = fromMaybe 0 $ getLengthOfModuleName s
        padDiff    = n - lenModName 
     in mconcat . intersperse (replicate padDiff ' ' ++ " as ") $ splitOn " as " s

padBrackets :: Int -> String -> String
padBrackets n s =
    let lenModName = fromMaybe 0 $ getLengthOfModuleName s
        padDiff    = n - lenModName 
     in mconcat . intersperse (replicate padDiff ' ' ++ "(") $ splitOn "(" $ removeRedundantWhitespace s

removeRedundantWhitespace :: String -> String
removeRedundantWhitespace = mconcat . intersperse " " . words

sortImports :: [(LineNumber, ImportStatement)] -> [(LineNumber, ImportStatement)]
sortImports xs = zip (fmap fst xs) $ sortBy (\a b -> compare (toLower <$> ignoreQualified (unImportStatement a)) (toLower <$> ignoreQualified (unImportStatement b))) (fmap snd xs)
  where
    ignoreQualified = concat . splitOn "qualified"

isImportStatement :: (LineNumber, String) -> Bool
isImportStatement (_, s) = maybe False (const True) $ matchRegex importRegex s

isQualified :: (LineNumber, ImportStatement) -> Bool
isQualified (_, s) = maybe False (const True) $ matchRegex qualifiedRegex (unImportStatement s)

