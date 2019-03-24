{-# LANGUAGE OverloadedStrings #-}

module HaskellFormatImport.Pad where

import Basement.IntegralConv      (intToInt64)
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe                 (maybe, fromMaybe)
import Text.Regex                 (matchRegex)

import HaskellFormatImport.Config (moduleNameRegex, importRegex, regexErrorMsg, qualifiedPadLength)

-- | Qualification here means one of the imports is a qualified import
-- It is a property that applies to the whole buffer
data Qualification = Present | NotPresent

newtype LongestModuleName = LongestModuleName Int

newtype LineNumber = LineNumber Int

newtype ImportStatement = ImportStatement { unImportStatement :: String } deriving (Eq, Ord)

instance Enum LineNumber where
  toEnum                  = LineNumber
  fromEnum (LineNumber a) = fromEnum a

padContent :: String -> Qualification -> LongestModuleName -> String
padContent content NotPresent (LongestModuleName longestModuleName) = padAsOrBrackets longestModuleName content
padContent content Present    (LongestModuleName longestModuleName) =
  if "qualified" `isInfixOf` content || ("import" ++ emptyQualified) `isInfixOf` content
     then padAsOrBrackets longestModuleName content
     else concat $ ("import" ++ emptyQualified) : splitOn "import" (padAsOrBrackets longestModuleName content)

emptyQualified :: String
emptyQualified = replicate qualifiedPadLength ' '

getLongestModuleName :: [(LineNumber, ImportStatement)] -> Int
getLongestModuleName xs 
  = maximum $ fmap (fromMaybe 0 . getLengthOfModuleName . unImportStatement . snd) xs

getQualification :: [(LineNumber, ImportStatement)] -> Qualification
getQualification xs = go $ filter isQualified xs where
  go [] = NotPresent
  go _  = Present

getLengthOfModuleName :: String -> Maybe Int
getLengthOfModuleName s = go $ matchRegex moduleNameRegex s where
  go (Just m ) = return $ length .concat $ m
  go Nothing   = error $ regexErrorMsg s 

padAsOrBrackets :: Int -> String -> String
padAsOrBrackets n = pad " as " n . pad "(" n . removeRedundantWhitespace 

pad :: String -> Int -> String -> String
pad this n s =
    let padDiff = n - (fromMaybe 0 $ getLengthOfModuleName s)
     in mconcat . intersperse (replicate padDiff ' ' ++ this) $ splitOn this s

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
