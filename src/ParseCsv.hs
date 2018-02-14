module ParseCsv
  ( getShareName
  ) where

import           Data.List (drop, isPrefixOf, null, takeWhile)
import           Utils     (stripWhitespace)

getShareName :: String -> Maybe String
getShareName s
  | null (shareName s) = Nothing
  | otherwise = Just $ shareName s
  where
    shareName = stripWhitespace . takeWhile (',' /=) . tail . dropWhile (',' /=)


