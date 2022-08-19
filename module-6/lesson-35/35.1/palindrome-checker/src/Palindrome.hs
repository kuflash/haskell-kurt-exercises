module Palindrome
  ( isPalindrome,
  )
where

import Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Text as T

stripWhiteSpace :: String -> String
stripWhiteSpace = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowerCase :: String -> String
toLowerCase = map toLower

preprocess :: T.Text -> T.Text
preprocess = T.pack . stripWhiteSpace . stripPunctuation . toLowerCase . T.unpack

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preprocess text
