import Data.Char

isPalindrome value = reverse normalizedValue == normalizedValue
  where
    normalizedValue = map toLower (filter (/= ' ') value)
