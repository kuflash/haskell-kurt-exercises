module Main where

import qualified Data.Text.IO as TIO
import qualified Palindrome

main :: IO ()
main = do
  putStrLn "Enter a word"
  text <- TIO.getLine
  let response =
        if Palindrome.isPalindrome text
          then "It's a palindrome"
          else "It's not a palindrome"
  putStrLn response
