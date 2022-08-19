module Main (main) where

import Data.Text.IO as TIO
import Lib
import Palindrome

main :: IO ()
main = do
  TIO.putStrLn "Enter a word"
  text <- TIO.getLine
  let response =
        if isPalindrome text
          then "It's a palindrome"
          else "It's not a palindrome"
  TIO.putStrLn response
