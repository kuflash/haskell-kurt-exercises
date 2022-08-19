module Main (main) where

import Lib (comparePizzas, describePizza)

main :: IO ()
main = do
  putStrLn "Enter size of a first pizza"
  size1 <- getLine
  putStrLn "Enter cost of a first pizza"
  cost1 <- getLine
  putStrLn "Enter size of a second pizza"
  size2 <- getLine
  putStrLn "Enter cost of a second pizza"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)
