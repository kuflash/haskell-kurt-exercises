module Main (main) where

import Primes

printIsPrimeResult :: Maybe Bool -> IO ()
printIsPrimeResult Nothing = putStrLn "Not found number"
printIsPrimeResult (Just True) = putStrLn "It's a prime"
printIsPrimeResult (Just False) = putStrLn "It's not a prime"

printFactorsResult :: Maybe [Int] -> IO ()
printFactorsResult Nothing = putStrLn "Can't find factors"
printFactorsResult (Just factors) = putStrLn ("Factors: " ++ show factors)

isPrimeIO :: IO ()
isPrimeIO = do
  putStrLn "Enter a number to check for simplicity"
  input <- getLine
  let value = read input
  let result = isPrime value
  printIsPrimeResult result

primeFactorsIO :: IO ()
primeFactorsIO = do
  putStrLn "Enter a number to get factors"
  input <- getLine
  let value = read input
  let result = primeFactors value
  printFactorsResult result

main :: IO ()
main = do
  isPrimeIO
  primeFactorsIO
