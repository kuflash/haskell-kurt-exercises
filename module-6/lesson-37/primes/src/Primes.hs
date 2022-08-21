module Primes where

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where
    noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

primes :: [Int]
primes = sieve [2 .. 10000]

isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors _ [] = []
unsafePrimeFactors n (next : rest) =
  if n `mod` next == 0
    then next : unsafePrimeFactors (n `div` next) (next : rest)
    else unsafePrimeFactors n rest

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primeLessThenN)
  where
    primeLessThenN = filter (<= n) primes
