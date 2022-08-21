import Data.Maybe
import Primes
import Test.QuickCheck

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val =
  if val < 2 || val >= length primes
    then isNothing result
    else isJust result
  where
    result = isPrime val

prop_primesArePrime :: Int -> Bool
prop_primesArePrime val =
  if result == Just True
    then length divisors == 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val =
  if result == Just False
    then length divisors > 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val =
  if isNothing result
    then True
    else product (fromJust result) == val
  where
    result = primeFactors val

prop_allFactorsIsPrime :: Int -> Bool
prop_allFactorsIsPrime val =
  if isNothing result
    then True
    else all (== Just True) resultPrimes
  where
    result = primeFactors val
    resultPrimes = map isPrime (fromJust result)

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs {maxSuccess = 1000} prop_primesArePrime
  quickCheckWith stdArgs {maxSuccess = 1000} prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsIsPrime
