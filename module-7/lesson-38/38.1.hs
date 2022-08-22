import Data.Char

data AddStrIntsError = InvalidFirstArg | InvalidSecondArg | InvalidArgs deriving (Show)

addStrInts :: String -> String -> Either AddStrIntsError Int
addStrInts first second
  | not (any isDigit first) && not (any isDigit second) = Left InvalidArgs
  | not (any isDigit first) = Left InvalidFirstArg
  | not (any isDigit second) = Left InvalidSecondArg
  | otherwise = Right (read first + read second)
