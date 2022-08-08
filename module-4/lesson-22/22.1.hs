import Data.List.Split

parseExpression :: String -> (String, Int, Int)
parseExpression expression = (fn, x, y)
  where
    chars = splitOn " " expression
    x = read (chars !! 0)
    y = read (chars !! 2)
    fn = chars !! 1

parseExpressions :: [String] -> [(String, Int, Int)]
parseExpressions = map parseExpression

invokeExpression :: (String, Int, Int) -> Int
invokeExpression ("+", x, y) = x + y
invokeExpression ("*", x, y) = x * y
invokeExpression (_, x, y) = 0

invokeExpressions :: [(String, Int, Int)] -> [Int]
invokeExpressions = map invokeExpression

main :: IO ()
main = do
  input <- getContents
  let expressions = lines input
  mapM_ print (invokeExpressions (parseExpressions expressions))
