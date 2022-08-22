safeSucc :: (Eq a, Enum a, Bounded a) => a -> Maybe a
safeSucc value =
  if value == maxBound
    then Nothing
    else Just (succ value)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x : xs) = xs

safeLast :: [a] -> Either String a
safeLast [] = Left "Empty list"
safeLast xs = safeLast' 10000 xs

safeLast' :: Int -> [a] -> Either String a
safeLast' 0 _ = Left "List is exceeds the allowable limit"
safeLast' _ (x : []) = Right x
safeLast' n (x : xs) = safeLast' (n - 1) xs
