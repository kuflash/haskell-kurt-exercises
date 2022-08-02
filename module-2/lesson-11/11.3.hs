myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x

-- nativeFoldl :: (b -> a -> b) -> b -> GHC.Types.Any a -> b
