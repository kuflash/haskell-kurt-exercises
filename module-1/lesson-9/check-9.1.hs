myRemove f [] = []
myRemove f (x : xs) = if f x then myRemove f xs else x : myRemove f xs
