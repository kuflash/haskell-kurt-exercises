myHead :: [a] -> a
myHead = head

myTail :: [a] -> [a]
myTail = tail

-- There is no way to create myHead that will return value of empty list,
-- because we don't know type of empty item
