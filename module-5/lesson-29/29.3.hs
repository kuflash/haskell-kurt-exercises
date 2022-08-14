packageSize :: [Int]
packageSize = [6, 12]

remainingBottles :: [Int]
remainingBottles = (-) <$> packageSize <*> pure 4

friendsCount :: [Int]
friendsCount = [2, 3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> friendsCount

bottlePerPerson :: [Int]
bottlePerPerson = [3, 4]

possibleDrinkBeersCount :: [Int]
possibleDrinkBeersCount = (*) <$> totalPeople <*> bottlePerPerson

possibleRequiredBottlesCount :: [Int]
possibleRequiredBottlesCount = (-) <$> possibleDrinkBeersCount <*> remainingBottles

requiredBottlesCount :: Int
requiredBottlesCount = foldl max 0 possibleRequiredBottlesCount
