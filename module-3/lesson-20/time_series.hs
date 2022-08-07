import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup

file1 :: [(Int, Double)]
file1 =
  [ (1, 200.1),
    (2, 199.5),
    (3, 199.4),
    (4, 198.9),
    (5, 199.0),
    (6, 200.2),
    (9, 200.3),
    (10, 201.2),
    (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 =
  [ (11, 201.6),
    (12, 201.5),
    (13, 201.5),
    (14, 203.5),
    (15, 204.9),
    (16, 207.1),
    (18, 210.5),
    (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 =
  [ (10, 201.2),
    (11, 201.6),
    (12, 201.5),
    (13, 201.5),
    (14, 203.5),
    (17, 210.5),
    (24, 215.1),
    (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8),
    (27, 220.5),
    (28, 223.8),
    (29, 222.8),
    (30, 223.8),
    (31, 221.7),
    (32, 222.3),
    (33, 220.8),
    (34, 219.4),
    (35, 220.1),
    (36, 220.6)
  ]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes maybeValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    maybeValues = map (\t -> Map.lookup t timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS file = createTS times values
  where
    (times, values) = unzip file

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time Nothing = mconcat [show time, "|NA\n"]
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where
      rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

calcAndInsertMaybePair :: (Ord k, Num v) => (v -> v -> v) -> Map.Map k v -> (k, Maybe v) -> Map.Map k v
calcAndInsertMaybePair fn myMap (_, Nothing) = myMap
calcAndInsertMaybePair fn myMap (key, Just value) = Map.insert key newValue myMap
  where
    oldValue = Map.lookup key myMap
    newValue =
      if isNothing oldValue
        then value
        else fn (fromJust oldValue) value

addAndInsertMaybePair :: (Ord k, Num v) => Map.Map k v -> (k, Maybe v) -> Map.Map k v
addAndInsertMaybePair = calcAndInsertMaybePair (+)

diffAndInsertMaybePair :: (Ord k, Num v) => Map.Map k v -> (k, Maybe v) -> Map.Map k v
diffAndInsertMaybePair = calcAndInsertMaybePair (-)

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    combinedValues = map (\t -> Map.lookup t updatedMap) completeTimes

addTS :: Num a => TS a -> TS a -> TS a
addTS (TS [] []) ts2 = ts2
addTS ts1 (TS [] []) = ts1
addTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl addAndInsertMaybePair tvMap (zip t2 v2)
    combinedValues = map (\t -> Map.lookup t updatedMap) completeTimes

subTS :: Num a => TS a -> TS a -> TS a
subTS (TS [] []) ts2 = ts2
subTS ts1 (TS [] []) = ts1
subTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl diffAndInsertMaybePair tvMap (zip t2 v2)
    combinedValues = map (\t -> Map.lookup t updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean numbers = total / count
  where
    total = (realToFrac . sum) numbers
    count = (realToFrac . length) numbers

median :: (Real a) => [a] -> Double
median numbers =
  if even count
    then (left + right) / 2
    else left
  where
    sortedNumbers = sort numbers
    count = length numbers
    center = count `div` 2
    left = realToFrac (sortedNumbers !! center)
    right = realToFrac (sortedNumbers !! (center + 1))

type CompareFn a = a -> a -> a

type TSCompareFn a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFn a -> TSCompareFn a
makeTSCompare fn = compareFn
  where
    compareFn (t1, Nothing) (t2, Nothing) = (t1, Nothing)
    compareFn (_, Nothing) (t2, v2) = (t2, v2)
    compareFn (t1, v1) (_, Nothing) = (t1, v1)
    compareFn (t1, Just v1) (t2, Just v2) =
      if fn v1 v2 == v1
        then (t1, Just v1)
        else (t2, Just v2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS fn (TS [] []) = Nothing
compareTS fn (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare fn) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just a) (Just b) = Just (a - b)

divPair :: Real a => Maybe a -> Maybe a -> Maybe Double
divPair Nothing _ = Nothing
divPair _ Nothing = Nothing
divPair (Just a) (Just b) = Just (realToFrac a / realToFrac b)

meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe values =
  if any (== Nothing) values
    then Nothing
    else Just avg
  where
    avg = mean (map fromJust values)

medianMaybe :: Real a => [Maybe a] -> Maybe Double
medianMaybe values =
  if any (== Nothing) values
    then Nothing
    else Just m
  where
    m = median (map fromJust values)

standardDeviationMaybe :: Real a => [Maybe a] -> Maybe Double
standardDeviationMaybe [] = Nothing
standardDeviationMaybe values =
  if all (== Nothing) values
    then Nothing
    else Just deviation
  where
    justValues = filter isJust values
    cleanValues = map fromJust justValues
    countValues = (realToFrac . length) cleanValues
    avg = mean cleanValues
    diffAvgSquares = map (\v -> (realToFrac v - avg) ^ 2) cleanValues
    sumSquares = sum diffAvgSquares
    deviation = sqrt (sumSquares / (countValues - 1.0))

meanTS :: Real a => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just avg
  where
    justValues = filter isJust values
    cleanValues = map fromJust justValues
    avg = mean cleanValues

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
  where
    shiftValues = tail values
    diffValues = zipWith diffPair shiftValues values

ratioTS :: Real a => TS a -> TS Double
ratioTS (TS [] []) = TS [] []
ratioTS (TS times values) = TS times (Nothing : ratioValues)
  where
    shiftValues = tail values
    ratioValues = zipWith (flip divPair) shiftValues values

standardDeviationTS :: Real a => TS a -> Maybe Double
standardDeviationTS (TS [] []) = Nothing
standardDeviationTS (TS times values) = standardDeviationMaybe values

createMovingFn :: Real a => ([Maybe a] -> Maybe Double) -> [Maybe a] -> Int -> [Maybe Double]
createMovingFn fn [] n = []
createMovingFn fn values n =
  if length nextValues == n
    then fn nextValues : movingAvg restValues n
    else []
  where
    nextValues = take n values
    restValues = tail values

movingAvg :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingAvg = createMovingFn meanMaybe

movingMedian :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingMedian = createMovingFn medianMaybe

createMovingTSFn :: Real a => ([Maybe a] -> Int -> [Maybe Double]) -> TS a -> Int -> TS Double
createMovingTSFn fn (TS [] []) n = TS [] []
createMovingTSFn fn (TS times values) n = TS times smoothedValues
  where
    ma = fn values n
    nothings = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [nothings, ma, nothings]

movingAverageTS :: Real a => TS a -> Int -> TS Double
movingAverageTS = createMovingTSFn movingAvg

movingMedianTS :: Real a => TS a -> Int -> TS Double
movingMedianTS = createMovingTSFn movingMedian
