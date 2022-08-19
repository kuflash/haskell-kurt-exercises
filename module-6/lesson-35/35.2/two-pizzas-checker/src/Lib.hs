module Lib
  ( comparePizzas,
    describePizza,
  )
where

import qualified Data.Map as Map

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costPerCm p1 < costPerCm p2
    then p1
    else p2

describePizza :: Pizza -> String
describePizza (size, cost) = "Pizza with size " ++ show size ++ " is cheaper at " ++ show (costPerCm (size, cost))

costData :: Map.Map Int Double
costData = Map.fromList [(1, 150), (2, 220)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 30), (2, 50)]
