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

pizzas :: [Pizza]
pizzas = [(30, 150), (50, 220), (40, 210), (45, 180)]

getCheaperPizza :: Monad m => m Pizza -> m String
getCheaperPizza pizzas = do
  pizza1 <- pizzas
  pizza2 <- pizzas
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
