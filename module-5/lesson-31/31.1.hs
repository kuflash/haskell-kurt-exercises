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

main :: IO ()
main =
  putStrLn "Enter size of a first pizza" >> getLine
    >>= ( \size1 ->
            putStrLn "Enter cost of a first pizza" >> getLine
              >>= ( \cost1 ->
                      putStrLn "Enter size of a second pizza" >> getLine
                        >>= ( \size2 ->
                                putStrLn "Enter cost of a second pizza" >> getLine
                                  -- >>= (\cost2 -> putStrLn (describePizza (comparePizzas (read size1, read cost1) (read size2, read cost2))))
                                  >>= ( \cost2 ->
                                          ( \pizza1 ->
                                              ( \pizza2 ->
                                                  (\betterPizza -> putStrLn (describePizza betterPizza))
                                                    (comparePizzas pizza1 pizza2)
                                              )
                                                (read size2, read cost2)
                                          )
                                            (read size1, read cost1)
                                      )
                            )
                  )
        )
