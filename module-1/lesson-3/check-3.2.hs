doubleDouble x =
  dubs * 2
  where
    dubs = x * 2

lambdaDoubleDouble x = (\doubleX -> doubleX * 2) x * 2
