harmonic n = foldl (\a b -> a + 1 / b) 0 [2 .. n]

lazyHarmonic n = sum (take n values)
  where
    range = zip (cycle [1.0]) [2.0, 3.0 ..]
    values = map (\pair -> (fst pair) / (snd pair)) range
