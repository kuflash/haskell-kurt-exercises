cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc a =
  if a == maxBound
    then minBound
    else succ a
