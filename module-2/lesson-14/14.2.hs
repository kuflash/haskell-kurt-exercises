data FiveSideDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show)

class (Eq d, Enum d) => Die d where
  next :: d -> d
  roll :: Int -> d

instance Die FiveSideDie where
  next S1 = S2
  next S2 = S3
  next S3 = S4
  next S4 = S5
  next S5 = S1
  roll n = toEnum (n `mod` 5)

a = roll 3 :: FiveSideDie

b = next a
