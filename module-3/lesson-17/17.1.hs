data Color
  = White
  | Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) color White = color
  (<>) White color = color
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Yellow, Red, Orange]) [a, b] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = White
  mappend = (<>)
