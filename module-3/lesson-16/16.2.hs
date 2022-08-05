type Width = Double

type Height = Double

type Radius = Double

data Shape
  = Circle Radius
  | Square Width
  | Rectangle Width Height

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square w) = w ^ 2
area (Rectangle w h) = w * h

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square w) = w * 4
perimeter (Rectangle w h) = w * 2 + h * 2
