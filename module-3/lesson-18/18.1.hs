data Box a = Box a deriving (Show)

data Triple a = Triple a a a deriving (Show)

transform :: (a -> a) -> Triple a -> Triple a
transform fn (Triple x y z) = Triple (fn x) (fn y) (fn z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap fn (Box a) = Box (fn a)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap fn (Triple x y z) = Triple (fn x) (fn y) (fn z)
