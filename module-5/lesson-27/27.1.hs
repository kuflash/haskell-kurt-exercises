data Box a = Box a deriving (Show)

instance Functor Box where
  fmap func (Box a) = Box (func a)

morePresent :: Int -> Box a -> Box [a]
morePresent count box = (take 5) . repeat <$> box

box :: Box Int
box = Box 1
