data Box a = Box a deriving (Show)

instance Functor Box where
  fmap func (Box a) = Box (func a)

myBox :: Box Int
myBox = Box 1

wrapped = fmap (\box -> Box box) myBox

unwrap :: Box a -> a
unwrap (Box a) = a
