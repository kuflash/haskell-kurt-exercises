example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> ((pure (+)) <*> pure 2 <*> pure 4) <*> pure 6
