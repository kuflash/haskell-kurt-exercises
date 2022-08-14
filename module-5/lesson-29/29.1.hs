allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap fn value = (pure fn) <*> value
