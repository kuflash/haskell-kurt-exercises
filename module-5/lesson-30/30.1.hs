allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM fn valueM = valueM >>= (\value -> pure (fn value))
