myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM repeatsCount ioFn = mapM (\_ -> ioFn) [1 .. repeatsCount]
