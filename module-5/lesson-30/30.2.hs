allApp :: Monad m => m (a -> b) -> m a -> m b
allApp fnM valueM = fnM >>= (\fn -> valueM >>= (\value -> pure (fn value)))
