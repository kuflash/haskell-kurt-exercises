maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap fn (Just a) = Just (fn a)

doubleMaybeNumber :: Maybe Int -> Maybe Int
doubleMaybeNumber = maybeMap (\x -> x * 2)
