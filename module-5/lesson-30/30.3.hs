bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing fn = Nothing
bind (Just value) fn = fn value
