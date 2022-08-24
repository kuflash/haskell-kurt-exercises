data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)

instance FromJSON IntList

instance ToJSON IntList
