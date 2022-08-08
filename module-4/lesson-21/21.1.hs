import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = mconcat ["Hello, ", name, "!"]

iodata :: Map.Map String String
iodata = Map.fromList [("name", "Igor")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup "name" iodata
  let statement = helloPerson name
  return statement
