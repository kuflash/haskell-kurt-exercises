import qualified Data.Map as Map

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "left arm for beating to face",
      cost = 1000.0,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "left arm for kind gests",
      cost = 1025.0,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "that head looks like crazy",
      cost = 5092.25,
      count = 2
    }

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

getPartCost :: Int -> Maybe Double
getPartCost id = cost <$> (Map.lookup id partsDB)

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "Not found part"
printCost (Just cost) = print (show cost)

main :: IO ()
main = do
  putStrLn "Enter an id of robot part"
  id <- readLn
  printCost (getPartCost id)
