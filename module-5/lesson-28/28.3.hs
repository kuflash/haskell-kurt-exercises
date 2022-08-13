import qualified Data.Map as Map

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

partsDB :: Map.Map Int RobotPart
partsDB =
  Map.fromList
    [ (1, RobotPart "left arm" "left arm for beating to face" 1000.0 3),
      (2, RobotPart "right arm" "right arm for kind gests" 1025.0 5),
      (3, RobotPart "robot head" "that head looks like crazy" 5092.25 2),
      (4, RobotPart "left leg" "some description" 2551.0 4),
      (5, RobotPart "right leg" "some description" 2551.0 4)
    ]

getCheaperPart :: RobotPart -> RobotPart -> RobotPart
getCheaperPart part1 part2 =
  if cost part1 < cost part2
    then part1
    else part2

printRobotPart :: Maybe RobotPart -> IO ()
printRobotPart Nothing = putStrLn "Not found part"
printRobotPart (Just part) = print (show part)

main :: IO ()
main = do
  putStrLn "Enter an id of first robot part"
  firstPartId <- getLine
  putStrLn "Enter an id of second robot part"
  secondPartId <- getLine
  let firstPart = Map.lookup (read firstPartId) partsDB
  let secondPart = Map.lookup (read secondPartId) partsDB
  let cheaperPart = getCheaperPart <$> firstPart <*> secondPart
  printRobotPart cheaperPart
