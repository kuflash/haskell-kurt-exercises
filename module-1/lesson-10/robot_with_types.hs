data Robot = Robot {name :: String, attack :: Int, hp :: Int}

damage :: Robot -> Int -> Robot
damage robot attack = robot {hp = hp robot - attack}

fight :: Robot -> Robot -> Robot
fight attacker defender = damage defender attackValue
  where
    attackValue =
      if hp attacker > 10
        then attack attacker
        else 0

getRobotsHealth :: [Robot] -> [Int]
getRobotsHealth = map hp

getWinner :: Robot -> Robot -> Robot
getWinner firstRobot secondRobot =
  if hp firstRobot > hp secondRobot
    then firstRobot
    else secondRobot

threeRoundFight :: Robot -> Robot -> Robot
threeRoundFight aRobot bRobot =
  ( \bRobot1 ->
      ( \aRobot1 ->
          ( \bRobot2 ->
              ( \aRobot2 ->
                  ( \bRobot3 ->
                      (\aRobot3 -> getWinner aRobot3 bRobot3)
                        (fight bRobot3 aRobot2) -- 3 round
                  )
                    (fight aRobot2 bRobot2) -- 3 round
              )
                (fight bRobot2 aRobot1) -- 2 round
          )
            (fight aRobot1 bRobot1) -- 2 round
      )
        (fight bRobot1 aRobot) -- 1 round
  )
    (fight aRobot bRobot) -- 1 round

printRobot :: Robot -> String
printRobot robot =
  name robot ++ ". "
    ++ "Attack: "
    ++ show (attack robot)
    ++ ". HP: "
    ++ show (hp robot)

robot1 :: Robot
robot1 = Robot {name = "Robot 1", attack = 5, hp = 100}

robot2 :: Robot
robot2 = Robot {name = "Robot 2", attack = 10, hp = 100}

robot3 :: Robot
robot3 = Robot {name = "Robot 3", attack = 15, hp = 100}

robot4 :: Robot
robot4 = Robot {name = "Robot 4", attack = 20, hp = 100}

fightWithRobot :: Robot -> Robot
fightWithRobot = fight robot1

defenders :: [Robot]
defenders = [robot2, robot3, robot4]

robotsAfterFight :: [Robot]
robotsAfterFight = map fightWithRobot defenders

healthAfterFight :: [Int]
healthAfterFight = getRobotsHealth robotsAfterFight
