robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, h) = h

getName robotInstance = robotInstance name

setName robotInstance newName = robotInstance (\(n, a, h) -> robot (newName, a, h))

getAttack robotInstance = robotInstance attack

setAttack robotInstance newAttack = robotInstance (\(n, a, h) -> robot (n, newAttack, h))

getHP robotInstance = robotInstance hp

setHP robotInstance newHP = robotInstance (\(n, a, h) -> robot (n, a, newHP))

printRobot robotInstance = robotInstance (\(n, a, h) -> n ++ ": attack: " ++ (show a) ++ ", health: " ++ (show h))

damage robotInstance attackDamage = robotInstance (\(n, a, h) -> robot (n, a, h - attackDamage))

fight attacker defender = damage defender attack
  where
    attack =
      if getHP attacker > 10
        then getAttack attacker
        else 0

getRobotsHP = map getHP

getWinner firstRobot secondRobot =
  if firstRobotHP > secondRobotHP
    then firstRobot
    else secondRobot
  where
    firstRobotHP = getHP firstRobot
    secondRobotHP = getHP secondRobot

-- It's not work, because infinity type error. Maybe need to learn next chapter about type system
-- threeRoundFight aRobot bRobot =
--   ( \bRobot1 ->
--       ( \aRobot1 ->
--           ( \bRobot2 ->
--               ( \aRobot2 ->
--                   ( \bRobot3 ->
--                       (\aRobot3 -> fight aRobot3 bRobot3)
--                         (fight bRobot3 aRobot2)
--                   )
--                     (fight aRobot2 bRobot2)
--               )
--                 (fight bRobot2 aRobot1)
--           )
--             (fight aRobot1 bRobot1)
--       )
--         (fight bRobot1 aRobot)
--   )
--     (fight aRobot bRobot)
