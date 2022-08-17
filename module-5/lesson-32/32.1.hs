countDays :: [Int]
countDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

calendarDays :: [Int] -> [Int]
calendarDays countDays = [day | days <- countDays, day <- [1 .. days]]
