countDays :: [Int]
countDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

doCalendarDays :: [Int] -> [Int]
doCalendarDays countDays = do
  days <- countDays
  day <- [1 .. days]
  return day

monadCalendarDays :: [Int] -> [Int]
monadCalendarDays countDays = countDays >>= (\days -> [1 .. days] >>= (\day -> return day))
