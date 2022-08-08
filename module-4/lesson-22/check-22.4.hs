toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let ints = toInts userInput
  print (sum (map (^ 2) ints))
