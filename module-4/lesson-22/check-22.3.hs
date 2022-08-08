main :: IO ()
main = do
  userInput <- getContents
  let reversed = reverse userInput
  putStrLn reversed
