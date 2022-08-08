main :: IO ()
main = do
  list <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn list
