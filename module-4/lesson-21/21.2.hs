fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 2
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)

fib :: Integer -> Integer
fib = fastFib 1 1

main :: IO ()
main = do
  putStrLn "Enter a number to get a fibonacci number:"
  number <- getLine
  let fibValue = fib (read number)
  putStrLn (mconcat [number, " number of fibonacci sequence = ", (show fibValue)])
