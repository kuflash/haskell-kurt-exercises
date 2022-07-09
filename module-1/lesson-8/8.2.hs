fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib n1 _ 1 = n1
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)

fastFibFromBook _ _ 0 = 0
fastFibFromBook _ _ 1 = 1
fastFibFromBook _ _ 2 = 2
fastFibFromBook n1 n2 3 = n1 + n2
fastFibFromBook n1 n2 counter = fastFibFromBook (n1 + n2) n1 (counter - 1)
