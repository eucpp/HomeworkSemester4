-- Вычисление n-ого числа Фиббоначи

fib :: Int -> [Int]
fib 1 = [1]
fib 2 = [1, 1]
fib x = (head list + head (tail list)) : list where list = fib (x - 1)

fib_nth :: Int -> Int
fib_nth n = head (fib n)
