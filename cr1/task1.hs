-- бесконечная последовательность простых чисел



prime_helper :: [Int] -> Int -> [Int]
prime_helper l 1 = l
prime_helper l n = prime_helper (filter (\x -> (x <= n) || (x `mod` n /= 0)) l) (n - 1)

prime :: Int -> [Int]
prime n = prime_helper [1 .. n] (n - 1)
