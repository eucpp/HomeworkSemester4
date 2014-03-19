-- произведение всех чисел от 1 до n

product :: Int -> [Int]
product n = l       >>=
            \x -> l >>=
            \y -> return (x * y)
            where l = [1, 2 .. n]
