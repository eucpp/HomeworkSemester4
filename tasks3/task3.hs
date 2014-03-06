-- список чисел, состоящих из 1, 7, 9

decart_helper :: [Int] -> [Int] -> [Int] -> [Int]
decart_helper [] _ _ = []
decart_helper (x:xs) [] z = decart_helper xs z z
decart_helper (x:xs) (y:ys) z = (x * 10 + y) : decart_helper (x:xs) ys z

decart :: [Int] -> [Int] -> [Int]
decart x y = decart_helper x y y

list179 = 1 : 7 : 9 : (decart list179 [1, 7, 9])
