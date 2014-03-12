-- три варианта функции, подсчитывающей количество четных чисел в списке

func1 :: [Int] -> Int
func1 = foldr (\x y -> ((x + 1) `mod` 2) + y) 0

func2 :: [Int] -> Int
func2 = length . filter (\x -> if (x `mod` 2 == 0) then True else False)

func3 :: [Int] -> Int
func3 = (foldr (\x y -> x + y) 0) . map (\x -> (x + 1) `mod` 2)
