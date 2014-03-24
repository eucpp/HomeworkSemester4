-- поиск заданного элемента в списке (без рекурсии)

pos :: Eq a => a -> [a] -> Int
pos elem list = foldr (+) 0  $ (map (\x -> if (elem == x) then 0 else 1) list)
