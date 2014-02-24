-- Первая позицию вхождения заданного числа в список

pos' :: (Eq a) => a -> [a] -> Int -> Int
pos' _ [] _ = 0
pos' a (x:xs) counter = if a == x 
                        then counter
                        else pos' a xs (counter + 1)

pos :: (Eq a) => a -> [a] -> Int
pos a xs = pos' a xs 1
