-- Используя монадичесие функции, опишите фунцию, которая ищет в списке первый элемент, больший своих соседей (предыдущего и следующего)

greatest_neighboor :: Ord a => [a] -> Maybe a
greatest_neighboor [] = Nothing
greatest_neighboor (x:[]) = Nothing
greatest_neighboor (x:y:[]) = Nothing
greatest_neighboor (x:y:z:rest) 
                               | (y > x) && (y > z) = Just y
                               | otherwise          = greatest_neighboor (y:z:rest)


