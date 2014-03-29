-- Используя монадичесие функции, опишите фунцию, которая ищет в списке первый элемент, больший своих соседей (предыдущего и следующего)

import Control.Monad

greatest_neighboor :: Ord a => [a] -> Maybe a
greatest_neighboor [] = Nothing
greatest_neighboor (x:[]) = Nothing
greatest_neighboor (x:y:[]) = Nothing
greatest_neighboor (x:y:z:rest) = maybeY `mplus` greatest_neighboor (y:z:rest) 
                                    where maybeY =  if (y > x) && (y > z) then Just y else Nothing 


