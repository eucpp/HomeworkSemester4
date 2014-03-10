-- Первая позиция в списке, на которой сумма соседних элементов максимальна

max_pos_helper :: [Int] -> Int -> Int -> Int -> Int
max_pos_helper [] _ pos _ = pos
max_pos_helper (x:[]) _ pos _ = pos
max_pos_helper (x:y:xs) max pos cur_pos = let sum = x + y in
                                  if (sum > max) then max_pos_helper (y:xs) sum cur_pos (cur_pos + 1)
                                                 else max_pos_helper (y:xs) max pos (cur_pos + 1)  

max_pos :: [Int] -> Int
max_pos l = max_pos_helper l 0 0 0
