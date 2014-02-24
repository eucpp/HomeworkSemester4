-- Обращение списка

reverse_concat :: [a] -> [a] -> [a]
reverse_concat xs [] = xs
reverse_concat xs (y:ys) = reverse_concat (y:xs) ys

my_reverse :: [a] -> [a]
my_reverse xs = reverse_concat [] xs
