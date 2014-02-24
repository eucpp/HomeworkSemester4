-- Формирование списка степеней двойки

two_pows_creator :: Int -> [Int] -> [Int]
two_pows_creator 0 xs = 1:xs
two_pows_creator n xs = two_pows_creator (n - 1) (2^n : xs)

two_pows :: Int -> [Int]
two_pows n = two_pows_creator n []

