-- Сумма цифр числа

digits_sum :: Int -> Int
digits_sum 0 = 0
digits_sum x = (digits_sum (x `div` 10)) + (x `mod` 10)

