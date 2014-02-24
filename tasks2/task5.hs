-- Проверка строки на полином

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs 
