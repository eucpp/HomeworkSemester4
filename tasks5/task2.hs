-- Проверить, что все элементы списка удовлетворяют некоторому условию (условие передается как параметр)

check_list :: (a -> Bool) -> [a] -> Bool
check_list f l = (length l) == (length $ filter f l)
