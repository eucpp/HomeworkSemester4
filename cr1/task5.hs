-- тип данных для книг/журналов

data Printed =   Book {name :: String, author :: String, price :: Int}
               | Magazine {name :: String, year :: Int, num :: Int, price :: Int}

total_price :: [Printed] -> Int
total_price [] = 0
total_price (x:xs) = (price x) + (total_price xs)


book1 :: Printed
book1 = Book "Abc" "Dbf" 100
magazine1 :: Printed
magazine1 = Magazine "asdgf" 1993 12 40
test_list :: [Printed]
test_list = [book1, magazine1]
