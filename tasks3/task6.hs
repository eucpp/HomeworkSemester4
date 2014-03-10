-- корректность скобочной последовательности

brackets :: [Char] -> Int -> Int -> Int -> Int -> Bool
brackets [] c1 c2 c3 c4 = (c1 == 0) && (c2 == 0) && (c3 == 0) && (c4 == 0)
brackets (x:xs) c1 c2 c3 c4 = case x of 
                              '(' -> brackets xs (c1 + 1) c2 c3 c4
                              ')' -> brackets xs (c1 - 1) c2 c3 c4
                              '[' -> brackets xs c1 (c2 + 1) c3 c4
                              ']' -> brackets xs c1 (c2 - 1) c3 c4
                              '{' -> brackets xs c1 c2 (c3 + 1) c4
                              '}' -> brackets xs c1 c2 (c3 - 1) c4
                              '<' -> brackets xs c1 c2 c3 (c4 + 1)
                              '>' -> brackets xs c1 c2 c3 (c4 - 1)
                              (_)  -> brackets xs c1 c2 c3 c4

is_correct_brackets :: [Char] -> Bool
is_correct_brackets str = brackets str 0 0 0 0


