-- корректность скобочной последовательности

is_open_bracket :: Char -> Bool
is_open_bracket ch = (ch == '(') || (ch == '[') || (ch == '{') || (ch == '<')

is_close_bracket :: Char -> Bool
is_close_bracket ch = (ch == ')') || (ch == ']') || (ch == '}') || (ch == '>')

is_pair_brackets :: Char -> Char -> Bool
is_pair_brackets '(' ')' = True
is_pair_brackets '[' ']' = True
is_pair_brackets '{' '}' = True
is_pair_brackets '<' '>' = True
is_pair_brackets _ _ = False


brackets :: [Char] -> [Char] -> Bool

brackets stack [] = stack == []

brackets [] (c:cs) | is_open_bracket c  = brackets (c:[]) cs
                   | is_close_bracket c = False
                   | otherwise          = brackets [] cs

brackets (x:xs) (c:cs) | is_open_bracket c       = brackets (c:x:xs) cs
                       | (is_close_bracket c) &&
                         (is_pair_brackets x c)  = brackets xs cs
                       | is_close_bracket c      = False
                       | otherwise               = brackets (x:xs) cs  

is_correct_brackets :: [Char] -> Bool
is_correct_brackets = brackets []


