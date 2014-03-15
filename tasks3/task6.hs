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

brackets [] (c:cs)  = if (is_open_bracket c)
                      then brackets (c:[]) cs
                      else if (is_close_bracket c)
                           then False
                           else brackets [] cs

brackets (x:xs) (c:cs) =  if (is_open_bracket c)
                          then brackets (c:x:xs) cs
                          else if (is_close_bracket c)
                               then if (is_pair_brackets x c)
                                    then brackets xs cs
                                    else False
                               else brackets (x:xs) cs
                              

is_correct_brackets :: [Char] -> Bool
is_correct_brackets = brackets []


