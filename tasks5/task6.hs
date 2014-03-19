-- вычисление производной полинома

data Operator = Plus | Minus

data Polynom = Monom Int Int | Polynom Polynom Operator Polynom

instance Show Operator where
    show Plus  = " + "
    show Minus = " - "

instance Show Polynom where
    show (Monom a b) | a == 0  = ""
    show (Monom a b) | b == 0  = show a 
    show (Monom a b)           = coef ++ "x" ++ pow where 
                                                    coef = if (a == 1) then "" else (show a)
                                                    pow  = if (b == 1) then "" else ("^" ++ show b) 
    show (Polynom a op b)      = p1 ++ op' ++ p2    where
                                                    p1  = show a
                                                    p2  = show b
                                                    op' = if ((p1 == "") || (p2 == "")) then "" else (show op)

diff :: Polynom -> Polynom
diff (Monom  coef pow) = Monom (coef * pow) (pow - 1)
diff (Polynom a op b) = Polynom (diff a) op (diff b)



p1 :: Polynom
p1 = Polynom (Monom 1 2) Plus (Polynom (Monom 3 1) Minus (Monom 6 0))
