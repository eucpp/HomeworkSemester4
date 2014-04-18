-- вычисление производной полинома

module Derivative (Operator (Plus, Minus, Prod, Div), Polynom (Monom, Polynom), diff) where

data Operator = Plus | Minus | Prod | Div 
    deriving Eq

data Polynom = Monom Int Int | Polynom Polynom Operator Polynom
    deriving Eq

instance Show Operator where
    show Plus  = " + "
    show Minus = " - "
    show Prod  = " * "
    show Div   = " / " 

instance Show Polynom where
    show (Monom a b) | a == 0  = ""
    show (Monom a b) | b == 0  = show a 
    show (Monom a b)           = coef ++ "x" ++ pow where 
                                                    coef = if (a == 1) then "" else (show a)
                                                    pow  = if (b == 1) then "" else ("^" ++ show b) 
    show (Polynom a op b)      = br_op ++  p1 ++ br_cl ++ op' ++ br_op ++ p2 ++ br_cl
                                                    where
                                                    p1    = show a
                                                    p2    = show b
                                                    op'   = if ((p1 == "") || (p2 == "")) then "" else (show op)
                                                    br_op = if ((op == Prod) || (op == Div)) then "(" else "" 
                                                    br_cl = if ((op == Prod) || (op == Div)) then ")" else "" 

diff' :: Polynom -> Polynom
diff' (Monom  coef pow) = Monom (coef * pow) (pow - 1)
diff' (Polynom a op b) 
            | (op == Plus) || (op == Minus) = Polynom (diff' a) op (diff' b)
            | (op == Prod)             = Polynom (Polynom (diff' a) Prod b) Plus (Polynom a Prod (diff' b))
            | (op == Div)              = Polynom (Polynom (Polynom (diff' a) Prod b) Minus (Polynom a Prod (diff' b))) Div (Polynom b Prod b)

simplify' :: Polynom -> Polynom
simplify' (Monom 0 p) = Monom 0 0
simplify' (Monom c p) = Monom c p
simplify' (Polynom (Monom c1 p1) op (Monom c2 p2)) | (op == Plus) && (p1 == p2) = Monom (c1 + c2) p1
simplify' (Polynom (Monom c1 p1) op (Monom c2 p2)) | (op == Minus) && (p1 == p2) = Monom (c1 - c2) p1
simplify' (Polynom (Monom c1 p1) Prod (Monom c2 p2)) = Monom (c1 * c2) (p1 + p2)
simplify' (Polynom (Monom c1 p1) Div (Monom c2 p2)) = Polynom (Monom (c1 `div` gcd') (p1 - p2)) Div (Monom (c2 `div` gcd') 0)
                                                    where gcd' = gcd c1 c2
simplify' (Polynom p1 op p2) = Polynom (simplify' p1) op (simplify' p2)

simplify :: Polynom -> Polynom
simplify p = if (simpl_p == p) then p else (simplify simpl_p) where simpl_p = simplify' p

diff = simplify . diff'

p1 :: Polynom
p1 = Polynom (Monom 1 2) Plus (Polynom (Monom 3 1) Minus (Monom 6 0))
p2 :: Polynom
p2 = Polynom (Monom 1 1) Prod (Monom 3 2)
p3 :: Polynom 
p3 = Polynom (Monom 2 1) Div (Monom 4 2)
p4 :: Polynom
p4 = Polynom (Polynom (Monom 1 2) Plus (Monom 2 1)) Div (Polynom (Monom 1 3) Minus (Monom 1 0))
