-- свёртка двоичного дерева

data Tree a = Leaf a
            | Branch { left_br  :: Tree a,
                       node     :: a,
                       rignt_br :: Tree a }

foldr_tree :: (a -> b -> b) -> b -> Tree a -> b
foldr_tree func x (Leaf a) = func a x
foldr_tree func x (Branch lbr val rbr) = func val accum 
                                         where accum = foldr_tree func (foldr_tree func x lbr) rbr
