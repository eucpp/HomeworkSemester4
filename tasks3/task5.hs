-- высота и минимально расстояние в двоичном дереве

data Tree a = Leaf a
            | Branch { left_br  :: Tree a,
                       node     :: a,
                       rignt_br :: Tree a }


tree_visitor :: Tree Int -> (Int -> Int -> Int) -> Int
tree_visitor (Leaf _) _  = 1
tree_visitor (Branch lbr val rbr) func = 1 + func (tree_visitor lbr func) (tree_visitor rbr func) 

min' :: Int -> Int -> Int
min' a b = if (a < b) then a else b

max' :: Int -> Int -> Int
max' a b = if (a < b) then b else a

tree_height :: Tree Int -> Int
tree_height tree = tree_visitor tree max'

tree_min_height :: Tree Int -> Int
tree_min_height tree = tree_visitor tree min'
