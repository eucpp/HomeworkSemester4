-- Список всех элементов дерева < 0

data Tree a = Leaf a
            | Branch { left_br :: Tree a,
                       node :: a,
                       rignt_br :: Tree a }

filter_tree_helper :: [a] -> (a -> Bool) -> Tree a -> [a]
filter_tree_helper accum func (Leaf a) = if (func a) then a:accum else accum
filter_tree_helper accum func (Branch lbr val rbr) = if (func val) then val:rec else rec
                                         where rec = filter_tree_helper (filter_tree_helper accum func lbr) func rbr

filter_tree :: (a -> Bool) -> Tree a -> [a]
filter_tree = filter_tree_helper []

filter_tree_negative = filter_tree (\x -> x < 0)
