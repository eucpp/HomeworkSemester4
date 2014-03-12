-- проверка уникальности элементов списка

import qualified Data.Set as Set

uniq ::  Ord a => [a] -> Bool
uniq l = (length l) == (Set.size (Set.fromList l))
