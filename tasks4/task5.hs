-- программа для работы с сортированным списком

import Data.String

insert :: Ord a => a -> [a] -> [a]
insert a [] = (a:[])
insert a list@(x:xs) = if (a < x) then a:list else x:(insert a xs)

remove :: Ord a => a -> [a] -> [a]
remove a [] = []
remove a list@(x:xs) = if (a == x) then xs else x:(remove a xs)

do_loop :: [Int] -> IO ()
do_loop list = 
               putStrLn "Enter a command;" >>
               putStrLn "aElem - add new element to list;" >>
               putStrLn "rElem - remove element from list;" >>
               putStrLn "p - print list;" >>
               putStrLn "q - quit; " >>
               getLine >>=
               \cmd -> case cmd of 
                         'a':elem -> do_loop (insert (read elem :: Int) list)
                         'r':elem -> do_loop (remove (read elem :: Int) list)
                         'p':_    -> putStrLn (show list) >>
                                     do_loop list
                         'q':_    -> return ()
                         'd':_    -> do_loop list
                         (_)      -> putStrLn "Unknow command " >>
                                     do_loop list
                       

main = do_loop [] 
