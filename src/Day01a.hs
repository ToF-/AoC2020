module Day01a
    where

twoTerms :: [Int] -> (Int,Int)
twoTerms ns = head [(x,y)| x <- ns, y <- ns, x + y == 2020]
