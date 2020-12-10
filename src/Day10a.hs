module Day10a
    where
import Data.List

countDiffs :: [Int] -> [(Int,Int)]
countDiffs xs = count $ zipWith (flip (-)) sl (tail sl) 
    where
        sl = sort (0 : maximum xs + 3 : xs) 
        count = map (\g -> (length g, head g)) . group . sort

solve :: [Int] -> Int
solve = product . map fst . countDiffs
