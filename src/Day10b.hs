module Day10b
    where
import Data.List
import Data.Maybe

type Node = (Int,[Int])
type SumNode = (Int,Int)

paths :: [Int] -> [Node]
paths xs = zipWith4 nodes l (tail l) (tail (tail l)) (tail (tail (tail l))) 
    where
        l = sort $ 0 : m : m + 4 : m + 8 : m + 12 : xs
        m = maximum xs + 3
        nodes a b c d = (a,map (+a) (filter (<=3) [b-a,c-a,d-a])) 

totalPaths :: [Int] -> [SumNode]
totalPaths xs = foldl addPath [] nodes
    where
        nodes = (reverse (paths xs))
        addPath :: [SumNode] -> Node -> [SumNode]
        addPath tp (node,next) = 
            case next of
              [] -> (node,1):tp
              ns -> (node,sum (mapMaybe (\x -> lookup x tp) ns)) : tp

solve :: [Int] -> Maybe Int
solve xs = lookup 0 (totalPaths xs)



