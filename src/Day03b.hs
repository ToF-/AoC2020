module Day03b
    where

relPos :: Int -> Int -> Int
relPos x w =  x `mod` w

isTree :: Int -> Int -> [String] -> Bool
isTree x y p = ((p !! y) !! i) == '#'
    where
    i = relPos x (length (head p))

countTrees :: (Int, Int) -> [String] -> Int
countTrees (w,h) p = length (filter id trees)
    where
    trees = [isTree x y p
            | (x,y) <- zip [w,w*2..] [h,h*2..length p-1]
            ]


countTreesForSlopes :: [(Int,Int)] -> [String] -> [Int]
countTreesForSlopes sls p =
    map (\sl -> countTrees sl p) sls 
