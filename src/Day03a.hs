module Day03a
    where

relPos :: Int -> Int -> Int
relPos x w =  x `mod` w

isTree :: Int -> Int -> [String] -> Bool
isTree x y p = ((p !! y) !! i) == '#'
    where
    i = relPos x (length (head p))

countTrees :: [String] -> Int
countTrees p = length (filter id trees)
    where
    trees = [isTree (y*3) y p 
            | y <- [0..length p - 1]
          ]
