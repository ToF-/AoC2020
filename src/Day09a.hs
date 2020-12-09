module Day09a
    where
import Data.List

possibleSums :: [Int] -> Int -> [Int]
possibleSums ls n = nub $ sort $ [n+m | n <- sub, m <- sub, m /= n]
    where sub = take n ls

firstNotSum :: [Int] -> Int -> Int
firstNotSum [] n = error "there is no wrong sum in the list"
firstNotSum ls n = if ls!!n `elem` possibleSums ls n then firstNotSum (tail ls) n else ls!!n
