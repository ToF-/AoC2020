module Day09b
    where
import Data.List

possibleSums :: [Int] -> Int -> [Int]
possibleSums ls n = nub $ sort $ [n+m | n <- sub, m <- sub, m /= n]
    where sub = take n ls

firstNotSum :: [Int] -> Int -> Int
firstNotSum [] n = error "there is no wrong sum in the list"
firstNotSum ls n = if ls!!n `elem` possibleSums ls n then firstNotSum (tail ls) n else ls!!n

findSubArray :: [Int] -> Int -> (Int,Int)
findSubArray xs t = sumSubArray (0,0,head xs) (tail xs) xs
    where
        sumSubArray (l,h,s) [] (y:ys) =
            case compare s t of
              EQ -> (l,h)
              LT -> error "target sum not in array"
              GT -> sumSubArray (l+1,h,s-y) [] ys
        sumSubArray (l,h,s) (x:xs) (y:ys) = 
            case compare s t of
              EQ -> (l,h)
              LT -> sumSubArray (l,h+1,s+x) xs (y:ys)
              GT -> sumSubArray (l+1,h,s-y) (x:xs) ys

solve :: [Int] -> Int -> Int 
solve xs n = minimum sl + maximum sl
    where 
        sl = take (h-l) $ drop l xs
        (l,h) = findSubArray xs (firstNotSum xs n)
                

