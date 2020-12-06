module Day06a
    where
import Data.List

collect :: [String] -> [String]
collect = filter (/= "")
        . map concat
        . groupBy (\g h -> (null g) == (null h))

count :: String -> Int
count = length . nub . sort

total :: [String] -> Int
total = sum . map count . collect
