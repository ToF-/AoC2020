module Day06b
    where
import Data.List
import Data.Char
import Data.Bits

collect :: [String] -> [[String]]
collect = filter (/= [""])
        . groupBy (\g h -> (null g) == (null h))

count :: [String] -> Int
count = popCount . foldl (.|.) zeroBits . map binary

total :: [String] -> Int
total = sum . map count . collect

binary :: String -> Int
binary = foldl accum zeroBits
    where 
    accum a c = a `setBit` (ord c - ord 'a')
