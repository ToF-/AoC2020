module Day20a
    where
import Data.List

type Tile = (Int,[String])
type Column = [[String]]
type Mosaic = [Column]

match :: [String] -> [String] -> Bool
match t u = last t == head u

allMatch :: Column -> Bool
allMatch ts = and $ zipWith match ts (tail ts)

rotate = reverse . transpose
vertFlip = map reverse
horzFlip = reverse

transformations :: [String] -> [[String]]
transformations t = map ($t) [ id
                             , rotate
                             , vertFlip
                             , horzFlip
                             , horzFlip . rotate
                             , vertFlip . rotate
                             , horzFlip . vertFlip
                             , horzFlip . vertFlip . rotate]

possibleMatches :: [String] -> [String] -> [([String],[String])]
possibleMatches t u = 
    [(tt,tu) 
    | tt <- transformations t
    , tu <- transformations u
    , t /= u
    , tt `match` tu]

matchCount :: [String] -> [[String]] -> Int
matchCount t ts = length [u | u <- ts, u /= t, not $ null (possibleMatches t u)]

corners :: [Tile] -> [Tile]
corners ts = [t | t <- ts, (matchCount t ts) == 2]
