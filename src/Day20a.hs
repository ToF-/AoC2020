module Day20a
    where
import Data.List

type Tile = (Int,[String])
type Column = [[String]]
type Mosaic = [Column]

match :: Tile -> Tile -> Bool
match (_,t) (_,u) = last t == head u


rotate = reverse . transpose
vertFlip = map reverse
horzFlip = reverse


transformations :: Tile -> [Tile]
transformations (i,t) = map (\f -> (i,f t)) 
                            [ id
                            , rotate
                            , vertFlip
                            , horzFlip
                            , horzFlip . rotate
                            , vertFlip . rotate
                            , horzFlip . vertFlip
                            , horzFlip . vertFlip . rotate]

possibleMatches :: Tile -> Tile -> [(Tile,Tile)]
possibleMatches t u = 
    [(tt,tu) 
    | tt <- transformations t
    , tu <- transformations u
    , t /= u
    , tt `match` tu]

matchCount :: Tile -> [Tile] -> Int
matchCount t ts = length [u | u <- ts, u /= t, not $ null (possibleMatches t u)]

corners :: [Tile] -> [Tile]
corners ts = [t | t <- ts, (matchCount t ts) == 2]

interpret :: [String] -> [Tile]
interpret ss = map readTile gs
    where
        gs = groupBy (\s t -> not (':' `elem` t))  (filter (not . null) ss)
        readTile ss = (number (head ss),tail ss) 
            where
                number  = read . drop 5 . init
