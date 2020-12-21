module Day20b
    where
import Data.List

type Tile = (Int,[String])
type Column = [[String]]
type Mosaic = [Column]

match :: Tile -> Tile -> Bool
match (_,t) (_,u) = last t == head u


rotate,vertFlip,horzFlip :: [[a]] -> [[a]]
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

matchedTile :: Tile -> Tile -> [Tile]
matchedTile t u = tt 
    where
    tt = map fst (possibleMatches t u)

matchCount :: Tile -> [Tile] -> Int
matchCount t ts = length [u | u <- ts, u /= t, not $ null (possibleMatches t u)]

matchers :: Tile -> [Tile] -> [Tile]
matchers t ts = [u | u <- ts, u /= t, not $ null (possibleMatches t u)]

centers :: [Tile] -> [Tile]
centers ts = [t | t <- ts, (matchCount t ts) == 4]

corners :: [Tile] -> [Tile]
corners ts = [t | t <- ts, (matchCount t ts) == 2]

borders :: [Tile] -> [Tile]
borders ts = [t | t <- ts, (matchCount t ts) == 3]

borderRows:: Tile -> Tile -> [Tile] -> [[Tile]]
borderRows c b ts = borderRows' [[c,b]] 
    where
        borderRows' :: [[Tile]] -> [[Tile]]
        borderRows' tts | (tts >>= addNextTile) == tts = tts
                        | otherwise = borderRows' (tts >>= addNextTile)
        addNextTile :: [Tile] -> [[Tile]]
        addNextTile tiles = case nextBorderTiles (last tiles) of
                              [] -> return tiles
                              ms -> map ((tiles++) . return) ms
                    where
                        nextBorderTiles :: Tile -> [Tile]
                        nextBorderTiles t | matchCount t ts == 2 = []
                                         | otherwise = matchers t (((borders ts) ++ (corners ts)) `minus` tiles)

nextRows :: [Tile] -> [Tile] -> [[Tile]]
nextRows rs ts = map nextTiles rs 
    where
        nextTiles :: Tile -> [Tile]
        nextTiles t = matchers t (ts `minus`rs)

nextRow :: [Tile] -> [Tile] -> [Tile] -> [Tile]
nextRow rs as ts = foldl nextTile [] rs
    where
        nextTile :: [Tile] -> Tile -> [Tile]
        nextTile ns t = ns ++ filter (not . (`elem` (as ++ ns))) (matchers t ts)
                          
minus :: Eq a => [a] -> [a] -> [a]
minus xs ms = filter (not . (`elem` ms)) xs

image :: [Tile] -> [[Tile]]
image ts = addRows row1 ts [row1]
    where
    row1 = head (borderRows corner1 border1 ts)
    corner1 = head $ corners ts
    border1 = head $ matchers corner1 ts
    addRows :: [Tile] -> [Tile] -> [[Tile]] -> [[Tile]]
    addRows _ [] result = result
    addRows tiles rest result = addRows tiles' rest' result'
        where
            tiles' = concat (nextRows tiles rest)
            result' = result ++ [tiles']
            rest' = (rest `minus` (concat result'))



interpret :: [String] -> [Tile]
interpret text = map readTile gs
    where
        gs = groupBy (\_ t -> not (':' `elem` t))  (filter (not . null) text)
        readTile ss = (number (head ss),tail ss) 
            where
                number  = read . drop 5 . init
