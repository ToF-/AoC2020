module Day20b
    where
import Data.List as L
import Data.Set as S

type Tile = (Int,[String])
type Column = [[String]]
type Mosaic = [Column]
type Coord = (Int,Int)

match :: Tile -> Tile -> Bool
match (_,t) (_,u) = last t == head u

rotate,vertFlip,horzFlip :: [[a]] -> [[a]]
rotate = reverse . transpose
vertFlip = L.map reverse
horzFlip = reverse

transformationFunctions :: [[[a]] -> [[a]]]
transformationFunctions = [ id
                          , rotate
                          , vertFlip
                          , horzFlip
                          , horzFlip . rotate
                          , vertFlip . rotate
                          , horzFlip . vertFlip
                          , horzFlip . vertFlip . rotate]

transformations :: Tile -> [Tile]
transformations (i,t) = L.map (\f -> (i,f t)) transformationFunctions

possibleMatches :: Tile -> Tile -> [(Tile,Tile)]
possibleMatches t u = 
    [(tt,tu) 
    | tt <- transformations t
    , tu <- transformations u
    , t /= u
    , tt `match` tu]

clip ::[String] -> [String] 
clip = tail . init . (L.map (tail . init))

recompose :: [[Tile]] -> [String]
recompose tts = concatMap recomposeRow tts
    where
        recomposeRow :: [Tile] -> [String]
        recomposeRow ts = assemble (L.map (clip . snd) ts)

assemble :: [[String]] -> [String]
assemble r = [concat 
                [r!!i!!j | i<-[0..length r -1]] 
             | j <- [0..length(head r)-1]]

originalRow :: [Tile] -> Maybe [Tile]
originalRow ts = find (\l -> length l == length ts) $ L.foldl discoverRow [] ts
    where
        discoverRow :: [[Tile]] -> Tile -> [[Tile]]
        discoverRow [] t = L.map return $ transformations t
        discoverRow tts t = tts >>= nextTile t

nextTile :: Tile -> [Tile] -> [[Tile]]
nextTile u rs = [ rs ++ [tu] | tu <- (transformations u), hmatch (last rs) tu]

hmatch :: Tile -> Tile -> Bool
hmatch (_,tt) (_,tu) = L.map last tt == L.map head tu

matchCount :: Tile -> [Tile] -> Int
matchCount t ts = length [u | u <- ts, u /= t, not $ L.null (possibleMatches t u)]

matchers :: Tile -> [Tile] -> [Tile]
matchers t ts = [u | u <- ts, u /= t, not $ L.null (possibleMatches t u)]

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
                              ms -> L.map ((tiles++) . return) ms
                    where
                        nextBorderTiles :: Tile -> [Tile]
                        nextBorderTiles t | matchCount t ts == 2 = []
                                         | otherwise = matchers t (((borders ts) ++ (corners ts)) `minus` tiles)

nextRows :: [Tile] -> [Tile] -> [[Tile]]
nextRows rs ts = L.map nextTiles rs 
    where
        nextTiles :: Tile -> [Tile]
        nextTiles t = matchers t (ts `minus`rs)

nextRow :: [Tile] -> [Tile] -> [Tile] -> [Tile]
nextRow rs as ts = L.foldl nextTile [] rs
    where
        nextTile :: [Tile] -> Tile -> [Tile]
        nextTile ns t = ns ++ L.filter (not . (`elem` (as ++ ns))) (matchers t ts)
                          
minus :: Eq a => [a] -> [a] -> [a]
minus xs ms = L.filter (not . (`elem` ms)) xs

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


coordsForSharps :: [String] -> [Coord]
coordsForSharps ss = [(i,j) | i <- [0..length ss -1], j <- [0..length (ss!!i) -1], ss!!i!!j == '#']

search :: [String] -> [String] -> [Coord]
search sh ss = [(i,j)  | i <- [0..length ss-1]
                              , j <- [0..length (ss!!i) -1]
                              , (setAt coordsShape i j) `isSubsetOf` pictureSet ]
    where
        coordsShape :: Set Coord
        coordsShape = S.fromList (coordsForSharps sh)

        pictureSet :: Set Coord
        pictureSet = S.fromList (coordsForSharps ss)

        setAt :: Set Coord -> Int -> Int -> Set Coord
        setAt cs i j = S.map (\(x,y) -> (x+i,y+j)) cs 

interpret :: [String] -> [Tile]
interpret text = L.map readTile gs
    where
        gs = groupBy (\_ t -> not (':' `elem` t))  (L.filter (not . L.null) text)
        readTile ss = (number (head ss),tail ss) 
            where
                number  = read . L.drop 5 . init
