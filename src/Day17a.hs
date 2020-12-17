module Day17a
    where

import Data.Set as S
import Data.List.Split
import Data.List as L

type Coord = (Int,Int,Int)
type Grid = S.Set Coord

neighbors :: Coord -> [Coord]
neighbors (x,y,z) = 
    [(i,j,k) 
    | i <- [x-1,x,x+1]
    , j <- [y-1,y,y+1]
    , k <- [z-1,z,z+1]
    , (i,j,k) /= (x,y,z)]

initial :: Grid
initial = S.empty

grid :: [Coord] -> Grid
grid coords = L.foldl (\g coord -> set coord g) initial coords

zView :: Int -> Grid -> [String]
zView z g = chunksOf (n+1) $
    [ if S.member (x,y,z) g then '#' else '.' 
    | y <- [minY..minY+n]
    , x <- [minX..minX+n]]
        where
            (minX,minY,n) = zPlane coords 
            coords = [(i,j) | (i,j,k) <- toList g, k == z]

zPlane :: [(Int,Int)] -> (Int,Int,Int)
zPlane []     = (0,0,0) 
zPlane coords = (minX,minY,n)
    where
        minX = L.minimum $ L.map fst coords
        maxX = L.maximum $ L.map fst coords
        minY = L.minimum $ L.map snd coords
        maxY = L.maximum $ L.map snd coords
        n = max (maxX-minX) (maxY-minY)

set :: Coord -> Grid -> Grid
set = S.insert 

unset :: Coord -> Grid -> Grid
unset = S.delete



evolve :: Grid -> Grid
evolve = fromList . newGeneration

newGeneration :: Grid -> [Coord]
newGeneration g = L.filter 
    (\coord -> generate (S.member coord g) (activeNeighbors coord g)) 
    (allElements g)

allElements :: Grid -> [Coord]
allElements g | g == S.empty = []
allElements g = [(i,j,k) | i <- range, j <- range, k <- range]
    where 
        range = [min-1..max+1]
        min = minimum [minimum $ (L.map x) coords
                      ,minimum $ (L.map y) coords
                      ,minimum $ (L.map z) coords]
        max = maximum [maximum $ (L.map x) coords
                      ,maximum $ (L.map y) coords
                      ,maximum $ (L.map z) coords]
        x (x,_,_) = x
        y (_,y,_) = y
        z (_,_,z) = z
        coords = toList g

activeNeighbors :: Coord -> Grid -> Int
activeNeighbors coord grid = 
    length $ L.filter (flip S.member grid) (neighbors coord)

generate :: Bool -> Int -> Bool
generate True 2 = True
generate True 3 = True
generate False 3 = True
generate _ _ = False

count :: Int -> Grid -> Int
count n g = length $ toList $ last $ L.take (n+1) $ iterate evolve g

pView :: Int -> Grid -> IO ()
pView n g = putStrLn $ unlines $ zView n g

i :: [Coord]
i = [(1,-2,0),(2,-1,0),(0,0,0),(1,0,0),(2,0,0)]


