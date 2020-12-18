module Day17b
    where

import Data.Set as S
import Data.List.Split
import Data.List as L

type Coord = (Int,Int,Int,Int)
type Grid = S.Set Coord

neighbors :: Coord -> [Coord]
neighbors (x,y,z,w) = 
    [(i,j,k,l) 
    | i <- [x-1,x,x+1]
    , j <- [y-1,y,y+1]
    , k <- [z-1,z,z+1]
    , l <- [w-1,w,w+1]
    , (i,j,k,l) /= (x,y,z,w)]

initial :: Grid
initial = S.empty

grid :: [Coord] -> Grid
grid = S.fromList

activeZone :: Grid -> (Int,Int)
activeZone g = (min-1,max+1)
    where
        min = minimum $ L.map (\f -> minimum $ L.map f coords) [x,y,z,w]
        max = maximum $ L.map (\f -> maximum $ L.map f coords) [x,y,z,w]
        coords = toList g


x,y,z,w :: Coord -> Int

x (a,b,c,d) = a
y (a,b,c,d) = b
z (a,b,c,d) = c
w (a,b,c,d) = d

zwView :: Int -> Int -> Grid -> [String]
zwView z w g = chunksOf (n+1) $ 
    [ if S.member (x,y,z,w) g then '#' else '.'
    | y <- [minY..minY+n]
    , x <- [minX..minX+n]]
        where
            (minX,minY,n) = zPlane coords 
            coords = [(i,j) | (i,j,k,l) <- toList g, k == z, l == w]

zPlane :: [(Int,Int)] -> (Int,Int,Int)
zPlane []     = (0,0,0) 
zPlane coords = (minX,minY,n)
    where
        minX = L.minimum $ L.map fst coords
        maxX = L.maximum $ L.map fst coords
        minY = L.minimum $ L.map snd coords
        maxY = L.maximum $ L.map snd coords
        n = max (maxX-minX) (maxY-minY)

evolve :: Grid -> Grid 
evolve = fromList . newGeneration

newGeneration :: Grid -> [Coord]
newGeneration g = L.filter 
    (\coord -> generate (S.member coord g) (activeNeighbors coord g)) 
    (allElements g)

allElements :: Grid -> [Coord]
allElements g | g == S.empty = []
allElements g = [(i,j,k,l) | i <- range, j <- range, k <- range, l <- range]
    where 
        range = [min..max]
        (min,max) = activeZone g

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
