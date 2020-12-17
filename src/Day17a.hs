module Day17a
    where

import Data.Set as S
import Data.List.Split

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

zView :: Coord -> Int -> Grid -> [String]
zView (x,y,z) n g = chunksOf n $
    [ if S.member c g then '#' else '.'
    | c <- zPlane (x,y,z) (n`div`2)]

set :: Coord -> Grid -> Grid
set = S.insert 


zPlane :: Coord -> Int -> [Coord]
zPlane (x,y,z) r = [ (i,j,z) | j <- [y-r..y+r] , i <- [x-r..x+r]] 

