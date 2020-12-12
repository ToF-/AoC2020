module Day12a
    where

type Direction = (Int,Int)
type Position = (Int,Int)
type Ship = (Position,Direction)

east,south,west,north :: Direction
east  =  (1,0)
south =  (0,1)
west  = (-1,0)
north = (0,-1)

move :: Ship -> Direction -> Int -> Ship
move ((x,y),d) (i,j) n = ((x+n*i,y+n*j),d) 

forward :: Ship -> Int -> Ship
forward s@((x,y),d) n = move s d n

rotate :: Ship -> Int -> Ship
rotate s@((x,y),d) a = ((x,y),(direction d a))
    where
        direction d a | a < 0 = direction d (360+a)
        direction d 0 = d
        direction (1,0) 90 = (0,1)
        direction (0,1) 90 = (-1,0)
        direction (-1,0) 90 = (0,-1)
        direction (0,-1) 90 = (1,0)
        direction d 180 = direction (direction d 90) 90
        direction d 270 = direction (direction d 90) 180
        direction d 360 = d
        direction d x = error $ "angle=" ++ show x

interpret :: Ship -> String -> Ship
interpret sh ('F':s) = forward sh (read s)
interpret sh ('R':s) = rotate sh (read s)
interpret sh ('L':s) = rotate sh (negate (read s))
interpret sh (c:s) = move sh (direction c) (read s)
    where
        direction 'E' = east
        direction 'S' = south
        direction 'N' = north
        direction 'W' = west
        direction c = error $ "direction=" ++ [c]

travel :: Ship -> [String] -> Ship
travel = foldl interpret 

distance :: Position -> Int
distance (x,y) = abs (0-x) + abs (0-y)
