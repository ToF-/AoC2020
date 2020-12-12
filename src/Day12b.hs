module Day12b
    where

type Direction = (Int,Int)
type Position = (Int,Int)
type Waypoint = (Int,Int)
type Ship = (Position,Waypoint)

position :: Ship -> Position
position = fst

waypoint :: Ship -> Waypoint
waypoint = snd

east,south,west,north :: Direction
east  =  (1,0)
south =  (0,1)
west  = (-1,0)
north = (0,-1)

move :: Ship -> Direction -> Int -> Ship
move (pos,(x,y)) (i,j) n = (pos,(x+n*i,y+n*j))

forward :: Ship -> Int -> Ship
forward s@((x,y),(i,j)) n = ((x+n*i,y+n*j),(i,j))

rotate :: Ship -> Int -> Ship
rotate s@(p,wp) a = (p,(direction wp a))
    where
        direction wp a | a < 0 = direction wp (360+a)
        direction wp 0 = wp
        direction (x,y) 90 = (-y,x)
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
