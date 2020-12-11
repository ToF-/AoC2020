module Day11b
    where
import Data.List as L
import Data.Vector as V
import Data.Maybe

type Coord = (Int,Int)
neighbors (x,y) = [(i,j) | i<-[x-1,x,x+1], j<-[y-1,y,y+1], not (x==i && y==j)]

neighborLines (w,h) (x,y) = L.map neighborLine [(1,0),(-1,0),(0,-1),(0,1),(-1,-1),(1,-1),(-1,1),(1,1)]
    where
        neighborLine (i,j) = L.takeWhile (within (w,h)) $ L.zip [x+i,x+2*i..] [y+j,y+2*j..]
        within (m,n) (x,y) = x >= 0 && x < m && y >= 0 && y < n

at :: Vector (Vector a) -> (Int,Int) -> Maybe a
v `at` (x,y) = do
    r <- v !? y
    c <- r !? x
    return c

fromListOfList :: [[a]] -> Vector (Vector a)
fromListOfList ass = fromList (Prelude.map fromList ass)

toListOfList :: Vector (Vector a) -> [[a]]
toListOfList vs = Prelude.map toList (toList vs)

allCoords :: Vector (Vector a) -> Vector (Vector Coord)
allCoords vv = fromListOfList cc
    where
        cc = [rowCoords y | y <- [0..V.length vv -1]]
        rowCoords j = [(i,j) | i <- [0..V.length (vv!0)-1]]

mmap :: (a->b) -> Vector (Vector a) -> Vector (Vector b)
mmap f = V.map (V.map f)

allNeighbors :: Vector (Vector Char) -> Vector (Vector Int)
allNeighbors vv = mmap (countNeighbors vv) $ mmap (neighborLines (w,h)) (allCoords vv)
    where
        w = V.length (vv!0)
        h = V.length vv
        countNeighbors :: Vector (Vector Char) -> [[Coord]] -> Int
        countNeighbors area ls = Prelude.sum $ Prelude.map checkSeatInLine ls
            where 
            checkSeatInLine :: [Coord] -> Int
            checkSeatInLine [] = 0
            checkSeatInLine (coord:coords) = 
                case (area `at` coord) of
                  Nothing -> 0
                  Just '#' -> 1
                  Just 'L' -> 0
                  Just '.' -> checkSeatInLine coords

evolution :: Vector (Vector Char) -> Vector (Vector Char)
evolution area = V.zipWith (V.zipWith evol) area (allNeighbors area)
    where
        evol :: Char -> Int -> Char
        evol 'L' 0 = '#'
        evol '#' n | n >= 5 = 'L'
        evol c _ = c

evolve :: [String] -> [String]
evolve = toListOfList . evolution . fromListOfList 

occupied :: [String] -> Int
occupied = Prelude.sum . Prelude.map (Prelude.length . Prelude.filter (=='#'))

stabilize :: Eq a => (a->a) -> a -> a
stabilize f a = if f a == a then a else stabilize f (f a)
