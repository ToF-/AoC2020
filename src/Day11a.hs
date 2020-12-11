module Day11a
    where
import Data.List
import Data.Vector as V
import Data.Maybe

type Coord = (Int,Int)
neighbors (x,y) = [(i,j) | i<-[x-1,x,x+1], j<-[y-1,y,y+1], not (x==i && y==j)]

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
allNeighbors vv = mmap (countNeighbors vv) $ mmap neighbors (allCoords vv)
    where
        countNeighbors :: Vector (Vector Char) -> [Coord] -> Int
        countNeighbors area coords = Prelude.sum $ Prelude.map checkSeat coords
            where 
            checkSeat :: Coord -> Int
            checkSeat coord = case (area `at` coord) of
                                Just '#' -> 1
                                _ -> 0
                
evolution :: Vector (Vector Char) -> Vector (Vector Char)
evolution area = V.zipWith (V.zipWith evol) area (allNeighbors area)
    where
        evol :: Char -> Int -> Char
        evol 'L' 0 = '#'
        evol '#' n | n >= 4 = 'L'
        evol c _ = c

evolve :: [String] -> [String]
evolve = toListOfList . evolution . fromListOfList 

occupied :: [String] -> Int
occupied = Prelude.sum . Prelude.map (Prelude.length . Prelude.filter (=='#'))

stabilize :: Eq a => (a->a) -> a -> a
stabilize f a = if f a == a then a else stabilize f (f a)
