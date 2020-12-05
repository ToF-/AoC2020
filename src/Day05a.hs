module Day05a
    where

type Range = (Int,Int)
type Narrow = Range -> Range

seatId :: String -> Int
seatId s = row * 8 + col
    where
    colPath = drop 7 s
    rowPath = take 7 s


    row = fst $ narrow (0,127) (narrowRow rowPath)
    col = fst $ narrow (0,7)   (narrowCol colPath)


narrowRow :: String -> [Narrow]
narrowRow = map narrowFromChar 
    where
    narrowFromChar 'F' = lower
    narrowFromChar 'B' = upper

narrowCol :: String -> [Narrow]
narrowCol = map narrowFromChar 
    where
    narrowFromChar 'L' = lower
    narrowFromChar 'R' = upper

lower :: Narrow
lower (min,max) = (min, (min+max)`div` 2)

upper :: Narrow
upper (min,max) = ((min+max)`div` 2+1, max)

narrow :: Range -> [Narrow] -> Range
narrow rg fs = foldl (\r f -> f r) rg fs
