module Day13a
    where
import Data.List
import Data.Ord

departure :: Int -> [Int] -> (Int,Int)
departure start = head . sortBy (comparing snd) . map schedule
    where
        schedule bus = (bus,bus- (start `mod` bus))

interpret :: [String] -> (Int,[Int])
interpret [s,t] = (read s, map read (words (map strip t)))
    where
    strip ',' = ' '
    strip 'x' = ' '
    strip c = c

