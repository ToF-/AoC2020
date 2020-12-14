module Day14a
    where
import Data.Bits
import Data.Char
import Data.List

type Value = Integer
type Mask = String
type Cell = (Int,Value)
type Memory = [Cell]

andMask :: String -> Value
andMask = foldl andBit 1
    where
        andBit a '0' = 2 * a
        andBit a  _  = 2 * a + 1

orMask :: String -> Value
orMask = foldl orBit 0
    where
        orBit a '1' = 2 * a + 1
        orBit a  _  = 2 * a

applyMask :: String -> Value -> Value
applyMask m v = (v .&. am) .|. om
    where
        am = andMask m
        om = orMask m

memory :: Memory
memory = []

at :: Memory -> Int -> Value
at m i = case lookup i m of
           Nothing -> 0
           Just v -> v

write :: Memory -> Int -> Value -> Memory
write m i v = (i,v) : m

readAll :: Memory -> [Value]
readAll m = [m `at` i| i <- [0..n]] 
    where
        n = maximum (map fst m)

load :: Memory -> Mask -> [Cell] -> Memory
load m mask cs = foldl (loadMaskedCell mask) m cs
    where
        loadMaskedCell :: Mask -> Memory -> Cell -> Memory
        loadMaskedCell mask m (i,v) = write m i (applyMask mask v)

interpret :: [String] -> (Mask,[Cell])
interpret ss = (m, map readCell ls) 
    where
        m = l !! 1 
        (l:ls) = map (words . strip) ss
        strip = map (\c -> if isDigit c || isAlpha c then c else ' ')
        readCell [s,i,v] = (read i,read v) 

groupLoad :: [String] -> [[String]]
groupLoad = groupBy (\ s t -> (take 4 s == "mask") > (take 4 t =="mask")) 

solve :: [String] -> Integer
solve ss = sum $ readAll $ foldl loadIt memory $ (map interpret . groupLoad) ss
    where
        loadIt :: Memory -> (Mask,[Cell]) -> Memory
        loadIt mem (mask,cells) = load mem mask cells

