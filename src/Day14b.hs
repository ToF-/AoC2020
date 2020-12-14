module Day14b
    where
import Data.List as L
import Data.Char
import Data.Map as M

type Address = Int
type Value = Int
type Mask = String
type Cell = (Int,Value)
type Memory = Map Address Value

dispatch :: String -> [Address]
dispatch = sort . snd . L.foldl dispatchBit (1,[0]) . reverse
    where
        dispatchBit :: (Address,[Address]) -> Char -> (Address,[Address])
        dispatchBit (i,as) '0' = (i*2,as)
        dispatchBit (i,as) '1' = (i*2,L.map (+i) as)
        dispatchBit (i,as) 'X' = (i*2,as >>= (\ad -> [ad+i,ad]))

showAddress :: Address -> String
showAddress = format . reverse . L.map intToDigit . digits 
    where
        digits :: Address -> [Int] 
        digits 0 = []
        digits n = (n `mod` 2) : digits (n `div` 2)
        format :: String -> String
        format s = p ++ s
            where
                p = replicate l '0'
                l = 36 - (length s)

mask :: Address -> Mask -> String
mask addr mask = zipWith maskBit (showAddress addr) mask
    where
        maskBit c '0' = c
        maskBit c m = m

memory :: Memory
memory = empty

at :: Memory -> Address -> Value
at m addr = case M.lookup addr m of
              Nothing -> 0
              Just v -> v

write :: Memory -> Address -> Value -> Memory
write m addr value = M.insert addr value m

readAll :: Memory -> [Value]
readAll = L.map snd . M.toList

load :: Memory -> Mask -> [Cell] -> Memory
load m msk cs = L.foldl (loadMaskedAddress msk) m cs
    where
        loadMaskedAddress :: Mask -> Memory -> Cell -> Memory
        loadMaskedAddress msk m (addr,v) = L.foldl (\mem a-> write mem a v) m (dispatch (mask addr msk))

interpret :: [String] -> (Mask,[Cell])
interpret ss = (m, L.map readCell ls) 
    where
        m = l !! 1 
        (l:ls) = L.map (words . strip) ss
        strip = L.map (\c -> if isDigit c || isAlpha c then c else ' ')
        readCell [s,i,v] = (read i,read v) 

groupLoad :: [String] -> [[String]]
groupLoad = groupBy (\ s t -> (L.take 4 s == "mask") > (L.take 4 t =="mask")) 

solve :: [String] -> Int
solve ss = sum $ readAll $ L.foldl loadIt memory $ (L.map interpret . groupLoad) ss
    where
        loadIt :: Memory -> (Mask,[Cell]) -> Memory
        loadIt mem (mask,cells) = load mem mask cells
