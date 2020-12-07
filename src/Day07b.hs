module Day07b
    where
import Data.List

type Rule = (Color,[(Color, Int)])
type Color = String

simplify :: String -> [String]
simplify = filter (not.(`elem` syntax)) . words . filter (not .(`elem` ",."))
    where
    syntax = ["bag","bags","contain"]

extract :: [String] -> Rule
extract s = (container,bags remainder) 
    where
        container = bag1 ++ " " ++ bag2
        ([bag1,bag2],remainder) = splitAt 2 s

        bags (n:c1:c2:ws) = (c1++" "++c2, read n) : bags ws
        bags _ = []
        
rules :: [String] -> [Rule]
rules = map (extract . simplify)

containers :: Color -> [Rule] -> [Color]
containers color rs = nub $ sort $ findContainers [color]
    where
        canContain c = map fst $ filter (\(k,bs) -> Nothing /= (c `lookup` bs )) rs
        findContainers [] = []
        findContainers cs = cs ++ findContainers (cs >>= canContain)

countContainers :: Color -> [String] -> Int
countContainers color ss = length (containers color (map (extract . simplify) ss)) - 1

containedBags :: Color -> [String] -> Int
containedBags color ss = (contained (rules ss) color) - 1
    where
        contained :: [Rule] -> Color -> Int
        contained rs color = 
            case lookup color rs of
                Nothing -> 0
                Just [] -> 1
                Just cs -> 1 + sum (map (\(c,n) -> n*contained rs c) cs) 

