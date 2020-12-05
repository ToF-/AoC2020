module Day04a
    where
import Data.List
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe

type Field = (Key,Value)
type Value = String
data Key = BYR | CID | ECL | EYR | HCL | HGT | IYR | PID
    deriving (Eq, Show, Ord)
    
validCount :: [String] -> Int
validCount = length . filter (id) . map valid . collect 

valid :: String -> Bool
valid = validKeys . sort . map fst . fields 
    where 
        validKeys [BYR, CID, ECL, EYR, HCL, HGT, IYR, PID] = True
        validKeys [BYR, ECL, EYR, HCL, HGT, IYR, PID] = True
        validKeys _ = False

parse = readP_to_S

fields :: String -> [Field]
fields = catMaybes . map field . words 

field :: String -> Maybe Field
field s = case parse oneField s of
            [(f,_)] -> Just f
            [] -> Nothing

oneField :: ReadP Field
oneField = choice [byrField
                  ,cidField
                  ,eclField
                  ,eyrField
                  ,hclField
                  ,hgtField
                  ,iyrField
                  ,pidField]

satisfying :: ReadP a -> (a -> Bool) -> ReadP a
satisfying parse predicate = do
    s <- parse
    if predicate s then return s else pfail


byrField :: ReadP Field
byrField = do
    string "byr:"
    s <- munch1 (const True)
    return (BYR,s)

cidField :: ReadP Field
cidField = do
    string "cid:"
    s <- munch1 (const True)
    return (CID,s)

eclField :: ReadP Field
eclField = do
    string "ecl:"
    s <- munch1 (const True)
    return (ECL,s)

eyrField :: ReadP Field
eyrField = do
    string "eyr:"
    s <- munch1 (const True)
    return (EYR,s)

iyrField :: ReadP Field
iyrField = do
    string "iyr:"
    s <- munch1 (const True)
    return (IYR,s)

hclField :: ReadP Field
hclField = do
    string "hcl:"
    s <- munch1 (const True)
    return (HCL,s)

hgtField :: ReadP Field
hgtField = do
    string "hgt:"
    s <- munch1 (const True)
    return (HGT,s)

pidField :: ReadP Field
pidField = do
    string "pid:"
    s <- munch1 (const True)
    return (PID,s)

collect :: [String] -> [String]
collect = filter (/= "")
        . map (concat . intersperse " ") 
        . groupBy (\g h -> (null g) == (null h))
