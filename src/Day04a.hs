module Day04a
    where
import Data.List
import Text.ParserCombinators.ReadP
import Data.Char

type Field = (Key,Value)
type Value = String
data Key = BYR
    deriving (Eq, Show)
    
parse = readP_to_S

field :: String -> Maybe Field
field s = case parse byrField s of
            [(f,_)] -> Just f
            [] -> Nothing

satisfying :: ReadP a -> (a -> Bool) -> ReadP a
satisfying parse predicate = do
    s <- parse
    if predicate s then return s else pfail


byrField :: ReadP Field
byrField = do
    string "byr:"
    -- s <- ((munch1 isDigit) `satisfying` ((==4).length))
    return (BYR,s)


collect :: [String] -> [String]
collect = filter (/= "")
        . map (concat . intersperse " ") 
        . groupBy (\g h -> (null g) == (null h))
