module Day18b
    where
import Data.Char
import Text.ParserCombinators.ReadP

parse :: ReadP a -> String -> Maybe a
parse parser s = case reverse (readP_to_S parser (strip s)) of
                   [] -> Nothing
                   ((r,_):_) -> Just r 

strip :: String -> String
strip "" = ""
strip (' ':cs) = strip cs
strip (c:cs) = c : strip cs

number :: ReadP Integer
number = do
    d <- satisfy isDigit 
    return $ fromIntegral (digitToInt d)

plus :: ReadP (Integer -> Integer -> Integer)
plus = string "+" >> return (+)

mult :: ReadP (Integer -> Integer -> Integer)
mult = string "*" >> return (*)

addition :: ReadP Integer
addition = chainl1 number plus

multiplication :: ReadP Integer
multiplication = chainl1 number mult

expression :: ReadP Integer
expression = chainl1 (chainl1 term plus) mult

term :: ReadP Integer
term = number +++ (parentheses expression)

parentheses :: ReadP Integer -> ReadP Integer 
parentheses parser = do
    string "("
    x <- parser
    string ")"
    return x
