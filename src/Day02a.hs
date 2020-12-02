module Day02a
    where
import Data.Char

howManyValid :: [String] -> Int
howManyValid = length . filter id . map validPassword

validPassword :: String -> Bool
validPassword = validate . readParameters

readParameters :: String -> (Int,Int,Char,String)
readParameters = readValues . words . map replaceNonAlphaNum
readValues [a,b,c,d] = (n,m,h,s)
    where
        n = read a :: Int
        m = read b :: Int
        h = head c
        s = d
replaceNonAlphaNum c | isDigit c || isAlpha c = c 
                     | otherwise = ' '

validate :: (Int,Int,Char,String) -> Bool
validate (n,m,c,s) = x >= n && x <= m
    where
    x = (length . filter (==c)) s
