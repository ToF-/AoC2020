module Day18a
    where
import Data.Char

eval :: String -> Int
eval = evaluate . prefixer

evaluate :: String -> Int
evaluate s = fst $ evaluate' (0,s)
    where 
        evaluate' :: (Int,String) -> (Int,String)
        evaluate' (r,"") = (r,"")
        evaluate' (_,c:cs) | isDigit c = (digitToInt c,cs)
        evaluate' (_,'+':cs) = (l + r, rest)
            where
                (l,rs)   = evaluate' (0,cs)
                (r,rest) = evaluate' (0,rs)
        evaluate' (_,'*':cs) = (l * r, rest)
            where
                (l,rs)   = evaluate' (0,cs)
                (r,rest) = evaluate' (0,rs)

prefixer :: String -> String
prefixer s = fst $ prefixer' ("",s) 
    where
        prefixer' :: (String,String) -> (String,String)
        prefixer' (s,"") = (s,"")
        prefixer' (s,' ':cs) = prefixer' (s,cs)
        prefixer' (s,c:cs) | isDigit c = prefixer' (s++[c],cs)
        prefixer' (s,'*':cs) = prefixer' ('*':s,cs)
        prefixer' (s,'+':cs) = prefixer' ('+':s,cs)
        prefixer' (s,'(':cs) = prefixer' (s++sub,rs)
            where
                (sub,rs) = prefixer' ("",cs)
        prefixer' (s,')':cs) = (s,cs)
