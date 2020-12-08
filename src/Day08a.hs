module Day08a
    where

import Data.Char

data Code = ACC Int
          | NOP Int
          | JMP Int
          deriving (Eq,Show,Read)
    
data BootCode = BootCode {
    accumulator :: Int,
    counter     :: Int,
    code        :: [Code],
    trace       :: [Int]
                         }
    
bootcode :: [Code] -> BootCode
bootcode code = sequence (BootCode 0 0 code []) 
    where
        sequence bc@(BootCode 0 0 [] []) = bc
        sequence bc@(BootCode _ c code _) | c < 0 || c >= length code = bc
        sequence bc@(BootCode _ c _ trace) | c `elem` trace = bc
        sequence bc@(BootCode a c code trace) =
            sequence (bc' { trace = (c:trace) })
                where bc' = case code!!c of
                              ACC n -> BootCode (a+n) (c+1) code trace
                              NOP _ -> BootCode a (c+1) code trace
                              JMP j -> BootCode a (c+j) code trace


readCode :: String -> Code
readCode = read . unwords . prepare . words . map toUpper 
    where
        prepare [a,('+':b)] = [a," "++b]
        prepare [a,('-':b)] = [a," (-"++b++")"]
