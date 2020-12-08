module Day08b
    where

import Data.Char

data Code = ACC Int
          | NOP Int
          | JMP Int
          deriving (Eq,Show,Read)

data Result = Running
            | Halt Int Int
            | Exit 
            deriving (Eq,Show)
    
data BootCode = BootCode {
    accumulator :: Int,
    counter     :: Int,
    code        :: [Code],
    trace       :: [Int],
    status      :: Result
                         }
    
bootcode :: [Code] -> BootCode
bootcode code = sequence (BootCode 0 0 code [] Running) 
    where
        sequence bc | status bc /= Running = bc
        sequence bc@(BootCode 0 0 [] [] _) = bc
        sequence bc@(BootCode _ c code _ _) | c < 0 || c > length code = error "code out of reach"
        sequence bc@(BootCode a c code _ _) | c == length code = bc { status = Exit }
        sequence bc@(BootCode a c _ trace _) | c `elem` trace = bc
            { status = Halt a c }
        sequence bc@(BootCode a c code trace status) =
            sequence (bc' { trace = (c:trace) })
                where bc' = case code!!c of
                              ACC n -> BootCode (a+n) (c+1) code trace status
                              NOP _ -> BootCode a (c+1) code trace status
                              JMP j -> BootCode a (c+j) code trace status


readCode :: String -> Code
readCode = read . unwords . prepare . words . map toUpper 
    where
        prepare [a,('+':b)] = [a," "++b]
        prepare [a,('-':b)] = [a," (-"++b++")"]

fixPoints :: [Code] -> [Int]
fixPoints = map fst . filter isAPoint . zip [0..]
    where
        isAPoint (_,(ACC _)) = False
        isAPoint _           = True

switchFixPoint :: [Code] -> Int -> [Code]
switchFixPoint pgm c = before ++ (instr' : after )
    where
        (before,(instr:after)) = splitAt c pgm
        instr' = switch instr
        switch (NOP n) = (JMP n)
        switch (JMP n) = (NOP n)
        switch _ = error "incorrect switch point"

fixCode :: [Code] -> BootCode
fixCode pgm = bootcode $ head $ filter isFixed $ map (switchFixPoint pgm) (fixPoints pgm)
    where
        isFixed pgm = status (bootcode pgm) == Exit
