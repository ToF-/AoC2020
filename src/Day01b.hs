module Day01b
    where

threeTerms :: [Int] -> (Int,Int,Int)
threeTerms ns = head [(x,y,z)
                     | x <- ns, y <- ns, z <- ns,
                     x + y + z == 2020]
