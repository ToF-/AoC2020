-- solving a system of congruences
-- x ≡ t0(mod i0)
-- x ≡ t1(mod i1)
--     …
-- x ≡ tn(in)
--
-- solve system t0(mod i0) , t1(mod i1) :
-- istart with Bezoux identities for i0 and i1 :
-- (r,u,v) = euclid i0 i1
-- solve x for this pair of congruences :
-- s0 = t1*u*i0+t0*v*i1
-- iterate with system s0(mod i0*i1) t2(mod i2)
-- start with Bezoux identities for i0*i1 and i2 :
-- (r,u,v) = euclid (i0*i1) i2
-- solve x for this pair of congruences :
-- s1 = t2*u*i0*i1+s0*v*i2
-- …
-- until last congruence t = sN-1 is found
-- reduce x ≡ t(mod k) with x = t - (t `div` k) * k
--
module Day13b
    where
import Data.List

interpret :: [String] -> [(Integer,Maybe Integer)]
interpret =  zip [0..]
             . map (\s -> if s == "x" then Nothing else Just (read s))
             . words 
             . map (\c -> if c == ',' then ' ' else c)
             . head . tail

congruences :: [(Integer, Maybe Integer)] -> [(Integer,Integer)]
congruences = map (\(i,Just k)->(-i,k)) . filter ((/=Nothing).snd)

euclid :: Integer -> Integer -> (Integer, Integer, Integer)
euclid a b = euclidExt a 1 0 b 0 1
    where
        euclidExt :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer,Integer,Integer)
        euclidExt r u v 0 u' v'= (r, u, v)
        euclidExt r u v r' u' v'= euclidExt r' u' v' (r- (r `div` r') * r') (u - (r `div` r') * u') (v - (r `div` r') * v')

reduce :: Integer -> Integer -> Integer
reduce t k = t - (t `div` k) * k 

solveCM :: Integer -> Integer -> Integer -> Integer -> Integer
solveCM a n b m = b * u * n + a * v * m
    where (_, u, v) = euclid n m

solveCMSystem :: [(Integer,Integer)] -> (Integer,Integer)
solveCMSystem = foldl1 solve1
    where
        solve1 :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
        solve1 (a,n) (b,m) = (c,k)
            where
                c = solveCM a n b m
                k = n*m
