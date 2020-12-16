module Day16a
    where

type Field = Int
type Interval = (Int,Int)
type PairOfIntervals = (Interval,Interval)

invalidForAnyField :: [PairOfIntervals] -> [Field] -> [Field]
invalidForAnyField is fs = filter (invalid is) fs

invalid :: [PairOfIntervals] -> Field -> Bool
invalid is f = all (not . (withinPair f)) is

withinPair :: Field -> PairOfIntervals -> Bool
withinPair f (i,j) = within f i || within f j

within :: Field -> Interval -> Bool
within f (a,b) = a <= f && f <= b
