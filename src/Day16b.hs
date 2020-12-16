module Day16b
    where

type Ticket = [Field]
type Field = Int
type Interval = (Int,Int)
type PairOfIntervals = (Interval,Interval)

validTickets :: [PairOfIntervals] -> [Ticket] -> [Ticket]
validTickets is = filter (null . (invalidForAnyField is))

invalidForAnyField :: [PairOfIntervals] -> Ticket -> Ticket
invalidForAnyField is fs = filter (invalid is) fs

invalid :: [PairOfIntervals] -> Field -> Bool
invalid is f = all (not . (withinPair f)) is

withinPair :: Field -> PairOfIntervals -> Bool
withinPair f (i,j) = within f i || within f j

within :: Field -> Interval -> Bool
within f (a,b) = a <= f && f <= b
