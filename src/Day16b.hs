module Day16b
    where
import Data.List

type Ticket = [Field]
type Field = Int
type Interval = (Int,Int)
type PairOfIntervals = (Interval,Interval)

data FieldId = DL | DS | DP | DK | DD | DM | AL | AS | AP | AK | CL | DU | PR | RT | RW | ST | TR | TY | WG | ZN
    deriving (Eq,Show,Enum,Bounded,Ord)

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

invalidForFields :: [(FieldId,PairOfIntervals)] -> Field -> [FieldId]
invalidForFields fis f = map fst $ filter (not . (withinPair f) . snd) fis 

impossibleFields :: [(FieldId,PairOfIntervals)] -> [[Field]] -> [[FieldId]]
impossibleFields fis fs = map (nub . sort . concat) . transpose $ map (map (invalidForFields fis)) fs

possibleFields :: [(FieldId,PairOfIntervals)] -> [[Field]] -> [[FieldId]]
possibleFields fis fs = map (fieldIds \\) (impossibleFields fis fs)
    where
        fieldIds = map fst fis

singletons :: [[FieldId]] -> [FieldId]
singletons = concat . filter ((==1) . length)

removeField :: FieldId -> [FieldId] -> [FieldId]
removeField f [a] = [a]
removeField f fs  = fs \\ [f]

narrow :: [[FieldId]] -> [[FieldId]]
narrow p = foldl (\p f -> map (removeField f) p) p (singletons p)


narrowFix :: [[FieldId]] -> [[FieldId]]
narrowFix p = case p' == p of
                True -> p'
                False -> narrowFix p'
            where p' = narrow p
