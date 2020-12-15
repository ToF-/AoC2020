module Day15a
    where
import Data.Map as M
import Data.List as L

type Game = (Turn,Number,Memory)
type Turn = Int
type Number = Int
type Memory = M.Map Number [Turn]

start :: [Number] -> Game
start = L.foldl init (1,0,M.empty)
    where
        init (turn,_,memory) x = 
            ( turn+1
            , speak x memory
            , M.insertWith (\_ m -> turn:m) x [turn] memory)

play :: Game -> Game
play (turn,last,memory) = (turn+1, speak last memory', memory')
    where memory' =  M.insertWith (\_ m -> turn:m) last [turn] memory

speak :: Number -> Memory -> Number
speak x memory = case M.lookup x memory of
                   Nothing -> 0
                   Just [t] -> 0
                   Just (t:u:_) -> t-u


loop :: Turn -> [Number] -> Game
loop n st = last $ L.take (n- (length st)) $ iterate play (start st)

solve :: Turn -> [Number] -> Number
solve n st = result 
    where
        (_,result,_) = loop n st

