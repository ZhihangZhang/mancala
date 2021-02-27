module Greedy where

-- To run it, try:
-- ghci
-- :load greedy
import Mancala

greedy:: Game -> State -> (Action, Double)
-- greedy game state   =>  (move,value_to_player)
-- precondition: there are some moves that are available
greedy game st  =
      argmax (valueact game st) $ fst avail
      where State _ avail = st

-- valueact game st action  is the value of doing action act in state st for game
valueact :: Game -> State -> Action -> Double
valueact game st act = value game (game act st)

value:: Game -> Result -> Double
value _  (EndOfGame val _) = fromIntegral val
value game (EndOfTurn val st) = fromIntegral val   
value game (ContinueTurn st) =  snd (greedy game st)   

-- to find the best opening move
-- greedy mancala mancala_start

-- argmax f lst  = (e, f e) for e <- lsts where f e is maximal
--  Note that this does not require the elements of lst to be comparable
-- like  max[(e,f e) <- e in lst] but where only the second elements of pairs are compared in the max.
argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t) 
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h

--- after surfing the web, I found this is the standard argmaxWithMax
--  http://hackage.haskell.org/package/list-extras-0.4.1.4/docs/Data-List-Extras-Argmax.html

-- Test case:
-- argmax (\x -> 5- (x-2)^2) [0..10]
-- argmax (\x -> 1 + 4*x - x^2) [0..10]


-- For Mancala
-- to find the best opening move
-- res = greedy mancala mancala_start
-- (fst res)
-- stats (snd res)   -- gets the size and depth of the memory

--Try
-- greedy mancala mancala_start
-- b1 = ([1,6,5,1,0,6,2],[0,2,1,8,7,7,2])
-- avail = avail_actions b1 
-- greedy mancala (State b1 avail)

greedy_player:: Game -> Player
greedy_player game state = fst $ greedy game state
