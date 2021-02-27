module LiteBot where
import Mancala

mm_player:: Game -> State -> Action
mm_player game state = bestMove state

bestMove:: State -> Action
bestMove (State currBoard avail) = getBestAction currBoard (snd avail)

getBestAction:: Board -> [Action] -> Action
getBestAction board avail
  | noExtraMoves scores = fst(maximum'(getScores board avail))
  | otherwise = getExtraMove (getCoords board avail)
  where 
   scores = getCoords board avail
  

noExtraMoves :: [(Action,Int)] -> Bool
noExtraMoves [] = True
noExtraMoves lst = and (noMancalas lst)

noMancalas :: [(Action,Int)] -> [Bool]
noMancalas lst = map (/=6) (sumPos lst)

sumPos :: [(Action,Int)] -> [Int] 
sumPos lst = map sumTuple lst

sumTuple :: (Action, Int) -> Int
sumTuple tup = (intAction (fst tup)) + (snd tup)

intAction :: Action -> Int
intAction (Action x) = x

getExtraMove :: [(Action, Int)] -> Action
getExtraMove (x:xs)
  | intAction(fst x) + (snd x) == 6 = (fst x)
  | otherwise = getExtraMove xs

maximum' :: Ord a => [(t, a)] -> (t, a)
maximum' []     = error "maximum of empty list"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

getScores :: Board -> [Action] -> [(Action, Int)]
getScores board avail = zip (map Action [0..5]) (init(reverse(getResults board avail 6)))

getCoords :: Board -> [Action] -> [(Action, Int)]
getCoords board avail = zip (map Action [0..5]) (get_pockets (fst board))

getResults:: Board -> [Action] -> Int -> [Int]
getResults _ _ (-1) = []
getResults board avail len = (get_store (fst(fst (move_pieces (Action len) board)))):getResults board avail (len - 1)
{-
as = map Action
myState = (State ([4,4,4,4,4,4,0],[4,4,4,4,4,4,0]) (as [0,1,2,3,4,5],as [0,1,2,3,4,5]))
myState = (State ([4,4,0,5,5,5,1],[4,4,4,4,4,4,0]) (as [0,1,2,3,4,5],as [0,1,2,3,4,5]))
myBoard = ([4,4,0,5,5,5,1], [4,4,4,4,4,4,0])
getResults myBoard (as[0,1,2,3,4,5]) 6
getScores myBoard (as[0..5])
-}
