module LiteBot where
import Mancala

mm_player:: Game -> State -> Action
mm_player game state = bestMove state

bestMove:: State -> Action
bestMove (State currBoard avail) = getBestAction currBoard (snd avail)

getBestAction:: Board -> [Action] -> Action
getBestAction board avail = (Action (maximum'(getScores board avail)))

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y ->if x >= y then x else y)

getScores:: Board -> [Action] -> [(Action, Int)]
getScores board avail = zip avail (getResults board avail (length avail - 1))

getResults:: Board -> [Action] -> Int -> [Int]
getResults _ _ 0 = 0
getResults board avail len = (get_store (snd(fst (move_pieces (Action len) board)))):getResults board avail (len - 1)


{- 

		int bestCaseIndex = 0;
		int temp = 0;
		for(int i = 0; i < avail.length; i++) {
			temp = play(board[i]);
			if(temp > board[bestCaseIndex]) {
				bestCaseIndex = i;
			}
		}
		return (Action) bestCaseIndex;
-}
