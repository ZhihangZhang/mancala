module LiteBot where
import Mancala

mm_player:: Game -> Player
mm_player game state = bestMove state

bestMove:: State -> Player
bestMove (State currBoard avail) = getBestAction currBoard (snd avail)

getBestAction:: Board -> [Action] -> Action
getBestAction board avail = 


{- 
	ArrayList <Integer> extraTurns = new ArrayList<>();
	for(int i = 0; i < avail.length; i++) {
		if(board[i].getBeads() + i == mancala) {
			extraTurns.add(i);
		}
	}
	if (!extraTurns.empty()){
		return (Action) extraTurns.size() - 1;
	} else {
		int bestCaseIndex = 0;
		int temp = 0;
		for(int i = 0; i < avail.length; i++) {
			temp = play(board[i]);
			if(temp > board[bestCaseIndex]) {
				bestCaseIndex = i;
			}
		}
		return (Action) bestCaseIndex;
	}
-}