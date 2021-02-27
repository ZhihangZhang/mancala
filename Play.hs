module Play where

-- To run it, try:
-- ghci
-- :load Play

import Mancala

import Greedy
--import Minimax_mem
import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)

type TournammentState = (Int,Int,Int)   -- wins, losses, ties
type PlayFn = Game -> Result -> Player -> TournammentState -> IO TournammentState


play :: Game -> State -> Player -> TournammentState -> IO TournammentState
play game start_state opponent ts =
  let (wins, losses,ties) = ts in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play game (EndOfTurn 0 start_state) opponent ts --Every ContinueGame has been replaced with EndOfGame
        else if line ==  "1"
             then computer_play game (EndOfTurn 0 start_state) opponent ts
        else if line == "2"
            then return ts
        else play game start_state opponent ts

person_play :: PlayFn
-- opponent has played, the person must now play
person_play game (EndOfGame val start_state) opponent ts =
  do
    newts <- update_tournament_state (-val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

person_play game (EndOfTurn val state) opponent ts =
   do
      let State _ (avail, _) = state
      putStrLn ("State:\n"++show state++"\nChoose one of "++show avail)
      line <- getLine
      let action = (readMaybe line :: Maybe Action)
      if (action == Nothing) || not ((fromJust action) `elem` avail)
        then do  -- error; redo
           putStrLn ("Invalid choice, try again...")
           person_play game (EndOfTurn val state) opponent ts
        else
           coordinate_next_play game (game (fromJust action) state) opponent ts person_play computer_play

person_play game (ContinueTurn state) opponent ts =
   do
      let State _ (avail, _) = state
      putStrLn ("You got an extra turn!")
      putStrLn ("State:\n"++show state++"\nChoose one of "++show avail)
      line <- getLine
      let action = (readMaybe line :: Maybe Action)
      if (action == Nothing) || not ((fromJust action) `elem` avail)
        then do -- error; redo
           putStrLn ("Invalid choice, try again...")
           person_play game (ContinueTurn state) opponent ts
        else
           coordinate_next_play game (game (fromJust action) state) opponent ts person_play computer_play


computer_play :: PlayFn
-- computer_play game current_result opponent ts
-- person has played, the computer must now play
computer_play game (EndOfGame val start_state) opponent ts =
   do
      newts <- update_tournament_state val ts
      play game start_state opponent newts

computer_play game (EndOfTurn _ state) opponent ts =
      let 
          opponent_move = opponent state
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            coordinate_next_play game (game opponent_move state) opponent ts computer_play person_play

computer_play game (ContinueTurn state) opponent ts =
      let 
          opponent_move = opponent state
        in
          do
            putStrLn ("The computer got an extra turn!")
            putStrLn ("The computer chose "++show opponent_move)
            coordinate_next_play game (game opponent_move state) opponent ts computer_play person_play

-- coordinate the next play by inspecting the result of last play
coordinate_next_play :: Game -> Result -> Player -> TournammentState -> PlayFn -> PlayFn -> IO TournammentState
-- game ends, pass to the other player
coordinate_next_play game (EndOfGame val start_state) opponent ts my_play other_play =
  do
    other_play game (EndOfGame val start_state) opponent ts

-- my turn ends, pass to the other player
coordinate_next_play game (EndOfTurn val state) opponent ts my_play other_play =
  do
    other_play game (EndOfTurn val state) opponent ts

-- I got an extra turn, pass to myself 
coordinate_next_play game (ContinueTurn state) opponent ts my_play other_play =
  do
    my_play game (ContinueTurn state) opponent ts

update_tournament_state:: Int -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses,ties)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses,ties)
  | val == 0 = do
      putStrLn "It's a tie"
      return (wins,losses,ties+1)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1,ties)

-- If you imported Mancala here and in Minimax try:
-- play mancala mancala_start simple_player (0,0,0)
-- play mancala mancala_start (greedy_player mancala) (0,0,0)
