module Mancala where
import Data.Tuple (swap)
import Data.List

-- To run it, try:
-- ghci
-- :load Mancala
--

-- https://endlessgames.com/wp-content/uploads/Mancala_Instructions.pdf
type Side = [Int] -- last element being the store/mancala
type Board = (Side, Side)
type InternalState = Board 
data Action = Action Int -- index of a selected pocket (non-empty)
         deriving (Ord,Eq)
data State = State InternalState ([Action], [Action])
         deriving (Ord, Eq)

data Result = EndOfGame Int State    -- end of game: value, starting state
            | EndOfTurn Int State        -- the other player continues 
            | ContinueTurn State        -- current player takes another turn 
         deriving (Eq, Show)
type Game = Action -> State -> Result
type Player = State -> Action

-- Start states
side_start = [4, 4, 4, 4, 4, 4, 0]
board_start = (side_start, side_start)
actions_start = avail_actions board_start 
mancala_start = State board_start actions_start

--mancala :: Game
mancala move (State (my_side, other_side) (my_actions, other_actions))
  | end = EndOfGame (win b3) mancala_start
  | extra_turn = ContinueTurn (State b3 avail) -- extra turn, no need to swap sides and available actions
  | otherwise = EndOfTurn val (State (swap b3) (swap avail)) -- opponent's turn, swap sides and available actions 
  where 
    (b1, end_index) = move_pieces move (my_side, other_side) 
    b2 = capture_pieces end_index (my_side, other_side) b1 
    (b3, end) = collect_pieces b2
    avail = avail_actions b3 
    extra_turn = end_index == (length $ get_pockets my_side)
    val = val_board b3 
-- Tests
-- 1. end of turn
-- *Mancala> mancala_start
-- State ([4,4,4,4,4,4,0],[4,4,4,4,4,4,0]) ([0,1,2,3,4,5],[0,1,2,3,4,5])
-- *Mancala> r1 = mancala (Action 1) mancala_start
-- *Mancala> r1
-- EndOfTurn (State ([4,4,4,4,4,4,0],[4,0,5,5,5,5,0]) ([0,1,2,3,4,5],[0,2,3,4,5]))
--
-- 2. extra of turn because the last piece is dropped in the store
-- *Mancala> mancala_start
-- State ([4,4,4,4,4,4,0],[4,4,4,4,4,4,0]) ([0,1,2,3,4,5],[0,1,2,3,4,5])
-- *Mancala> r1 = mancala (Action 2) mancala_start
-- *Mancala> r1
-- ContinueTurn (State ([4,4,0,5,5,5,1],[4,4,4,4,4,4,0]) ([0,1,3,4,5],[0,1,2,3,4,5]))
--
-- 3. capture opponent's pieces because the last piece is dropeed in an empty
-- pocket
-- *Mancala> s1 = (State ([4,4,0,5,0,5,1],[4,5,4,4,4,4,0]) (actionize [0,1,3,5],actionize [0,1,2,3,4,5]))
-- *Mancala> mancala (Action 0) s1
-- EndOfTurn (State ([4,0,4,4,4,4,0],[0,5,1,6,0,5,7]) ([0,2,3,4,5],[1,2,3,5]))
--
-- 4. capture even if the opposite pocket is empty
-- *Mancala> s1 = (State ([4,4,0,5,0,5,1],[4,0,4,4,4,4,0]) (actionize [0,1,3,5],actionize [0,1,2,3,4,5]))
-- *Mancala> mancala (Action 0) s1
-- EndOfTurn (State ([4,0,4,4,4,4,0],[0,5,1,6,0,5,2]) ([0,2,3,4,5],[1,2,3,5]))


-- get the number of pieces in a pocket, 0-index starting from the left most
-- pocket on my side and excluding the opponent's mancala 
-- precondition: 0 <= index <= 2 * number of pockets
get_val :: Board -> Int -> Int
get_val board index = (to_list board) !! index
{-
*Mancala> myBoard = ([1..7], [8..14])
*Mancala> myBoard
([1,2,3,4,5,6,7],[8,9,10,11,12,13,14])
*Mancala> get_val myBoard 0
1
*Mancala> get_val myBoard 13
14
*Mancala> get_val myBoard 14
*** Exception: Prelude.!!: index too large
*Mancala> get_val myBoard 8
9
-}

-- set the number of pieces in a pocket, indexed similarly as get_val
-- precondition: 0 <= index <= 2 * number of pockets
set_val :: Board -> Int -> Int -> Board
set_val (my_side, other_side) index value = to_board new_l other_store len
  where
    len = length $ get_pockets my_side
    other_store = get_store other_side
    l = to_list (my_side, other_side)
    new_l = (take index l) ++ (value : drop (index+1) l)
{-
*Mancala> myBoard = ([1..7], [8..14])
*Mancala> myBoard
([1,2,3,4,5,6,7],[8,9,10,11,12,13,14])
*Mancala> set_val myBoard 0 100
([100,2,3,4,5,6,7],[8,9,10,11,12,13,14])
*Mancala> set_val myBoard 12 100
([1,2,3,4,5,6,7],[8,9,10,11,12,100,14])
*Mancala> set_val myBoard 2 100
([1,2,100,4,5,6,7],[8,9,10,11,12,13,14])
-}

-- get pockets of a side
get_pockets :: Side -> [Int]
get_pockets s = init s
{-
*Mancala> mySide = [1..7]
*Mancala> mySide
[1,2,3,4,5,6,7]
*Mancala> get_pockets mySide
[1,2,3,4,5,6]
-}

-- get the store/mancala of a side
get_store :: Side -> Int
get_store s = last s
{-
*Mancala> mySide = [1..7]
*Mancala> mySide
[1,2,3,4,5,6,7]
*Mancala> get_store mySide
7
-}

-- convert the board to a list representation from my perspective (excluding
-- the opponent's store/mancala)
to_list :: Board -> [Int]
to_list (my_side, other_side) = my_side ++ (get_pockets other_side) 
{-
*Mancala> to_list myBoard
[1,2,3,4,5,6,7,8,9,10,11,12,13]
*Mancala> myBoard = ([1..7],[8..14])
*Mancala> myBoard
([1,2,3,4,5,6,7],[8,9,10,11,12,13,14])
*Mancala> to_list myBoard
[1,2,3,4,5,6,7,8,9,10,11,12,13]
-}

-- convert a list representation to board
-- l: list presentation of the board from my perspective (result of to_list)
-- other_store: opponent's store/mancala
-- len: the number of pockets on my side
to_board :: [Int] -> Int -> Int -> Board 
to_board l other_store len = 
  (my_side, other_side)
    where
      my_side = take (len+1) l
      other_side = drop (len+1) l ++ [other_store]
{-
*Mancala> to_board [1..7] 2 1
([1,2],[3,4,5,6,7,2])
*Mancala> to_board [1..7] 2 2
([1,2,3],[4,5,6,7,2])
*Mancala> to_board [1..7] 2 3
([1,2,3,4],[5,6,7,2])
*Mancala> to_board [1..7] 2 4
([1,2,3,4,5],[6,7,2])
*Mancala> to_board [1..7] 2 5
([1,2,3,4,5,6],[7,2])
*Mancala> to_board [1..7] 2 6
([1,2,3,4,5,6,7],[2])
*Mancala> to_board [1..7] 2 7
([1,2,3,4,5,6,7],[2])
-}

-- Map a list in a circular manner starting from 
-- a specific position for a certain number of times
circular_map :: [a] -> Int -> Int -> (a -> a) -> [a]
circular_map [] _ _ _ = []
circular_map l _ 0 _ = l
circular_map l start n f
  | start < 0 = l
  | n < 0 = l
  | start >= len = l
  | wrap_n <= 0 = zipWith ($) fnList l
  | otherwise = circular_map (zipWith ($) fnList l) 0 wrap_n f
 where
      len = length l
      fnList = [if (i < start) || (i >= start + n) then id else f | i <- [0..len-1]]
      wrap_n = start + n - len
-- Tests
-- *Mancala> circular_map [1, 2, 3, 4] 1 3 (+1)
-- [1,3,4,5]
-- *Mancala> circular_map [1, 2, 3, 4] 1 4 (+1)
-- [2,3,4,5]
-- *Mancala> circular_map [1, 2, 3, 4] 1 5 (+1)
-- [2,4,4,5]
-- *Mancala> circular_map [1, 2, 3, 4] 0 1 (+1)
-- [2,2,3,4]
-- *Mancala> circular_map [1, 2, 3, 4] 0 8 (+1)
-- [3,4,5,6]
-- *Mancala> circular_map [1, 2, 3, 4] 0 0 (+1)
-- [1,2,3,4]
-- *Mancala> circular_map [] 0 0 (+1)
-- []
-- *Mancala> circular_map [] 0 1 (+1)
-- []



-- Given an action, move pieces around the board and return the end index and
-- the updated board
move_pieces :: Action -> Board -> (Board, Int) 
move_pieces (Action index) (my_side, other_side) =
  (new_board, end_index)
    where
      other_store = get_store other_side
      len = length $ get_pockets my_side
      val = get_val (my_side, other_side) index
      zeroed_board = set_val (my_side, other_side) index 0 
      l = circular_map (to_list zeroed_board) (index+1) val (+1)
      new_board = to_board l other_store len
      end_index = mod (index + val) (length l)
-- Tests
-- *Mancala> myBoard = ([1..7], [8..14])
-- *Mancala> myBoard
-- ([1,2,3,4,5,6,7],[8,9,10,11,12,13,14])
-- *Mancala> move_pieces (Action 0) myBoard
-- (([0,3,3,4,5,6,7],[8,9,10,11,12,13,14]),1)
-- *Mancala> move_pieces (Action 2) myBoard
-- (([1,2,0,5,6,7,7],[8,9,10,11,12,13,14]),5)
-- *Mancala> move_pieces (Action 4) myBoard
-- (([1,2,3,4,0,7,8],[9,10,11,11,12,13,14]),9)
-- *Mancala> move_pieces (Action 5) myBoard
-- (([1,2,3,4,5,0,8],[9,10,11,12,13,13,14]),11)


-- Given the end index of a move, capture pieces on a board, if any
-- assuming two sides have the same number of pockets
capture_pieces :: Int -> Board -> Board -> Board
capture_pieces end_index (my_old_side, other_old_side) (my_side, other_side)
  | end_index < len && old_val == 0 = b3
  | otherwise = (my_side, other_side) 
  where
    len = length $ get_pockets my_side 
    old_val = get_val (my_old_side, other_old_side) end_index
    old_store = get_store my_old_side
    capture_index = 2 * len - end_index 
    capture_val = get_val (my_side, other_side) capture_index
    b1 = set_val (my_side, other_side) capture_index 0
    b2 = set_val b1 end_index 0
    b3 = set_val b2 len (capture_val+1+old_store)
-- Tests
-- 1. valid capture
-- *Mancala> b1 = fst $ move_pieces (Action 4) (side_start, side_start)
-- *Mancala> b1
-- ([4,4,4,4,0,5,1],[5,5,4,4,4,4,0])
-- *Mancala> b2 = fst $ move_pieces (Action 0) b1
-- *Mancala> b2
-- ([0,5,5,5,1,5,1],[5,5,4,4,4,4,0])
-- *Mancala> snd $ move_pieces (Action 0) b1
-- 4
-- *Mancala> capture_pieces 4 b1 b2
-- ([0,5,5,5,0,5,7],[5,0,4,4,4,4,0])
--
-- 2. invalid capture (expecting no-op)
-- *Mancala> b1 = fst $ move_pieces (Action 2) (side_start, side_start)
-- *Mancala> b1
-- ([4,4,0,5,5,5,1],[4,4,4,4,4,4,0])
-- *Mancala> snd $ move_pieces (Action 2) (side_start, side_start)
-- 6
-- *Mancala> capture_pieces 6 (side_start, side_start) b1
-- ([4,4,0,5,5,5,1],[4,4,4,4,4,4,0])
--
-- *Mancala> b1 = fst $ move_pieces (Action 4) (side_start, side_start)
-- *Mancala> b1
-- ([4,4,4,4,0,5,1],[5,5,4,4,4,4,0])
-- *Mancala> snd $ move_pieces (Action 4) (side_start, side_start)
-- 8
-- *Mancala> capture_pieces 8 (side_start, side_start) b1
-- ([4,4,4,4,0,5,1],[5,5,4,4,4,4,0])


-- Determine if a game ends or not and collect pieces on the board, if any
-- return the updated board and a bool indicating whether any pieces are
-- collected because the game ends
collect_pieces :: Board -> (Board, Bool)
collect_pieces board
  | one_side_clear board = (collect_pieces_helper board, True)
  | otherwise = (board, False)
-- Tests
-- *Mancala> b1 = (side_start, side_start)
-- *Mancala> b1
-- ([4,4,4,4,4,4,0],[4,4,4,4,4,4,0])
-- *Mancala> collect_pieces b1
-- (([4,4,4,4,4,4,0],[4,4,4,4,4,4,0]),False)
-- *Mancala> b1 = (replicate 5 0 ++ [4], side_start)
-- *Mancala> b1
-- ([0,0,0,0,0,4],[4,4,4,4,4,4,0])
-- *Mancala> collect_pieces b1
-- (([0,0,0,0,0,4],[0,0,0,0,0,24]),True)
 
-- Helper to collect pieces
collect_pieces_helper :: Board -> Board 
collect_pieces_helper (my_side, other_side) = updated_board
    where
      len = length $ get_pockets my_side
      zeros = replicate len 0
      my_side_sum = sum my_side
      other_side_sum = sum other_side 
      updated_board = (zeros ++ [my_side_sum], zeros ++ [other_side_sum])
-- Tests
-- *Mancala> b1 = (side_start, side_start)
-- *Mancala> b1
-- ([4,4,4,4,4,4,0],[4,4,4,4,4,4,0])
-- *Mancala> collect_pieces_helper b1
-- ([0,0,0,0,0,0,24],[0,0,0,0,0,0,24])
-- *Mancala> b2 = (replicate 7 0, replicate 7 0)
-- *Mancala> b2
-- ([0,0,0,0,0,0,0],[0,0,0,0,0,0,0])
-- *Mancala> collect_pieces_helper b2
-- ([0,0,0,0,0,0,0],[0,0,0,0,0,0,0])


-- Determine if one side is already cleared
one_side_clear :: Board -> Bool
one_side_clear (my_side, other_side) = 
  all (== 0) (get_pockets my_side) || all (== 0) (get_pockets other_side)

-- Determine who wins given a board that has been fully updated (i.e., after
-- pieces are moved, captured, and collected) 
-- return an int, it is positive if I win, or negative if the opponent wins
win :: Board -> Int
win (my_side, other_side) = get_store my_side - get_store other_side

val_board b = win b

-- Given a board return the available actions
avail_actions :: Board -> ([Action], [Action])
avail_actions (my_side, other_side) =
  (filter_avail my_pockets, filter_avail other_pockets)
  where 
    my_pockets = get_pockets my_side
    other_pockets = get_pockets other_side
    filter_avail pockets = [Action i | (p, i) <- zip pockets [0..], p > 0]
-- Tests
-- Mancala> b1
-- ([4,4,4,4,4,4,0],[4,4,4,4,4,4,0])
-- *Mancala> avail_actions b1
-- ([0,1,2,3,4,5],[0,1,2,3,4,5])
-- *Mancala> b1 = (replicate 5 0 ++ [4], side_start)
-- *Mancala> b1
-- ([0,0,0,0,0,4],[4,4,4,4,4,4,0])
-- *Mancala> avail_actions b1
-- ([],[0,1,2,3,4,5])


instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]
instance Show State where
    show (State (my_side, other_side) _) = first_line ++ "\n" ++ second_line
      where 
        other_pockets = intercalate " " $ map show $ reverse $ get_pockets other_side 
        other_store = "*" ++ show (get_store other_side) ++ "* "
        first_line = other_store ++ other_pockets  
        my_pockets = intercalate " " $ map show $ get_pockets my_side 
        my_store = " *" ++ show (get_store my_side) ++ "*"
        second_line = my_pockets ++ my_store

        
        

-- Helpers for testing in the repl
actionize :: [Int] -> [Action]
actionize = map Action

------- A Player -------

-- a simple player that always chooses the first in the availble actions 
simple_player :: Player
simple_player (State _ (avail, _)) = head avail

