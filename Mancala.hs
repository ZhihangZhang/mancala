module Mancala where

-- To run it, try:
-- ghci
-- :load Mancala
--

-- https://endlessgames.com/wp-content/uploads/Mancala_Instructions.pdf
type Side = [Int] -- last element being the store/mancala
type Board = (Side, Side)  -- TODO: not sure if using just a list is any better
type InternalState = Board 
data Action = Action Int -- index of a selected pocket (non-empty)
         deriving (Ord,Eq)
data State = State InternalState ([Action], [Action])  
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | EndOfTurn State        -- the other player continues 
            | ContinueTurn State        -- current player takes another turn
         deriving (Eq, Show)
type Game = Action -> State -> Result
type Player = State -> Action

side_start = [4, 4, 4, 4, 4, 4, 0]
actions_start = [Action i | i <- [0..5]] -- only not-empty pockets are valid actions 
mancala_start = State (side_start, side_start) (actions_start, actions_start) 


-- TODO: write out preconditions for functions
--mancala :: Game
--mancala move (State ((my_pockets, my_store), (other_pockets, other_store)) (my_actions, other_actions))
-- - move pieces

get_val :: Board -> Int -> Int
get_val board index = (to_list board) !! index
-- TODO: tests

set_val :: Board -> Int -> Int -> Board
set_val (my_side, other_side) index value = to_board new_l other_store len
  where
    len = length $ get_pockets my_side
    other_store = get_store other_side
    l = to_list (my_side, other_side)
    new_l = (take index l) ++ (value : drop (index+1) l)
---- TODO: tests

get_pockets :: Side -> [Int]
get_pockets s = init s

get_store :: Side -> Int
get_store s = last s

to_list :: Board -> [Int]
to_list (my_side, other_side) = my_side ++ (get_pockets other_side) 
-- TODO: tests

to_board :: [Int] -> Int -> Int -> Board 
to_board l other_store len = 
  (my_side, other_side)
    where
      my_side = take (len+1) l
      other_side = drop (len+1) l ++ [other_store]
-- TODO: tests

-- Map a list in a circular manner starting from 
-- a specific position for a certain number of times
-- This will be used to update board state
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



move_pieces :: Action -> Board -> Board
move_pieces (Action index) (my_side, other_side) =
  to_board l other_store len
    where
      other_store = get_store other_side
      len = length $ get_pockets my_side
      val = get_val (my_side, other_side) index
      zeroed_board = set_val (my_side, other_side) index 0 
      l = circular_map (to_list zeroed_board) (index+1) val (+1)
-- TODO: tests


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
-- TODO: tests

-- Some preliminary tests
-- 1. testing move and valid capture
-- *Mancala> b1 = move_pieces (Action 4) (side_start, side_start)
-- *Mancala> b1
-- ([4,4,4,4,0,5,1],[5,5,4,4,4,4,0])
-- *Mancala>
-- *Mancala> b1
-- ([4,4,4,4,0,5,1],[5,5,4,4,4,4,0])
-- *Mancala> b2 = move_pieces (Action 0) b1
-- *Mancala> b2
-- ([0,5,5,5,1,5,1],[5,5,4,4,4,4,0])
-- *Mancala> capture_pieces 4 b1 b2
-- ([0,5,5,5,0,5,7],[5,0,4,4,4,4,0])
-- *Mancala>
-- 2. testing invalid capture
-- *Mancala> b1 = move_pieces (Action 2) (side_start, side_start)
-- *Mancala> b1
-- ([4,4,0,5,5,5,1],[4,4,4,4,4,4,0])
-- *Mancala> capture_pieces 6 (side_start, side_start) b1
-- ([4,4,0,5,5,5,1],[4,4,4,4,4,4,0])
-- *Mancala> b1 = move_pieces (Action 4) (side_start, side_start)
-- *Mancala> b1
-- ([4,4,4,4,0,5,1],[5,5,4,4,4,4,0])
-- *Mancala> capture_pieces 8 (side_start, side_start) b1
-- ([4,4,4,4,0,5,1],[5,5,4,4,4,4,0])


--update_available_actions :: Board -> ([Action], [Action]) 
--win :: Board -> Bool

instance Show Action where
    show (Action i) = show i


-- TODO: remove these
-- Thoughts on implementation:
--
-- mancala :: Game 
-- - move stones counterclockwise + possibly capture
--      - empty the selected pocket (aka Action)
--      - update stone count in pockets couterclockwise (include the player's own store, but
--      skip opponent's)
--      - possibly capture opponent's stones if the last stone ends up in an
--      empty pocket on our side
-- - update available actions for both players (aka indices of non-empty pockets)
-- - determine the result of the turn
--      - end of game? (remeber to collect any remaining pieces)
--      - end of turn?
--      - another turn because the last piece ends up in the store?
--
-- simple_player :: Player
-- - make the move that brings the greatest number of stones into his/her store
-- 
-- mm_player :: Player
-- - pretty much the same as Minimax, the only difference being a player can
--      take multiple turns (thus we need to change the value function)
--
-- IO stuff
-- - we can modify Play.hs so that a player can take multiple turns






