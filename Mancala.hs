module Mancala where

-- To run it, try:
-- ghci
-- :load Mancala
--

-- https://endlessgames.com/wp-content/uploads/Mancala_Instructions.pdf
type Pockets = [Int]
type Store = Int
type Side = (Pockets, Store)
type Board = (Side, Side)  -- essentially ([Int], Int, [Int], Int)
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

side_start = ([4, 4, 4, 4, 4, 4], 0)
actions_start = [Action i | i <- [0..5]] 
mancala_start = State (side_start, side_start) (actions_start, actions_start) 


mancala :: Game
mancala move (State ((my_pockets, my_store), (other_pockets, other_store)) (my_actions, other_actions))
--update_board :: Action -> Board -> Board

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



 
--update_available_actions :: Board -> ([Action], [Action]) 
--win :: Board -> Bool

instance Show Action where
    show (Action i) = show i


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






