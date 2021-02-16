module Mancala where

-- To run it, try:
-- ghci
-- :load Mancala
--

type Board = ([Int], [Int]) -- number of stones for two players, including stores 
type Store = (Player, Int) -- index of a player's store in [Int]
type InternalState = (Board, Store, Store) 
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

mancala :: Game

win :: InternalState -> Bool

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






