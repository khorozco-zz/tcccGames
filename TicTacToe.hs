module TicTacToe
( initialBoard
) where

data Players = X
             | O
    deriving Show

type Square = Either Char Players

main = undefined

initialBoard :: IO ()
initialBoard = putStr $ unlines ["Let's begin!","   +---+---+---+"," 3 |   |   |   |","   +---+---+---+"," 2 |   |   |   |","   +---+---+---+"," 1 |   |   |   |","   +---+---+---+","     A   B   C  "]

--   +---+---+---+
-- 3 |   |   |   |
--   +---+---+---+
-- 2 |   |   |   |
--   +---+---+---+
-- 1 |   |   |   |
--   +---+---+---+
--     A   B   C  