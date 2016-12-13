module TicTacToe
( createBoardTTT, multiplyRows, multiplyColumns, replicate'
) where

data Players = X
             | O
    deriving Show

type Square = Either Char Players

main = undefined

multiplyRows :: Int -> String
multiplyRows n = (concat $ replicate n "+---") ++ "+"

multiplyColumns :: Int -> String
multiplyColumns n = (concat $ replicate n "|   ") ++ "|"

replicate' :: Int -> String -> String -> [String]
replicate' 0 _ _ = [multiplyRows 3]
replicate' n s1 s2 = s1:s2:replicate' (n-1) s1 s2

createBoardTTT :: IO ()
createBoardTTT = putStr $ unlines $ replicate' 3 (multiplyRows 3) (multiplyColumns 3)
-- somewhat hard-coded, specific to tic tac toe board
-- no labels for rows and columns

--   +---+---+---+
-- 3 |   |   |   |
--   +---+---+---+
-- 2 |   |   |   |
--   +---+---+---+
-- 1 |   |   |   |
--   +---+---+---+
--     A   B   C  