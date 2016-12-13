module Checkers
( createBoardCK, replicateCK
) where

import TicTacToe

data Players = X
             | O
    deriving Show

type Square = Either Char Players

main = undefined

replicateCK :: Int -> String -> String -> [String]
replicateCK 0 _ _ = [multiplyRows 8]
replicateCK n s1 s2 = s1:s2:replicateCK (n-1) s1 s2

createBoardCK :: IO ()
createBoardCK = putStr $ unlines $ replicateCK 8 (multiplyRows 8) (multiplyColumns 8)