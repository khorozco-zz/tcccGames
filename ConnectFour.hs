module ConnectFour
( createBoardCF, replicateCF
) where

import TicTacToe

data Players = X
             | O
    deriving Show

type Square = Either Char Players

main = undefined

replicateCF :: Int -> String -> String -> [String]
replicateCF 0 _ _ = [multiplyRows 7]
replicateCF n s1 s2 = s1:s2:replicateCF (n-1) s1 s2

createBoardCF :: IO ()
createBoardCF = putStr $ unlines $ replicateCF 6 (multiplyRows 7) (multiplyColumns 7)