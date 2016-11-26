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

stringBoard :: [String]
stringBoard = replicateCF 6 (multiplyRows 7) (multiplyColumns 7)
-- prints full board in array of strings

get6th :: [String] -> String
get6th [_, _, _, _, _, _, _, _, _, _, _, a, _] = a
-- get6th stringBoard returns bottom row of board

findFst :: Int -> [(Int, Integer)]
findFst n = filter ((==n).fst) (zip [1..] [2,6,10,14,18,22,26])
-- gets correct index, so when user wants to insert in column 3, it replaces column 10, etc.

boardTuples :: [(Int, Char)]
boardTuples = zip [0..] (get6th stringBoard)

replace :: Int -> [(Int, Char)] -> [(Int, Char)]
replace n boardTuples = map (\f@(i, c) -> if (i == n && c == ' ') then (n, 'X') else f) (boardTuples)
-- replaces snd of tuple with fst = n
-- does not save the new array
-- change 'X' to player, Player should be part of type signature
-- change n to make use of findFst
-- eventually use boardTuples to create stringBoard to create createBoardCF

-- function that gets correct snd value from findFst and uses it to replace snd of the tuple in zip [0..] (get6th stringBoard)

--findColumn :: Int -> [(Int, Integer)]
--findColumn n = filter ((==n).fst) (findFst n)
--findColumn x = filter ((==findFst x.fst).fst) (zip [0..] (get6th stringBoard))

--findColumn :: (Num a, Ord a) => Int -> [(a, Char)] -> Char
--findColumn n [x:xs] = if (n == fst (head x)) then (snd (head x))
--                                             else findColumn n xs
-- findColumn zip [0..] (get6th stringBoard)
-- filter ((==1).fst) (zip [0..] (get6th stringBoard))
-- actual spaces are: 2, 6, 10, 14, 18, 22, 26
--                    1, 2,  3,  4,  5,  6,  7

-- zip [0..] "|   |   |   |   |   |   |   |" == zip [0..] (snd(head(tail(zip [0..] stringBoard))))