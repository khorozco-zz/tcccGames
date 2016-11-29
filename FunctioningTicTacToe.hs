module TicTacToe where

import Data.List (transpose, intersperse)
import Data.Foldable (asum)

--datatype for game players
data Players = PlayerX | PlayerO
    deriving (Eq)

instance Show Players where
    show PlayerX = "Player X"
    show PlayerO = "Player O"


switchPlayers :: Players -> Players
switchPlayers PlayerX = PlayerO
switchPlayers PlayerO = PlayerX

toPlayers :: Square -> Players
toPlayers X = PlayerX
toPlayers O = PlayerO

fromPlayers :: Players -> Square
fromPlayers PlayerX = X
fromPlayers PlayerO = O

-- datatype for gameboard squares
data Square = X
            | O
            | Empty
    deriving (Eq)

instance Show Square where
    show X     = " X "
    show O     = " O "
    show Empty = "   "

-- the gameboard
type Board = [[Square]]

displayBoard :: Board -> String
displayBoard xs = unlines
             . surround vertex . map (concat . surround line . map show) $ xs
    where line = "|"
          vertex = "+---+---+---" ++ "+"

surround :: a -> [a] -> [a]
surround x ys = x : intersperse x ys ++ [x]

getCoordinates :: Int -> Board -> (Int, Int)
getCoordinates n = divMod (n - 1) . length

nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = []
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n - 1) f xs

fillSquare :: Board -> Int -> Square -> Board
fillSquare xs n s = nth row (nth col (const s)) xs
    where (row, col) = getCoordinates n xs

getInput :: Players -> Board -> IO ()
getInput p board = do
    putStrLn $ displayBoard board
    putStrLn $ show p ++ ", make your move"
    putStr $ "(Enter number 1-" ++ show (length board ^ 2) ++ "): \n"
    move1Num <- getLine
    case checkFreeSquare board move1Num of
        Left s  -> putStrLn ("Invalid input: " ++ s) >> gameStatus p board
        Right n -> let cell = fromPlayers p
                       next = switchPlayers p
                   in gameStatus next (fillSquare board n cell)

checkFreeSquare :: Board -> String -> Either String Int
checkFreeSquare xs s  = case reads s of
   [(n, "")] -> check n
   _         -> Left "Error: Please enter an integer"
   where check n
           | n < 1 || n > length xs ^ 2 = Left "Please enter integer in range"
           | xs !! row !! col /= Empty  = Left "Square already taken"
           | otherwise                   = Right n
           where (row, col) = getCoordinates n xs

checkWinner :: Board -> Maybe Players
checkWinner ys = asum . map winner $ diag : rev : cols ++ ys
   where cols = transpose ys
         diag = zipWith (!!) ys           [0..]
         rev = zipWith (!!) (reverse ys) [0..]
         winner (x:xs) = if all (==x) xs && x /= Empty
                             then Just (toPlayers x)
                             else Nothing

gameStatus :: Players -> Board -> IO ()
gameStatus p board = case checkWinner board of
    Just winner -> do
        putStrLn $ displayBoard board
        putStrLn $ show winner ++ " wins!"
    Nothing -> do
        if endGame board
            then do
                putStrLn $ displayBoard board
                putStrLn "It's a draw"
        else getInput p board

endGame :: Board -> Bool
endGame = all (notElem Empty)

startBoard :: Int -> Board
startBoard x = replicate x (replicate x Empty)

initialBoard :: IO ()
initialBoard = putStr $ unlines ["\n",
                                "Let's begin!"]

-- length of board
boardSize :: Int
boardSize = 3

main :: IO ()
main = do
    initialBoard
    gameStatus PlayerX (startBoard boardSize)
