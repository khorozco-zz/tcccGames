module ConnectFour where

import Data.List (transpose, intersperse)
import Data.Foldable (asum)

-- initial board creation
initialBoard :: IO ()
initialBoard = putStr $ unlines ["\n",
                                "Let's begin!"]

-- called in main, where x is the size of the board
startBoard :: Int -> Board
startBoard x = replicate (x-1) (replicate x Empty)

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

-- shows board
displayBoard :: Board -> String
displayBoard xs = unlines
             . surround vertex . map (concat . surround line . map show) $ xs
    where line = "|"
          vertex = "+---+---+---+---+---+---+---" ++ "+"

-- surrounds every element of ys between x values
surround :: a -> [a] -> [a]
surround x ys = x : intersperse x ys ++ [x]
-- returns [x,ys[1],x,ys[2],x,ys[3]]

-- players
data Players = PlayerX | PlayerO deriving (Eq)

instance Show Players where
    show PlayerX = "Player X"
    show PlayerO = "Player O"

toPlayers :: Square -> Players
toPlayers X = PlayerX
toPlayers O = PlayerO

fromPlayers :: Players -> Square
fromPlayers PlayerX = X
fromPlayers PlayerO = O

switchPlayers :: Players -> Players
switchPlayers PlayerX = PlayerO
switchPlayers PlayerO = PlayerX

checkWinner :: Board -> Maybe Players
checkWinner yss = asum
           . map winner
           $ diag : rev : cols ++ yss

    where cols = transpose yss
          diag = zipWith (!!) yss          [0..]
          rev = zipWith (!!) (reverse yss) [0..]

          winner (x:xs) = if all (==x) xs && x /= Empty
                              then Just (toPlayers x)
                              else Nothing

-- we should not need this bc we will insert by column number only
getCoordinates :: Int -> Board -> (Int, Int)
getCoordinates n = divMod (n-1) . length

-- finding the nth value in a list
nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = []
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n - 1) f xs

fillSquare :: Board -> Int -> Square -> Board
fillSquare xss n s = nth (row+(boardSize - 2)) (nth col (const s)) xss
    -- if row 7 of selected col is filled, recursively check higher rows
    where (row,col) = getCoordinates n xss

findRow :: Square -> [Square] -> [Square]
findRow p (r:rs) = if (r == Empty) then p:rs
                                   else p : findRow p rs

-- create function that fills squares after checking all rows

checkOpenSquare :: Board -> String -> Either String Int
checkOpenSquare xss s  = case reads s of

    [(colNumber, "")] -> check colNumber
    _         -> Left "Error: Please enter an integer"

    where check colNumber
            | colNumber < 1 || colNumber > length xss  = Left "Please enter integer in range" -- correct, does not accept input > 8
            | xss !! colNumber !! 6 /= Empty           = Left "Column already filled"
            -- hard-coded 6, checks if top row is filled, then input is invalid
            | otherwise                                = Right colNumber

            where (row, col) = getCoordinates colNumber xss

askInput :: Players -> Board -> IO ()
askInput p board = do

    putStrLn $ displayBoard board
    putStrLn $ show p ++ ", make your move"

    putStr $ "(Enter number 1-" ++ show (length board) ++ "): \n"
    colNumber <- getLine

    case checkOpenSquare board colNumber of

        Left s  -> putStrLn ("Invalid input: " ++ s) >> gameStep p board

        Right n -> let cell = fromPlayers p
                       next = switchPlayers p

                   in gameStep next (fillSquare board n cell)

gameStep :: Players -> Board -> IO ()
gameStep p board = case checkWinner board of

    Just winner -> do

        putStrLn $ displayBoard board
        putStrLn $ show winner ++ " wins!"

    Nothing -> do
        if gameOver board
            then do
                putStrLn $ displayBoard board
                putStrLn "It's a draw"
        else askInput p board

gameOver :: Board -> Bool
gameOver = all (notElem Empty)

-- length of board
boardSize :: Int
boardSize = 7

main :: IO ()
main = do
    initialBoard
    gameStep PlayerX (startBoard boardSize)
