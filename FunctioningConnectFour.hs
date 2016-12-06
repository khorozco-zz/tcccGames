module ConnectFour where

import Data.List (transpose, intersperse)
import Data.Foldable (asum)
import Data.Char (digitToInt)

-- initial board creation
startMessage :: IO ()
startMessage = putStr $ unlines ["\n",
                                "\t Let's begin!"]

-- called in main, where x is the size of the board
startBoard :: Int -> Board
startBoard x = replicate x (replicate (x) Empty)

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
             . surround vertex . map (concat . surround line . map show) $ reverse (transpose xs)
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
checkWinner ys = asum . map winner $ diag : rev : cols ++ ys
    where cols = transpose ys
          diag = zipWith (!!) ys           [0..]
          rev  = zipWith (!!) (reverse ys) [0..]
          winner (x:y:z:w:xs) = if x /= Empty && (x == y) && (y == z) && (z == w)
                              then Just (toPlayers x)
                              else Nothing

getCoordinates :: Int -> Board -> (Int, Int)
getCoordinates n = divMod (n) . length


fillSquare :: Board -> Int -> Square -> Board
fillSquare b@(x:xs) col currentplayer = if col == 0
                                        then (findRow currentplayer (b !! col)) : xs
                                        else x : fillSquare xs (col-1) currentplayer

findRow :: Square -> [Square] -> [Square]
findRow p (r:rs) 
    | (r == Empty)         = p:rs
    | (r == (last (r:rs))) = (r:rs)
    | otherwise            = r:findRow p rs

nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = []
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n - 1) f xs

{-}
findRow' :: Square -> Board -> Int -> Int -> Board
findRow' p b@(r:rs) rows cols = if (b !! rows !! cols == Empty)
                                then (nth cols  r) : rs
                                else findRow' p b (rows-1) cols
-}
-- create function that fills squares after checking all rows

checkOpenSquare :: Board -> String -> Either String Int
checkOpenSquare xs s = case reads s of

    [(colNumber, "")] -> check colNumber
    _         -> Left "Error: Please enter an integer"

    where check colNumber
            | colNumber < 0 || colNumber > (length xs)
                    = Left "Please enter integer in range"
                    -- correct, does not accept input > 8
            | xs !! row !! 6 /= Empty
                    = Left "Column already filled"
            -- hard-coded 6, checks if top row is filled, then input is invalid
            | otherwise
                    = Right row

            where (row, col) = getCoordinates colNumber xs

askInput :: Players -> Board -> IO ()
askInput p board = do

    putStrLn $ displayBoard board
    putStr "  1   2   3   4   5   6   7  \n \n"
    putStrLn $ show p ++ ", make your move"

    putStr $ "(Enter number 1-" ++ show (length board) ++ "): \n"
    colNumber <- getLine
    putStr "\n"

    case checkOpenSquare board colNumber of

        Left s  -> putStrLn ("Invalid input: " ++ s) >> gameStep p board

        Right n -> let cell = fromPlayers p
                       next = switchPlayers p
--                       (row, col) = getCoordinates colNumber board
                       col = digitToInt (head colNumber) - 1

                   in gameStep next (fillSquare board col cell)

gameStep :: Players -> Board -> IO ()
gameStep p board = case checkWinner board of

    Just winner -> do

        putStrLn $ displayBoard board
        putStr "  1   2   3   4   5   6   7  \n \n"
        putStrLn $ "\t" ++ show winner ++ " wins! \n"

    Nothing -> do
        if gameOver board
            then do
                putStrLn $ displayBoard board
                putStr "  1   2   3   4   5   6   7  \n \n"
                putStrLn "It's a draw"
        else askInput p board

gameOver :: Board -> Bool
gameOver = all (notElem Empty)

-- length of board
boardSize :: Int
boardSize = 7

main :: IO ()
main = do
    startMessage
    gameStep PlayerX (startBoard boardSize)