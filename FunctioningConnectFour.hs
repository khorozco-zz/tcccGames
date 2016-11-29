module ConnectFour where

import Data.List (transpose, intersperse)
import Data.Foldable (asum)

-- initial board creation
initialBoard :: IO ()
initialBoard = putStr $ unlines ["\n",
                                "Let's begin!"]

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

-- add last | to this function?
displayBoard :: Board -> String
displayBoard xs = unlines
             . surround vertex . map (concat . surround' line . map show) $ xs
    where line = "|"
          vertex = "+---+---+---+---+---+---+---" ++ "+"

surround :: a -> [a] -> [a]
surround x ys = x : intersperse x ys ++ [x]
-- returns [x,ys[1],x,ys[2],x,ys[3]]

surround' :: a -> [a] -> [a]
surround' x ys = x : intersperse x ys

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

getCoordinates :: Int -> Board -> (Int, Int)
getCoordinates n = divMod (n-1) . length

-- finding the nth value in a list
nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = []
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n - 1) f xs

fillSquare :: Board -> Int -> Square -> Board
fillSquare xss n s = nth (row+(boardSize - 2)) (nth col (const s)) xss

    where (row, col) = getCoordinates n xss

-- create function that fills squares after checking all rows

gameOver :: Board -> Bool
gameOver = all (notElem Empty)

checkOpenSquare :: Board -> String -> Either String Int
checkOpenSquare xss s  = case reads s of

    [(n, "")] -> check n
    _         -> Left "Error: Please enter an integer"

    where check n
            -- change n parameters here to modify indices?
            | n < 1 || n > length xss  = Left "Please enter integer in range"
            | xss !! 1 !! col /= Empty = Left "Square already taken"
            | otherwise                = Right n

            where (row, col) = getCoordinates n xss

askInput :: Players -> Board -> IO ()
askInput p board = do

    putStrLn $ displayBoard board
    putStrLn $ show p ++ ", make your move"

    putStr $ "(Enter number 1-" ++ show (length board) ++ "): \n"
    number <- getLine

    case checkOpenSquare board number of

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

-- length of board
boardSize :: Int
boardSize = 8

main :: IO ()
main = do
    initialBoard
    gameStep PlayerX (startBoard boardSize)
