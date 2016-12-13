module CheckersGame where

import Data.List (transpose, intersperse)
import Data.Foldable (asum)
import Data.Char (digitToInt)

startMessage :: IO ()
startMessage = putStr $ unlines ["\n",
                                "\t Let's begin!"]

-- initial board
startBoard :: Board
startBoard = [[X, Empty, X, Empty, Empty, Empty, O, Empty],
                [Empty, X, Empty, Empty, Empty, O, Empty, O],
                [X, Empty, X, Empty, Empty, Empty, O, Empty],
                [Empty, X, Empty, Empty, Empty, O, Empty, O],
                [X, Empty, X, Empty, Empty, Empty, O, Empty],
                [Empty, X, Empty, Empty, Empty, O, Empty, O],
                [X, Empty, X, Empty, Empty, Empty, O, Empty],
                [Empty, X, Empty, Empty, Empty, O, Empty, O],
                [Eight, Seven, Six, Five, Four, Three, Two, One]]

-- datatype for gameboard squares and row numbers
data Square = X
            | O
            | XX -- kings
            | OO -- kings
            | Empty
            | One | Two | Three | Four | Five | Six | Seven | Eight -- row num
    deriving (Eq)

instance Show Square where
    show X      = " x "
    show O      = " o "
    show XX     = " X "
    show OO     = " O "
    show Empty  = "   "
    show One    = "  |1"
    show Two    = "  |2"
    show Three  = "  |3"
    show Four   = "  |4"
    show Five   = "  |5"
    show Six    = "  |6"
    show Seven  = "  |7"
    show Eight  = "  |8"

-- the gameboard
type Board = [[Square]]

-- shows board
displayBoard :: Board -> String
displayBoard xs = unlines
             . surround vertex . map (concat . surround line . map show)
             $ reverse (transpose xs)
    where line = "|"
          vertex = "+---+---+---+---+---+---+---+---+"

-- surrounds every element of ys between x values
surround :: a -> [a] -> [a]
surround x ys = x : intersperse x ys ++ [x]
-- returns [x,ys[1],x,ys[2],x,ys[3]]

-- players X and O
data Players = PlayerX | PlayerO deriving (Eq)

instance Show Players where
    show PlayerX = "Player X"
    show PlayerO = "Player O"

toPlayers :: Square -> Players
toPlayers X = PlayerX
toPlayers XX = PlayerX
toPlayers O = PlayerO
toPlayers OO = PlayerO

fromPlayers :: Players -> Square
fromPlayers PlayerX = X
fromPlayers PlayerO = O

switchPlayers :: Players -> Players
switchPlayers PlayerX = PlayerO
switchPlayers PlayerO = PlayerX

checkWinner :: Board -> Maybe Players
checkWinner ys = asum . map winner $ ys
    where winner (x:xs) = if all (==x) xs
                            then Just (toPlayers x)
                            else Nothing

-- takes current board, person's input, current player and outputs new board
fillSquare :: Board -> String -> Square -> Board
fillSquare b playerInput currentplayer = do
    let fromLetter = if (playerInput !! 0 == 'A')
                    then 0
                    else if (playerInput !! 0 == 'B')
                        then 1
                        else if (playerInput !! 0 == 'C')
                            then 2
                            else if (playerInput !! 0 == 'D')
                                then 3
                                else if (playerInput !! 0 == 'E')
                                    then 4
                                    else if (playerInput !! 0 == 'F')
                                        then 5
                                        else if (playerInput !! 0 == 'G')
                                            then 6
                                            else if (playerInput !! 0 == 'H')
                                                then 7
                                                else 0
        fromInt = digitToInt (playerInput !! 1)

        toLetter = if (playerInput !! 3 == 'A')
                 then 0
                 else if (playerInput !! 3 == 'B')
                     then 1
                     else if (playerInput !! 3 == 'C')
                         then 2
                         else if (playerInput !! 3 == 'D')
                             then 3
                             else if (playerInput !! 3 == 'E')
                                 then 4
                                 else if (playerInput !! 3 == 'F')
                                     then 5
                                     else if (playerInput !! 3 == 'G')
                                         then 6
                                         else if (playerInput !! 3 == 'H')
                                             then 7
                                             else 0
        toInt = digitToInt (playerInput !! 4)
        in movePieces b currentplayer fromLetter fromInt toLetter toInt

-- picks up old piece and puts down new piece
movePieces :: Board -> Square -> Int -> Int -> Int -> Int -> Board
movePieces b cP fromLetter fromInt toLetter toInt =
    if fromLetter == toLetter && fromInt == toInt
        then b
    else changeFromEmpty (changeToEmpty b fromLetter fromInt) cP toLetter toInt

-- changes cell from piece to Empty
changeToEmpty :: Board -> Int -> Int -> Board
changeToEmpty b@(x:xs) c d = a ++ [e++(Empty:zs)] ++ ys
    where (a,ys) = splitAt c b
          (e,_:zs) = splitAt d (b!!c)

-- changes cell from Empty to piece
changeFromEmpty :: Board -> Square -> Int -> Int -> Board
changeFromEmpty b@(x:xs) p c d = a ++ [e++(p:zs)] ++ ys
    where (a,ys) = splitAt c b
          (e,_:zs) = splitAt d (b!!c)

-- checks if player has entered a valid letter (A-H)
isValidLetter :: Char -> Bool
isValidLetter c = if (c == 'A' || c == 'B' || c == 'C' || c == 'D'
                    || c == 'E' || c == 'F' || c == 'G' || c == 'H')
                    then True
                    else False

-- checks if player has entered a valid digit (1-8)
isValidDigit :: Char -> Bool
isValidDigit c = if (c == '1' || c == '2' || c == '3' || c == '4'
                    || c == '5' || c == '6' || c == '7' || c == '8')
                    then True
                    else False

-- checks if player has input valid move
checkValidMove :: String -> Bool
checkValidMove (x:y:z:w:v:xs) = if (isValidLetter x && isValidDigit y
                                    && (z == ' ')
                                    && isValidLetter w && isValidDigit v
                                    && xs == [])
                                then True
                                else False

-- play game, make move
gameStep :: Players -> Board -> IO ()
gameStep p board = case checkWinner board of

    Just winner -> do
        putStrLn $ displayBoard board
        putStr "  A   B   C   D   E   F   G   H  \n \n"
        putStrLn $ "\t" ++ show winner ++ " wins! \n"

    Nothing -> do
        putStrLn $ displayBoard board
        putStr "  A   B   C   D   E   F   G   H  \n \n"
        putStrLn $ show p ++ ", enter from and to coordinates,"
        putStr "separated by a space (e.g. A8 B7):\n"
        playerMove <- getLine
        putStr "\n"

        if (checkValidMove playerMove == False)
            then putStrLn ("Invalid input: please enter a valid move")
                    >> gameStep p board
            else let cell = fromPlayers p
                     next = switchPlayers p
                 in gameStep next (fillSquare board playerMove cell)

-- run main
main :: IO ()
main = do
    startMessage
    gameStep PlayerX (startBoard)
