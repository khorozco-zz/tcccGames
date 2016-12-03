module ConnectFour where

import Data.List (transpose, intersperse)
{- Play Connect Four game. Alternate between two players. First to make four
    in a row wins. -}

rowSize' :: Int
rowSize' = 6

colSize' :: Int
colSize' = 7

numInARow' :: Int
numInARow' = 4


-- initial board creation

initialBoard :: IO ()
initialBoard = putStr $ unlines ["\n",
                                "Let's begin!"]


{- Function creates list of columns that contain rows -}
startBoard :: Board
startBoard = replicate (colSize') (replicate rowSize' Empty)


displayBoard :: Board -> String
displayBoard xs = do
    unlines . surround vertex . map (concat . surround' line . map show) $ xs
    where line = "|"
          vertex = "+---+---+---+---+---+---+---" ++ "+"

surround :: a -> [a] -> [a]
surround x ys = x : intersperse x ys ++ [x]
-- returns [x,ys[1],x,ys[2],x,ys[3]]

surround' :: a -> [a] -> [a]
surround' x ys = x : intersperse x ys

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

-- players
data Players = PlayerX | PlayerO
    deriving (Eq)

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

{-Function gets move from user and verifies validity of input. If move is
invalid, function recursively calls for user to make another move. Function
takes current board, player, number of rows, and number of columns as
parameters-}
getMove :: Board -> Players -> IO ()
getMove board p = do
    putStrLn $ displayBoard board
    putStrLn $ show p ++ ", make your move"

    putStr $ "(Enter number 1-" ++ show ((length board)+1) ++ "): \n"
    colNumber <- getLine

    if ((colNumber > 1) && (toInt(colNumber) < (colSize'+1))) && (board !! (colNumber-1) !! 0 == Empty)
        then makeMove board p
    else getMove board p


makeMove :: Board -> Players -> IO ()
makeMove board p = putStrLn $ "HELLO"
{-


def get_move(board, player,rows,columns):
    move = int(input(str(player)))     #input = number user enters

    #valid move
    if move in list(range(1,columns+1)) and board[0][move-1] == "  ":
        return move

    #either column is already full or input is out of columns range
    else:
        return get_move(board, "Not valid, try again: ", rows,columns)

def make_move(board, symb, player, move, rows, columns):
    '''Function stores player's move in current board. Function takes current
    board, player's symbol (X or O), player, move, number of rows, and number
    of columns as parameters.'''

    #find lowest available spot in column
    while board[int(rows)-1][int(move)-1] != "  ":
        return make_move(board,symb,player,move,(rows-1),columns)

    #once available spot is found, print character
    if board[int(rows)-1][int(move)-1] == "  ":
        board[int(rows)-1][int(move)-1] = str(symb) + " "
        return board

def alternate_player(turn):
    '''Function determines which player's turn it is, based upon the remainder
    of the turn number. Function takes turn number as a parameter and returns
    which player is up.'''

    player = ""
    if turn % 2 == 0:   #Player 1's turn
        player = "Player 1 (X) move: "
    else:               #turn % 2 == 1; so it is Player 2's turn
        player = "Player 2 (O) move: "
    return player

def alternate_symb(turn):
    '''Function determines which symbol is to be used, based upon which player
    is making a move (which, in turn, is decided by alternate_player function).
    Function takes turn number as a parameter and returns symbol to be used.'''

    symb = ""
    if turn % 2 == 0:  #Player 1's turn
        symb = "X"
    else:              #turn % 2 == 1; so it is Player 2's turn
        symb = "O"
    return symb


def check_horizontal_win(board,turn,move,row):
    '''Checks if move creates a horizontal win. Function takes current board,
    turn number, player's move, and row as parameters.'''
    symb = alternate_symb(turn) + " "

    for i in range(rows):
        if board[i][0] == symb:
            if board[i][1] == symb:
                if board[i][2] == symb:
                    if board[i][3] == symb:
                        return True
                    else:
                        symb = symb
                else:
                    symb = symb
            else:
                symb = symb

        elif board[i][1] == symb:
            if board[i][2] == symb:
                if board[i][3] == symb:
                    if board[i][4] == symb:
                        return True
                    else:
                        symb = symb
                else:
                    symb = symb
            else:
                symb = symb

        elif board[i][2] == symb:
            if board[i][3] == symb:
                if board[i][4] == symb:
                    if board[i][4] == symb:
                        return True
                    else:
                        symb = symb
                else:
                    symb = symb
            else:
                symb = symb

        elif board[i][3] == symb:
            if board[i][4] == symb:
                if board[i][5] == symb:
                    if board[i][6] == symb:
                        return True
                    else:
                        symb = symb
                else:
                    symb = symb
            else:
                symb = symb
        else:
            symb = symb

def check_vertical_win(board,turn,move,row):
    '''Checks if move creates a vertical win. Function takes current board,
    turn number, player's move, and row as parameters.'''

    symb = alternate_symb(turn) + " "
    move = move -1

    if board[row][move] == symb:
        if board[row-1][move] == symb:
            if board[row-2][move] == symb:
                if board[row-3][move] == symb:
                    return True
                else:
                    symb = symb
            else:
                symb = symb
        else:
            symb = symb

    elif board[row-1][move] == symb:
        if board[row-2][move] == symb:
            if board[row-3][move] == symb:
                if board[row-4][move] == symb:
                    return True
                else:
                    symb = symb
            else:
                symb = symb
        else:
            symb = symb

    elif board[row-2][move] == symb:
        if board[row-3][move] == symb:
            if board[row-4][move] == symb:
                if board[row-5][move] == symb:
                    return True
                else:
                    symb = symb
            else:
                symb = symb
        else:
            symb = symb

    elif board[row-3][move] == symb:
        if board[row-4][move] == symb:
            if board[row-5][move] == symb:
                if board[row-6][move] == symb:
                    return True
                else:
                    symb = symb
            else:
                symb = symb
        else:
            symb = symb

def play_game(board,turn,rows,columns):
    '''Run game.'''

    player = ""

    if "  " in board[0]:
        if int(turn)%2 == 0:
            player = "Player 1"
        else:       #turn % 2 == 1:
            player = "Player 2"

        print_board(board,rows,columns)
        move = get_move(board, alternate_player(turn),rows,columns)
        board = make_move(board, alternate_symb(turn), alternate_player(turn),\
        move, rows, columns)

        #CHECK FOR WINNING MOVE
        count = 1
        horz_win = check_horizontal_win(board,turn,move,rows-1)
        if horz_win == True:
            print_board(board,rows,columns)

            print("*** " + str(player) + " wins!")
            return
        else:
            vert_win = check_vertical_win(board,turn,move,rows-1)
            if vert_win == True:
                print_board(board,rows,columns)

                print("*** " + str(player) + " wins!")
                return
            else:
                return play_game(board,turn+1,rows,columns)

    else:
        print_board(board,rows,columns)
        print("*** It's a tie!")
        print("")
        return

#start game automatically (but game does not start playing if module imported)
if __name__ == "__main__":
    turn = 0
    board = initial_board(rows,columns)
    play_game(board,turn,rows,columns)
-}
