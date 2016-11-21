--module ConnectFour
--( createBoard
--) where

import TicTacToe
import Checkers

data Players = X
             | O
    deriving Show

type Square = Either Char Players

main = do createBoardCK
-- above line may need to change