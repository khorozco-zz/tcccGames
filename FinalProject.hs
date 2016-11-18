-- Final Project

-- GIT STEPS:
-- make sure you're in the correct directory in your terminal
-- $ git pull, pulls changes made (by other person)
-- $ git status, checks to see what files have changed
-- $ git add FinalProject.hs
-- $ git commit -m "commit description here, what changes were made?"
-- $ git push, pushes changes to git so we both see them

-- COMPILER STEPS:
-- $ ghc --make FinalProject, first time only
-- $ ./FinalProject

import TicTacToe
data Players = X
             | O
             | E
    deriving Show

board :: [Players] -> [Players]
board = undefined

main :: IO ()
main = do
     putStrLn "What game do you want to play? \n A: Tic-Tac-Toe \n B: Connect Four \n C: Checkers \n D: Chess"
     gameChoice <- getLine
     initialBoard

-- Next Steps:
-- create initial board
-- can main take helper functions? how does main work?
-- make function that modifies the board every time a move is made
-- function that checks after each move if there is a winner
-- can we loop in main?
-- make the board a list? a list of lists?
-- how to associate grid with actual spot, so A2 with bottom middle square

-- First Steps:
-- get skeleton of one pile per game
-- write code that creates the board per game (checkers chess has things)
-- break up work: both do tictactoe board, one do other three boards, one get more depth on ttt