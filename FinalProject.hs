-- Final Project

module FinalProject where

import TicTacToeGame
import ConnectFourGame
import CheckersGame

main :: IO ()
main = do
     putStrLn "---*---*---*---*---*---*---*--- \n"
     putStrLn "What game do you want to play? \n"
     putStrLn "A: Tic-Tac-Toe \n"
     putStrLn "B: Connect Four \n"
     putStrLn "C: Checkers \n"
     gameChoice <- getLine
     if gameChoice == "A"
         then TicTacToeGame.main
         else if gameChoice == "B"
             then ConnectFourGame.main
             else if gameChoice == "C"
                 then CheckersGame.main
                 else do
                     putStrLn "Please enter a valid letter: \n"
                     FinalProject.main
