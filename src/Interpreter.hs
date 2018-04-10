module Interpreter where

import Board
import Moves

initGame :: IO()
initGame = do
  putStrLn "Welcome to Reversi!!"

movePrompt :: BoardMap -> Coin -> String -> IO Move
movePrompt bm coin msg =
  do showBoard bm
     putStrLn ("--> " ++ msg)
     putStrLn ("--> " ++ (show coin) ++ "\'s Turn: ")
     putStr "Row Column : "
     line <- getLine
     putStr "\n"
     let inputs = words line
     let row = read (inputs !! 0) :: Index
     let col = read (inputs !! 1) :: Index
     return ((row,col), coin)
