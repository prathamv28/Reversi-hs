module Interpreter where

import qualified Data.Map as M
import Board
import Moves
import Utils

initGame :: IO()
initGame = do
  putStrLn "Welcome to Reversi!!"

movePrompt :: BoardMap -> Coin -> String -> IO Move
movePrompt bm coin msg =
  do showBoard bm
     showScore bm
     putStrLn ("--> " ++ msg)
     putStrLn ("--> " ++ (show coin) ++ "\'s Turn: ")
     putStr "Row Column : "
     line <- getLine
     putStr "\n"
     let inputs = words line
     let row = checkInt (inputs !! 0)
     let col = checkInt (inputs !! 1)
     case (row, col) of
       (Just r, Just c) -> return ((r,c), coin)
       _                -> movePrompt bm coin "Invalid Input Format"


showResult :: BoardMap -> IO()
showResult bm =
      do putStrLn "--> !! GAME OVER !!"
         showScore bm
         let score = getScores bm
             white = fst score
             black = snd score
             in if white == black
                then putStrLn "--> It's a Tie !"
                else if black > white then putStrLn "--> BLACK WINS !" else putStrLn "--> WHITE WINS !"

showScore :: BoardMap -> IO()
showScore bm = let score = getScores bm in
  do putStrLn ("O = " ++ (show . fst $ score) ++ "   X = " ++ (show . snd $ score))

showBoard :: BoardMap -> IO ()
showBoard bm = do
  putStr "\n"
  sequence $ replicate 8 (putStr "+-------")
  putStr "+\n"
  sequence [printRow bm row | row <- [1..8]]
  putStr "\n"

printRow :: BoardMap -> Int -> IO()
printRow bm n = do
  sequence $ replicate 8 (putStr "|       ")
  putStr "|\n"
  sequence [if (M.member cell bm)
            then putStr ("|   " ++ show (bm M.! cell) ++ "   ")
            else putStr ("| " ++ (show cell) ++ " ")
           | cell <- map ((,) n) [1..8]]
  putStr "|\n"
  sequence $ replicate 8 (putStr "|       ")
  putStr "|\n"
  sequence $ replicate 8 (putStr "+-------")
  putStr "+\n"
