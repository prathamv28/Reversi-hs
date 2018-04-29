module Interpreter where

import qualified Data.Map as M
import Board
import Moves
import Utils
import AI

data Gameplay = VsCPU | VsPlayer

levelOpts :: [(String, AILevel)]
levelOpts = [
                 ("EASY", 1),
                 ("MEDIUM", 3),
                 ("HARD", 5)
            ]

gameplayOpts :: [(String, Gameplay)]
gameplayOpts = [
                 ("1 Player", VsCPU),
                 ("2 Player", VsPlayer)
               ]

coinOpts :: [(String, Coin)]
coinOpts = [
                 ("Black (" ++ show BLACK ++ ")", BLACK),
                 ("White (" ++ show WHITE ++ ")", WHITE)
           ]

movePrompt :: BoardMap -> Coin -> String -> IO Move
movePrompt bm coin msg =
  do showBoard bm
     showScore bm
     putStrLn ("--> " ++ msg)
     putStrLn ("--> " ++ (show coin) ++ "\'s Turn: ")
     putStrLn "Row Column : "
     line <- getLine
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
                else if black > white
                     then putStrLn "--> BLACK WINS !"
                     else putStrLn "--> WHITE WINS !"

promptOptions :: String -> [(String, a)] -> String -> IO a
promptOptions ques list msg = do putStrLn (if msg == "" then "" else "--> "++msg)
                                 sequence [putStrLn ("["++ show i ++"] " ++ str) |
                                      (i, (str, _)) <- zip [1..(length list)] list]
                                 putStrLn ("Select " ++ ques ++ ": ")
                                 input <- getLine
                                 putStr "\n"
                                 let sel = checkInt input
                                 case sel of
                                   Just n  -> if n>0 && n<=length list
                                              then return . snd $ (list !! (n-1))
                                              else promptOptions ques list "Invalid Option !!"
                                   _       -> promptOptions ques list "Invalid Input Format !!"


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
