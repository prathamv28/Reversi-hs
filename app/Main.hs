module Main where

import Interpreter
import Board
import Moves
import Gameplay

main :: IO ()
main = do showBoard initBoard
          putStrLn ((replicate 21 '-') ++ "  Welcome to Reversi !!  " ++ (replicate 21 '-'))
          gp   <- promptOptions "Gameplay Mode" gameplayOpts ""
          coin <- promptOptions "Player 1 Coin" coinOpts ""
          p <- case gp of
                 VsPlayer -> playTwoPlayer coin
                 VsCPU    -> do level <- promptOptions "Level" levelOpts ""
                                playWithAI level coin
          return ()
