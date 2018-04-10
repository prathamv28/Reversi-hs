module Gameplay where

import Board
import Interpreter
import Moves

playTwoPlayer :: BoardMap -> Coin -> String -> IO Move
playTwoPlayer bm coin msg =
  do move <- movePrompt bm coin msg
     case (tryMove bm move) of
       Nothing    -> playTwoPlayer bm coin "Invalid Move !!"
       Just newbm -> playTwoPlayer newbm (notCoin coin) ""

