module Gameplay where

import qualified Data.Map as M
import Board
import Interpreter
import Moves
import Utils


playTwoPlayer :: BoardMap -> Coin -> String -> IO Move
playTwoPlayer bm coin msg
  | isGameOver bm coin =
    do showResult bm
       return ((0,0), WHITE) -- Dummy return
  | otherwise          =
      do
        move <- movePrompt bm coin msg
        case (tryMove bm move) of
          Nothing    -> playTwoPlayer bm coin "Invalid Move !!"
          Just newbm -> playTwoPlayer newbm (notCoin coin) ""

