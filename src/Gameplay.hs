module Gameplay
(
  playTwoPlayer,
  playWithAI,
  playAIvsAI
) where

import qualified Data.Map as M
import System.Random
import Board
import Interpreter
import Moves
import Utils
import AI

playTwoPlayer :: Coin -> IO Move
playTwoPlayer coin = eUserTurn 0 initBoard coin ""

playWithAI :: AILevel -> Coin -> IO Move
playWithAI lev coin = eUserTurn lev initBoard coin ""

playAIvsAI :: AILevel -> IO Move   -- For testing purpose
playAIvsAI level = playAIvsAI' level initBoard WHITE

playAIvsAI' :: AILevel -> BoardMap -> Coin -> IO Move 
playAIvsAI' 0 _ _ = error ("AI Level 0 is Invalid")
playAIvsAI' lev bm coin
  | isGameOver bm coin =
    do showResult bm
       return ((0,0), WHITE) -- Dummy return
  | otherwise          =
      do
        (newbm, cell) <- selectAIMove lev bm coin
        playAIvsAI' lev newbm (notCoin coin)

-- execute User Turn
eUserTurn :: AILevel -> BoardMap -> Coin -> String -> IO Move
eUserTurn lev bm coin msg
  | isGameOver bm coin =
    do showResult bm
       return ((0,0), WHITE) -- Dummy return
  | otherwise          =
      do
        move <- movePrompt bm coin msg
        case (tryMove bm move) of
          Nothing    -> eUserTurn lev bm coin "Invalid Move !!"
          Just newbm -> if lev == 0  -- two player game
                        then eUserTurn lev newbm (notCoin coin) ""
                        else eAITurn lev newbm (notCoin coin)

-- execute AI Turn (should never be called with lev = 0)
eAITurn :: AILevel -> BoardMap -> Coin -> IO Move
eAITurn 0 _ _ = error ("AI Level 0 is Invalid")
eAITurn lev bm coin
  | isGameOver bm coin =
    do showResult bm
       return ((0,0), WHITE) -- Dummy return
  | otherwise          =
      do
        (newbm, cell) <- selectAIMove lev bm coin
        eUserTurn lev newbm (notCoin coin) ("Computer's Move is " ++ show cell)

-- Need to make sure there is atleast one valid move
selectAIMove :: AILevel -> BoardMap -> Coin -> IO Attempt
selectAIMove lev bm coin = let moves = getAIMoves lev bm coin
                           in do random <- getStdRandom (randomR (0, lev-1))
                                 return (safeIndexValue moves random)
