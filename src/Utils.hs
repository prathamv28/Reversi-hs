module Utils where

import qualified Data.Map as M
import Board
import Moves

type Score = (Int, Int) -- (WHITE, BLACK)

getScores :: BoardMap -> Score
getScores bm = (count WHITE, count BLACK)
  where
    coinList   = [snd pair | pair <- M.toList bm]
    count      = \coin -> length $ filter (== coin) coinList

isGameOver :: BoardMap -> Coin -> Bool
isGameOver bm turn
  | M.size bm == 8*8       = True  -- Board Full
  | length validMoves == 0 = True  -- No moves left for this turn
  | otherwise              = False
    where
      validMoves = [ move
                   | Just move <- [tryMove bm ((x,y), turn)
                   | x <- [1..8], y <- [1..8], M.notMember (x,y) bm]]

checkInt :: String -> Maybe Int
checkInt str = case reads str of
  [(i, [])] -> Just i
  _         -> Nothing
