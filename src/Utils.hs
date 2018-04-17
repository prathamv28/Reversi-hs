module Utils where

import qualified Data.Map as M
import Board
import Moves
import Data.List

type Attempt = (BoardMap, Cell)
type Score = (Int, Int) -- (WHITE, BLACK)

getScores :: BoardMap -> Score
getScores bm = (count WHITE, count BLACK)
  where
    coinList   = [snd pair | pair <- M.toList bm]
    count      = \coin -> length $ filter (== coin) coinList

-- Calculate score difference between 1st and 2nd board
deltaScore :: BoardMap -> BoardMap -> Score
deltaScore bm1 bm2 = let (w1, b1) = getScores bm1
                         (w2, b2) = getScores bm2
                     in (w2-w1, b2-b1)

isGameOver :: BoardMap -> Coin -> Bool
isGameOver bm turn
  | M.size bm == 8*8       = True  -- Board Full
  | length validMoves == 0 = True  -- No moves left for this turn
  | otherwise              = False
    where
      validMoves = getValidMoves bm turn


getValidMoves :: BoardMap -> Coin -> [Attempt]
getValidMoves bm coin = [ let Just b = board x y in (b, (x,y)) |
                               x <- [1..8],
                               y <- [1..8],
                               M.notMember (x,y) bm && board x y /= Nothing
                             ]
  where board x y = tryMove bm ((x,y), coin)

validCornerMoves :: BoardMap -> Coin -> [Attempt]
validCornerMoves bm coin = [ let Just b = board x y in (b, (x,y)) |
                               x <- [1, 8],
                               y <- [1, 8],
                               M.notMember (x,y) bm && board x y /= Nothing
                             ]
  where board x y = tryMove bm ((x,y), coin)

checkInt :: String -> Maybe Int
checkInt str = case reads str of
  [(i, [])] -> Just i
  _         -> Nothing

-- Need to make sure list isn't empty
safeIndexValue :: [a] -> Int -> a
safeIndexValue [] _ = error ("Cannot Access SafeIndex")
safeIndexValue list index = if index >= length list
                            then head . reverse $ list
                            else list !! index
