module AI
(
  getAIMove,
  AILevel
) where

import Data.List
import Utils
import Board
type Depth = Int

-- AILevel is the depth upto which AI looks into the game
-- Best  = 64
-- Worst = 1
-- 0 Means not using AI
type AILevel = Int

-- returns AI's Move
getAIMove :: AILevel -> BoardMap -> Coin -> Attempt
getAIMove lev bm coin
  | validMoves /= [] = fst (maximumBy ordfunc moveValueZip)
  | otherwise        = error ("No Valid Moves for AI")
  where
    validMoves   = getValidMoves bm coin
    moveValueZip = [(mv, getBestValue (fst mv) (lev-1) coin) | mv <- validMoves]
    ordfunc      = \(_, v1) (_, v2) -> if coin == WHITE
                                       then compare v1 v2
                                       else compare v2 v1


-- Gives best BValue after a move
getBestValue :: BoardMap -> Depth -> Coin -> BValue
getBestValue bm 0 _ = bValue bm
getBestValue bm depth coin
  | oppMove == Nothing = bValue bm       -- no moves for opponent
  | validMoves == []   = bValue newbm    -- no valid moves after Opponent's move
  | otherwise          = optFun [getBestValue bm' (depth-1) coin | (bm', _) <- validMoves]
  where
      oppMove      = getBestMove bm (notCoin coin)
      newbm        = let Just (nbm,_) = oppMove in nbm
      validMoves   = getValidMoves newbm coin
      optFun       = if coin == WHITE then maximum else minimum
