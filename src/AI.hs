module AI
(
  getAIMoves,
  AILevel
) where

import Data.List
import Utils
import Board

-- Lower the value of CPULevel, better the game of AI
-- 0 Means Two Player Game
-- Best  = 1
-- Worst = 10
type AILevel = Int

-- returns moves that AI considers taking level into account
getAIMoves :: AILevel -> BoardMap -> Coin -> [Attempt]
getAIMoves lev bm coin = take lev sortedMoves
  where sortMoves = sortValidMoves bm coin
        sortedMoves = sortMoves (validCornerMoves bm coin) ++
                      sortMoves (getValidMoves bm coin)

-- sorts attempts in descenting order of gain
sortValidMoves :: BoardMap -> Coin -> [Attempt] -> [Attempt]
sortValidMoves bm coin attempts = sortBy sortfunc attempts
  where ord = if coin == WHITE then fst else snd
        delta newbm = ord (deltaScore bm newbm)
        sortfunc    = \(bm1, _) (bm2, _) -> compare (delta bm2) (delta bm1)
