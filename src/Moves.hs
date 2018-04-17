module Moves
  (
    Move,
    tryMove
  ) where

import Data.Map as M
import Board

type Move = (Cell, Coin)
type Direction = Int
-- 1:Up  , 2:Up-right , 3:Right, 4:Right-down,
-- 5:Down, 6:Down-Left, 7: Left, 8:Left-Up

-- Tries a move
-- Returns new Board if move is valid, else Nothing
tryMove :: BoardMap -> Move -> Maybe BoardMap
tryMove bm move =
  do newbm <- flipCoins bm move 1
     if newbm == bm then Nothing
     else putCoin newbm move

-- returns Nothing when move is invalid
-- otherwise returns new board
flipCoins :: BoardMap -> Move -> Direction -> Maybe BoardMap
flipCoins bm (cell, coin) dir
  | not . onBoard $ cell = Nothing
  | dir>=1 && dir<=8     = case res of
                             Nothing    -> flipCoins bm (cell, coin) nextdir
                             Just newbm -> flipCoins newbm (cell, coin) nextdir
  | otherwise            = Just bm
  where res      = flipCoinsInDir bm coin nextCell dir
        nextCell = getNextCellInDir cell dir
        nextdir  = dir+1


flipCoinsInDir :: BoardMap -> Coin -> Cell -> Direction -> Maybe BoardMap
flipCoinsInDir bm coin cell dir
  | (not . onBoard $ cell) || notMember cell bm = Nothing -- no next coin
  | bm M.! cell == coin                         = Just bm -- next coin reached
  | otherwise = let next       = getNextCellInDir cell dir -- continue search
                    newbm      = flipCoinInCell bm cell
                in do
      res <- flipCoinsInDir bm coin next dir
      return (flipCoinInCell res cell)

flipCoinInCell :: BoardMap -> Cell -> BoardMap
flipCoinInCell bm cell = M.insert cell (notCoin (bm M.! cell)) bm

getNextCellInDir :: Cell -> Direction -> Cell
getNextCellInDir (x,y) dir = case dir of
  1         -> (x-1, y)
  2         -> (x-1, y+1)
  3         -> (x, y+1)
  4         -> (x+1, y+1)
  5         -> (x+1, y)
  6         -> (x+1, y-1)
  7         -> (x, y-1)
  8         -> (x-1, y-1)
  otherwise -> error ("Invalid Direction Access")

putCoin :: BoardMap -> Move -> Maybe BoardMap
putCoin bm (cell, coin) | member cell bm = Nothing
                        | otherwise      = Just (M.insert cell coin bm)
