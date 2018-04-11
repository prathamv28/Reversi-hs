module Board where

import qualified Data.Map as M

data Coin = BLACK | WHITE deriving (Eq)
instance Show Coin where
  show coin = case coin of
    WHITE -> "O"
    BLACK -> "X"

type Index = Int
type Cell = (Index, Index)
type BoardMap = M.Map Cell Coin

initBoard :: BoardMap
initBoard = M.fromList [
  ((4,4), WHITE),
  ((5,5), WHITE),
  ((4,5), BLACK),
  ((5,4), BLACK)]

onBoard :: Cell -> Bool
onBoard (x, y) | x>=1 && x<=8 && y>=1 && y<=8 = True
                 | otherwise                  = False


notCoin :: Coin -> Coin
notCoin coin = case coin of
  WHITE -> BLACK
  BLACK -> WHITE

-- FOR DEBUGGING PERPOSE ONLY
-- p s = putStr s

-- boardToString :: BoardMap -> String
-- boardToString bm =
--   "\n" ++
--   (concat (replicate 8 ("+-------"))) ++
--   "+\n" ++
--   (concat [rowToString bm row | row <- [1..8]]) ++
--   "\n"

-- rowToString :: BoardMap -> Int -> String
-- rowToString bm n =
--   (concat (replicate 8 ("|       "))) ++
--   "|\n" ++
--   (concat [if (bm cell) /= EMPTY
--             then ("|   " ++ (show . bm $ cell) ++ "   ")
--             else ("| " ++ (show cell) ++ " ")
--            | cell <- map ((,) n) [1..8]]) ++
--   "|\n" ++
--   concat (replicate 8 ("|       ")) ++
--   "|\n" ++
--   concat (replicate 8 ("+-------")) ++
--   "+\n"

-- f :: Maybe BoardMap -> IO()
-- f Nothing = putStrLn "NOTHING"
-- f (Just bm) = showBoard bm

-----------------------------------------------
