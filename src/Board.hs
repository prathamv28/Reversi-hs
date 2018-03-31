module Board
  (
    showBoard,
    initBoard,
    putCoin
  )
where

data Coin = BLACK | WHITE | EMPTY deriving (Eq, Show)

type Index = Int
type Cell = (Index, Index)
type BoardMap = Cell -> Coin

showBoard :: BoardMap -> IO ()
showBoard bm = do
  putStr "\n"
  sequence $ replicate 8 (putStr "+-------")
  putStr "+\n"
  sequence [printRow bm row | row <- [1..8]]
  putStr "\n"

putCoin :: Cell -> Coin -> BoardMap -> BoardMap
putCoin cell coin bm
  | validCell cell = \cell' -> if cell' == cell then coin else bm cell'
  | otherwise      = error ("PUTCOIN : Index " ++ show cell ++ " out of bounds ")

initBoard :: BoardMap
initBoard (4,4) = WHITE
initBoard (5,5) = WHITE
initBoard (4,5) = BLACK
initBoard (5,4) = BLACK
initBoard _     = EMPTY

---

validCell :: Cell -> Bool
validCell (x, y) | x>=1 && x<=8 && y>=1 && y<=8 = True
                 | otherwise                    = False


printRow :: BoardMap -> Int -> IO()
printRow bm n = do
  sequence $ replicate 8 (putStr "|       ")
  putStr "|\n"
  sequence [if (bm cell) /= EMPTY
            then putStr ("|   " ++ (convert . bm $ cell) ++ "   ")
            else putStr ("| " ++ (show cell) ++ " ")
           | cell <- map ((,) n) [1..8]]
  putStr "|\n"
  sequence $ replicate 8 (putStr "|       ")
  putStr "|\n"
  sequence $ replicate 8 (putStr "+-------")
  putStr "+\n"

convert :: Coin -> String
convert WHITE = "O"
convert BLACK = "X"

