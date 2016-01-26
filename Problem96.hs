module Problem96 where

import Data.Array.IArray (elems,(!))

import Sudoku.Sudoku
import Util

main = do
  contents <- readFile "sudoku.txt"
  let sols = map (f . head . solve . parseBoard. tail) . chunks 10 .
               lines $ contents
      f b = let [x,y,z] = map (\(Known n) -> n) . take 3 . elems . fields $ b
            in  100*x + 10*y + z
  print $ sum sols

