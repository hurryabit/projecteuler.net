module Problem215 where

import Data.Array.IArray
import Data.List

main = print solution32

type Row = [Int]

rowCombos :: [[Row]]
rowCombos = [[]]:[]:[[2]]:zipWith f (tail rowCombos) rowCombos
  where f rcs1 rcs2 = map (2:) rcs1 ++ map (3:) rcs2

indexTable32 :: Array Int [Int]
indexTable32 =
  listArray (0,length rcs-1) $ map (flip findIndices rcs . match) rcs
  where rcs = rowCombos !! 32

solution32 :: Integer
solution32 =
  let (l,u) = bounds indexTable32
      bnds = ((1,l),(10,u))
      table :: Array (Int,Int) Integer
      table = listArray bnds $ map (uncurry f) (range bnds)
      f 1   rci = 1
      f lvl rci = sum [ table ! (lvl-1,rci') | rci' <- indexTable32 ! rci ]
  in  sum $ map (\rci -> table ! (10,rci)) (indices indexTable32)

match :: Row -> Row -> Bool
match r1 r2 = run (scanl1 (+) r1) (scanl1 (+) r2)
  where run [c1] [c2] = c1 == c2
        run (c1:cs1) (c2:cs2) = case compare c1 c2 of
          LT -> run cs1 (c2:cs2)
          GT -> run (c1:cs1) cs2
          EQ -> False
        run _ _ = False
