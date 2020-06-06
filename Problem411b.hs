{-# LANGUAGE BangPatterns #-}
import Data.Function
import Data.List hiding (insert)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewR ((:>)))
import Numeric.Search.Range

import Numbers

period :: Int -> Int
period n =
  let (n2,k2) = divMaxPow n 2
      (n3,k3) = divMaxPow n 3
      dlog b m = minimum $ filter (\d -> powMod b d m == 1 `mod` m) (divisors (phi m))
  in max k2 k3 + lcm (dlog 2 n2) (dlog 3 n3)

type Point = (Int,Int)

stations :: Int -> Int -> Int -> [Point]
stations a b n = take (min (2*n+1) (period n)) $ zip (powers a) (powers b)
  where powers c = iterate (\i -> c*i `mod` n) 1

initialize :: [(Int,Int)] -> [[Int]]
initialize = map (map snd) . groupBy ((==) `on` fst) . sort


seqLast :: Seq a -> a
seqLast xs =
  let _ :> x = Seq.viewr xs
  in  x

-- Return the longest non-decreasing subsequence of an input sequence of integers
nonDecreasing :: [Int] -> [Int]
nonDecreasing = reverse . seqLast . makeM
makeM = foldl updateM Seq.empty
updateM m x
  | Seq.null m = Seq.singleton [x]
  | otherwise  = case may_insert_idx of
      Nothing         -> m |> (x:(seqLast m))
      Just 0          -> Seq.update 0 [x] m
      Just insert_idx -> Seq.update insert_idx (x:(Seq.index m (insert_idx - 1))) m
  where
    may_insert_idx = searchFromTo (\idx -> x < (head (Seq.index m idx))) 0 ((Seq.length m) - 1)

lndsubSolve :: Int -> Int
lndsubSolve n =
  let init23 = concat $ initialize $ stations 2 3 n
  in  Seq.length $ makeM init23

main = do
    let k = 17
--  sols <- forM [1 .. 30] $ \k -> do
    let s = lndsubSolve (k^5)
    putStrLn $ "S(" ++ show k ++ "^5) = " ++ show s
--    return s
--  print $ sum sols
