module Problem93 where

import Control.Monad
import Data.List
import Data.Maybe (catMaybes)
import Data.Ratio
import Data.Set (Set, fromList, singleton, toList, unions)

solutions = maximum [ (firstMissing (naturals abcd),abcd) | abcd <- subLists 4 [1..9] ]

subLists 0 _ = [[]]
subLists n [] = []
subLists n (x:xs) = map (x:) (subLists (n-1) xs) ++ subLists n xs

firstMissing = fst . head . filter (uncurry (/=)) . zip [1..]

naturals ds = do
  n <- toList . unions . map numbersFixed . permutations $ ds
  let p = numerator n
  guard $ denominator n == 1 && p > 0
  return p

numbersFixed [d] = singleton $ fromIntegral d
numbersFixed ds = fromList . catMaybes $ do
  (ds1,ds2) <- init . tail $ zip (inits ds) (tails ds)
  liftM3 (\op x y -> op x y) (safeDiv:map (\op x y -> Just (x `op` y)) [(+),(-),(*)])
    (toList $ numbersFixed ds1) (toList $ numbersFixed ds2)

safeDiv x y = if y == 0 then Nothing else Just $ x / y
