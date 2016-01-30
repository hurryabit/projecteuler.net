module Problem274 where

import Data.List
import qualified Numbers as N

main = print solution

primes :: [Int]
primes = 3:drop 3 N.primes

divMultiplier :: Int -> Int
divMultiplier p = case p `divMod` 10 of
  (k,1) -> 9*k+1
  (k,3) -> 3*k+1
  (k,7) -> 7*k+5
  (k,9) ->   k+1

solution :: Integer
solution = foldl' (+) 0 . map (fromIntegral . divMultiplier) .
  takeWhile (<10000000) $ primes
