module Problem187 where

import Data.List
import qualified Numbers as N

main = print solution

primes :: [Int]
primes = N.primes

solution =
  sum . map (\(p,ps) -> length $ takeWhile (<=(100000000 `div` p)) ps) .
  takeWhile ((<10000) . fst) $ zip primes (tails primes)
