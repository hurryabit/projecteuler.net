module Problem37 where

import Control.Monad
import Data.List

import Primes

truncablesFR :: [Int]
truncablesFR = concat . take 5 . tail .
  iterate (filter isPrime . flip (liftM2 (\p d -> 10*p+d)) [1,3,7,9]) $
  [2,3,5,7]

isTruncableFL :: Int -> Bool
isTruncableFL = all (isPrime . read) . init . tails . show
