module Problem500 where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Numbers.Primes

data State = State { queue :: IntSet, result :: Int }

start :: Int -> State
start k = State { queue = IntSet.fromDistinctAscList (take k primes), result = 1 }

step :: State -> State
step (State { queue = q, result = r }) =
    let Just (m,q') = IntSet.minView q
    in  State { queue = IntSet.insert (m*m) q', result = (r*m) `mod` 500500507 }

solve k = result $ iterate step (start k) !! k

main = print $ solve 500500