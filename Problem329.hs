module Problem329 where

import Data.Ratio
import qualified Data.MemoCombinators as Memo
import qualified Data.Numbers.Primes as Primes

isPrime :: Int -> Bool
isPrime = Memo.arrayRange (1,500) Primes.isPrime

left, right :: Int -> Int
left 1    = 2
left n    = n-1
right 500 = 499
right n   = n+1

pr :: [Char] -> Int -> Rational
pr = Memo.memo2 (Memo.boundedList 15 Memo.char) (Memo.arrayRange (1,500)) pr'
    where pr' []     n = 1
          pr' (c:cs) n = let pc = if isPrime n == (c == 'P') then 2%3 else 1%3
                         in  pc * (1%2) * (pr cs (left n) + pr cs (right n))

solution = (1%500) * sum (map (pr "PPPPNNPPPNPPNPN") [1..500])