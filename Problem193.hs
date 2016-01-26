module Problem193 where

import Data.Array
import Data.List
import Data.Maybe
import Data.Numbers.Primes (primeFactors)
import Sieve

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _   = False

isSquareFree :: Integer -> Bool
isSquareFree = all isSingleton . group . primeFactors

count_spec :: Integer -> Integer
count_spec = genericLength . filter isSquareFree . enumFromTo 1

squareFreeSC :: SieveCombinator Int (Maybe Int)
squareFreeSC = SieveCombinator
    { one     = Just 0
    , combine = \p k _ sf_q _ -> if k == 1 then fmap succ sf_q else Nothing
    , isPrime = \_ sf_n -> sf_n == Just 1
    }

count :: Integer -> Integer
count n = let m :: Int
              m = floor . sqrt . fromIntegral $ n
              table = sieveTable squareFreeSC m
          in  sum [ n `quot` (if even (fromJust sf_k) then k'*k' else -k'*k') | k <- [1..m], let sf_k = table ! k, isJust sf_k, let k' = fromIntegral k ]

main :: IO ()
main = print $ count (2^50)