{-# LANGUAGE NamedFieldPuns #-}
module Sieve
    ( module Sieve
    , module Data.Array
    )
    where

import Data.Array
import ListPlus

data SieveCombinator int a = SieveCombinator
    { one     :: a
    , combine :: int -> Int -> int -> a -> (int -> a) -> a
    , isPrime :: int -> a -> Bool
    }

isPrimeSC :: Integral int => SieveCombinator int Bool
isPrimeSC = SieveCombinator
    { one     = False
    , combine = \p k q _ _ -> k == 1 && q == 1
    , isPrime = curry snd
    }
{-# SPECIALIZE isPrimeSC :: SieveCombinator Int Bool #-}

factorizationSC :: Integral int => SieveCombinator int [(int,Int)]
factorizationSC = SieveCombinator
    { one     = []
    , combine = \p k _ fs_q _ -> (p,k):fs_q
    , isPrime = \_ fs_n -> case fs_n of
                           [(_,1)] -> True
                           _       -> False
    }
{-# SPECIALIZE factorizationSC :: SieveCombinator Int [(Int,Int)] #-}

divisorsSC :: Integral int => SieveCombinator int [int]
divisorsSC = SieveCombinator
    { one     = [1]
    , combine = \p k _ ds_q _ -> foldl1 merge . take (k+1) $ iterate (map (p*)) ds_q
    , isPrime = \_ ds_n -> case ds_n of
                           [_,_] -> True
                           _     -> False
    }
{-# SPECIALIZE divisorsSC :: SieveCombinator Int [Int] #-}

numDivisorsSC :: Integral int => SieveCombinator int int
numDivisorsSC = SieveCombinator
    { one     = 1
    , combine = \p k _ s_q _ -> (fromIntegral k+1)*s_q
    , isPrime = \_ s_q -> s_q == 2
    }
{-# SPECIALIZE numDivisorsSC :: SieveCombinator Int Int #-}

totientSC :: Integral int => SieveCombinator int int
totientSC = SieveCombinator
    { one     = 1
    , combine = \p k _ phi_q _ -> (p-1)*p^(k-1)*phi_q
    , isPrime = \n phi_n -> phi_n == n-1
    }
{-# SPECIALIZE totientSC :: SieveCombinator Int Int #-}

sigmaSC :: Integral int => Int -> SieveCombinator int int
sigmaSC r = SieveCombinator
    { one     = 1
    , combine = \p k _ sig_q _ -> let p2r = p^r in (p2r^(k+1) - 1) `div` (p2r-1) * sig_q
    , isPrime = \n sig_n -> sig_n == n^r + 1
    }
{-# SPECIALIZE sigmaSC :: Int -> SieveCombinator Int Int #-}

sieveTable :: (Integral int, Ix int) => SieveCombinator int a -> int -> Array int a
sieveTable sc = fst . sieveTablePrimes sc
{-# SPECIALIZE sieveTable :: SieveCombinator Int Bool        -> Int -> Array Int Bool        #-}
{-# SPECIALIZE sieveTable :: SieveCombinator Int Int         -> Int -> Array Int Int         #-}
{-# SPECIALIZE sieveTable :: SieveCombinator Int [Int]       -> Int -> Array Int [Int]       #-}
{-# SPECIALIZE sieveTable :: SieveCombinator Int [(Int,Int)] -> Int -> Array Int [(Int,Int)] #-}
{-# SPECIALIZE sieveTable :: SieveCombinator Int a           -> Int -> Array Int a           #-}

sieveTablePrimes :: (Integral int, Ix int) => SieveCombinator int a -> int -> (Array int a,[int])
sieveTablePrimes (SieveCombinator { one, combine, isPrime }) n = (table,primes)
    where table  = listArray (1,n) $ map (run primes) (range (1,n))
          primes = 2:(map fst . filter (uncurry isPrime) . drop 2 . assocs $ table)
          run _      1    = one
          run (p:ps) m
              | m < p*p   = combine m 1 1 one (table !)
              | otherwise = case divMaxPow m p of
                                (0,q) -> run ps m
                                (k,q) -> combine p k q (table ! q) (table !)
{-# SPECIALIZE sieveTablePrimes :: SieveCombinator Int Bool        -> Int -> (Array Int Bool       ,[Int]) #-}
{-# SPECIALIZE sieveTablePrimes :: SieveCombinator Int Int         -> Int -> (Array Int Int        ,[Int]) #-}
{-# SPECIALIZE sieveTablePrimes :: SieveCombinator Int [Int]       -> Int -> (Array Int [Int]      ,[Int]) #-}
{-# SPECIALIZE sieveTablePrimes :: SieveCombinator Int [(Int,Int)] -> Int -> (Array Int [(Int,Int)],[Int]) #-}
{-# SPECIALIZE sieveTablePrimes :: SieveCombinator Int a           -> Int -> (Array Int a          ,[Int]) #-}

-- | divMaxPow n m finds the greatest power m^k that divides n an returns
--   n/m^k and k.
divMaxPow :: Integral int => int -> int -> (Int,int)
divMaxPow n m = divMaxPow' 0 n
  where divMaxPow' k n = case n `divMod` m of
          (q,0) -> divMaxPow' (k+1) q
          (q,_) -> (k,n)
{-# SPECIALIZE divMaxPow :: Int -> Int -> (Int,Int) #-}
