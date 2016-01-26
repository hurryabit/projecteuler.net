module Problem425 where

import Control.Monad
import Control.Monad.Loops
import Data.Array.Unboxed
import Data.Array.IO
import Data.Array.Unsafe
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.IntSet as Set
import System.IO.Unsafe

import Numbers (isPrimeArray)

limit :: Int
limit = 10000000

primes :: Array Int Bool
primes = isPrimeArray limit

type IntQueue = IORef Set.IntSet

enqueue :: IntQueue -> Int -> IO ()
enqueue q x = modifyIORef' q (Set.insert x)

dequeue :: IntQueue -> IO (Maybe Int)
dequeue q = atomicModifyIORef' q $ \xs ->
    case Set.minView xs of
        Nothing     -> (xs,Nothing)
        Just (y,ys) -> (ys,Just y )

solve :: Int
solve = sum [ p | (p,tp) <- assocs chains, primes ! p, p < tp ]

main = print solve

chains :: UArray Int Int
chains = unsafePerformIO $ do
    let bnd = bounds primes
    mem <- newArray (bounds primes) limit :: IO (IOUArray Int Int)
    writeArray mem 2 1
    que <- newIORef (Set.singleton 2)
    whileJust_ (dequeue que) $ \p -> do
        tp <- readArray mem p
        forM_ (filter (primes !) (connected p)) $ \q -> do
            tq <- readArray mem q
            when (tq == limit) $ do
                writeArray mem q (max p tp)
                enqueue que q
    unsafeFreeze mem

connected :: Int -> [Int]
connected = filter (\n -> 0 < n && n < limit) . map read . filter (not . isPrefixOf "000") . connS . ('0':) . show

connS :: String -> [String]
connS ""     = []
connS (d:ds) = map (:ds) ['0' .. pred d] ++ map (d:) (connS ds) ++ map (:ds) [succ d .. '9']

-- 10^6 -> 532658671