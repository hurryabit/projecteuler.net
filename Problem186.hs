module Problem186 where

import Control.Monad
import Data.Array.IO
import Data.IORef
import Data.List

lfg :: [Int]
lfg = [ fromIntegral ((100003-200003*k+300007*k^3) `mod` 1000000)
      | k <- [(1::Integer) .. 55] ] ++
      zipWith (\a b -> (a+b) `mod` 1000000) (drop 31 lfg) lfg

calls :: [(Int,Int)]
calls = filter (uncurry (/=)) $ pair lfg
  where pair (x:y:zs) = (x,y):pair zs

minister :: Int
minister = 524287

simulate :: IO ()
simulate = do
  parts <- newListArray (0,999999) [0..]
  sizes <- newArray (0,999999) 1
  step parts sizes 1 calls

target :: IOUArray Int Int -> Int -> IO Int
target parts c = do
  t <- readArray parts c
  if c == t then return t else target parts t

update :: IOUArray Int Int -> Int -> Int -> IO ()
update parts c t = do
  c' <- readArray parts c
  when (c' /= t) $ do
    writeArray parts c t
    update parts c' t

step :: IOUArray Int Int -> IOUArray Int Int -> Int -> [(Int,Int)] -> IO ()
step parts sizes n ((c1,c2):cs) = do
  t1 <- target parts c1
  t2 <- target parts c2
  when (t1 /= t2) $ do
    let t = min t1 t2
    update parts t1 t
    update parts t2 t
    liftM2 (+) (readArray sizes t1) (readArray sizes t2) >>= writeArray sizes t
  p <- target parts minister
  s <- readArray sizes p
  if s >= 990000 then print n else step parts sizes (n+1) cs

main = simulate
