module Problem88 where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Numbers

limit :: Int
limit = 1000000

divs :: Array Int [Int]
divs = divisorsArray limit

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = case x `compare` y of
                        LT -> x:merge xs     (y:ys)
                        EQ -> x:merge xs     ys
                        GT -> y:merge (x:xs) ys

findNothing :: IOArray Int (Maybe Int) -> Int -> IO Int
findNothing arr i = do
    e <- readArray arr i
    case e of
      Just _  -> findNothing arr (i+1)
      Nothing -> return (i-1)

loop :: IOArray Int [Int] -> IOArray Int (Maybe Int) -> Int -> Int -> IO ()
loop sizes results prog n = do
    let ks = takeWhile (\k -> k*k <= n) . tail . sort $ divs ! n
    sizess_n <- forM ks $ \k -> do
                  let l = n `div` k
                      m = n-k-l+1
                  liftM (takeWhile (<= 12000) . map (+m)) (readArray sizes l)
    let sizes_n = foldl1 merge ([1]:sizess_n)
    forM_ sizes_n $ \s -> readArray results s >>= writeArray results s . (`mplus` Just n)
    writeArray sizes n $ sizes_n
    prog' <- findNothing results (prog+1)
    unless (prog' == 12000) $ loop sizes results prog' (n+1)


main = do
    sizes <- newArray_ (2,limit)
    results <- newArray (1,12001) Nothing
    loop sizes results 1 2
    res <- getElems results
    print $ sum . Set.toList . Set.fromList . catMaybes . tail $ res
    