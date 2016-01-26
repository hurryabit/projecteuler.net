module Problem258 where

import Data.List

newtype Matrix a = M { unM :: [[a]] }

(<*>) :: Num a => Matrix a -> Matrix a -> Matrix a
(M xss) <*> (M yss) = M [ [ sum (zipWith (*) xs ys) | ys <- yssT ] | xs <- xss ]
  where yssT = transpose yss

instance Show a => Show (Matrix a) where
  show = unlines . map show . unM

fibM :: Num a => Int -> Matrix a
fibM = 

lfibs :: Int -> [Int]
lfibs m = lfs
  where lfs = replicate 2000 1 ++ zipWith (\x y -> (x+y) `mod` m) lfs (tail lfs)

glfibs = map (\ns -> (length ns,head ns)) . group . lfibs
