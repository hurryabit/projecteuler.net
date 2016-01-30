{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Problem155 where

import Data.Array
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as Set

newtype Dbl = Dbl Double
    deriving (Num, Fractional)

instance Eq Dbl where
    (Dbl a) == (Dbl b) = abs (a-b) < 1e-8

instance Ord Dbl where
    compare (Dbl a) (Dbl b)
        | a-b >= 1e-8 = GT
        | b-a >= 1e-8 = LT
        | otherwise   = EQ

capacitances :: Array Int (Set Dbl)
capacitances = listArray size . map f . range $ size
  where size = (1,18)
        f 1 = Set.singleton 1
        f n = Set.fromList $ concat
                [ [ x+y, x+recip y, recip x+y, max (recip x+ recip y) (recip $ recip x+recip y) ]
                | k <- [1..n `div` 2]
                , x <- Set.elems (capacitances ! k)
                , y <- Set.elems (capacitances ! (n-k))
                ]

--d = scanl1 (+) . map length . elems $ capacitances

main = print $ 2 * Set.size (Set.unions (elems capacitances)) - 1
