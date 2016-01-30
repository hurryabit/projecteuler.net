module Problem259 where

import Data.MemoTrie
import Data.Ratio
import qualified Data.Set as Set

main :: IO ()
main = print . sum $ [ p | n <- Set.toList $ numbers (1,9), denominator n == 1,
                           let p = numerator n, p > 0 ]

numbers :: (Int,Int) -> Set.Set (Ratio Integer)
numbers = memo $ \r@(a,b) -> if a > b then Set.empty else
            Set.fromList $ fromIntegral (foldl (\n d -> 10*n+d) 0 [a..b]):
              [ n1 `op` n2 | op <- [(+),(-),(*),(/)], c <- [a..b-1],
                             n1 <- Set.toList $ numbers (a,c),
                             n2 <- Set.toList $ numbers (c+1,b), n2 /= 0 ]
