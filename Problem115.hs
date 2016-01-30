module Problem115 where

import Data.Array
import Data.Function


sol = let f m n = fix (\a -> listArray (-1,n) $ replicate (m+1) 1 ++
                    [ sum . map (a!) $ (k-1):[-1..k-m-1] | k <- [m..n] ])
      in  length (takeWhile (<1000000) . elems $ f 50 200) - 1
