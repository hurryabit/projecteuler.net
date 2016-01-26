module Problem114 where

import Data.Array
import Data.Function

sol = fix (\a -> listArray (-1,50) $ 1:1:1:1:
        [ sum . map (a!) $ (n-1):[-1..n-4] | n <- [3..50] ]) ! 50
