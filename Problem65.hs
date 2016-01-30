module Problem65 where

import Data.Char
import Data.Ratio

sol = sum . map digitToInt . show . numerator . foldr1 (\d f -> d+1/f) . take 100 $ 2:concatMap (\k -> [1,2*k,1]) [1..]
