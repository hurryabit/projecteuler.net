module Problem99 where

import Data.List
import Data.Ord
import System.IO.Unsafe

pairs :: [(Double,Double)]
pairs = map f . lines . unsafePerformIO $ readFile "base_exp.txt"
  where f xs = let (ys,',':zs) = break (== ',') xs in (read ys,read zs)

sol = maximumBy (comparing snd) . zip [1..] . map f $ pairs
  where f (b,e) = e*log b
