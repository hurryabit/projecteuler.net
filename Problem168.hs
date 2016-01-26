module Problem168 where

import Data.List

byLastAndMultiple d0 m = let cd:cds = iterate (\(c,d) -> (m*d+c) `divMod` 10) (0,d0)
                             ds = map snd $ cd:takeWhile (cd /=) cds
                             dss = take (100 `div` length ds) (iterate (ds++) ds)
                         in  filter (10 <) $ map (foldr (\d r -> 10*r+fromIntegral d) 0) dss

solution = sum (concat [byLastAndMultiple d0 m | d0 <- [1..9], m <- [1..d0] ]) `mod` (10^5)
