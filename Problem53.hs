module Problem53 where

nCr :: [[Integer]]
nCr = [1]:map (\zs -> zipWith (+) (0:zs) (zs++[0])) nCr
