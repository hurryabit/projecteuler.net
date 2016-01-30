import Data.List

exps = 0:zipWith3 (\n e s -> (2*n*e+n-1-s) / n) [1..] exps (scanl1 (+) exps)

sol = exps !! 30
