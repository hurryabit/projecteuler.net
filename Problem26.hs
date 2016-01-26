module Problem26 where

import Data.List

cycleLength n = work n 1 []

work n r rs = case elemIndex r rs of
  Nothing -> let r' = (10*r) `mod` n in work n r' (r:rs)
  Just i  -> i+1

sols = maximum [ (cycleLength n,n) | n <- [1..999] ]
