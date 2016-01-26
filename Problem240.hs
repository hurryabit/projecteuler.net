module Problem240 where

import Control.Monad
import Data.List

binom :: Integer -> Integer -> Integer
binom n k = foldl (\r (a,b) -> (a*r) `div` b) 1 $ zip [n,n-1..] [1..k]

-- topDice (no of dice) (max pips) (rem top dice) (rem sum)
topDice :: Integer -> Integer -> Integer -> Integer -> Integer
topDice d m 0 0 = m^d
topDice 0 m t s = 0
topDice d 0 t s = 0
topDice d m t s
--    | t > s     = 0
    | t*m < s   = 0
    | t*m == s  = sum [ binom d k * topDice (d-k) (m-1) 0 0 | k <- [t..d] ]
    | t*m > s   = sum [ binom d k * topDice (d-k) (m-1) (t-k) (s-k*m) | k <- [max (s-t*(m-1)) 0..min ((s-t) `div` (m-1)) d] ]
    | otherwise = error $ "uncovered case: topDice " ++ intercalate " " (map show [d,m,t,s])

topDice_spec :: Integer -> Integer -> Integer -> Integer -> Integer
topDice_spec d m t s = genericLength . filter ((s ==) . sum . genericTake t . sortBy (flip compare)) $ foldr (liftM2 (:)) [[]] (genericReplicate d [1..m])

solution = topDice 20 12 10 70