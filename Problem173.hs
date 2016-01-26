module Problem173 where

f :: Integer -> Integer
f p = sum [ q `div` k - k | k <- [1..floor (sqrt (fromIntegral q))] ]
  where q = p `div` 4
