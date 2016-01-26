module Problem112 where

isStraight :: Int -> Bool
isStraight n = and (zipWith (<=) s (tail s)) || and (zipWith (>=) s (tail s))
  where s = show n

straights, solutions :: [(Int,Int)]
straights = zip (scanl (+) 0 (map (fromEnum . isStraight) [1..])) [0..]
solutions = filter (\(s,n) -> 100*s==n) straights
