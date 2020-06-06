module Problem601 where


candidates :: Integer -> [Integer]
candidates s
  | n `mod` (s+1) == 0 = []
  | otherwise = [ k+1 | k <- [n, n+n ..], k `mod` (s+1) /= 0 ]
  where
    n = foldl lcm 1 [1..s]

solve :: Integer -> Int
solve i = length (takeWhile (<(4^i)) (candidates i))

main = print $ sum $ map solve [1..31]
