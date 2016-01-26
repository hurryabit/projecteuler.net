module Problem245 where

import Numbers

isSolution :: [Integer] -> Bool
isSolution ps = let n = product ps
                    m = product . map pred $ ps
                in  (n-1) `mod` (n-m) == 0

candidates :: Integer -> [[Integer]]
candidates limit = filter (not . null . drop 1) .
  takeWhile (\(n:_) -> n*n<limit) . run 1 . drop 1 $ primes
  where run :: Integer -> [Integer] -> [[Integer]]
        run n (p:ps)
          | n'<= limit  = map (p:) (run n' ps') ++ run n ps
          | otherwise   = [[]]
          where n' = n*p
                ps' = filter (\q -> q `mod` p /= 1) ps

solutions :: Integer -> [[Integer]]
solutions = filter isSolution . candidates

main = print . solutions $ 10^8
