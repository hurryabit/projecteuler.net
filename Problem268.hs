module Problem268 where

import Data.Array.IArray
import Numbers
import Tuples

primes100 :: [Integer]
primes100 = takeWhile (<100) primes

countForTuple :: Integer -> [Integer] -> Integer
countForTuple m ps = n - divisibleBy n (reverse qs)
--(n * product (map pred qs)) `div` product qs
  where n = m `div` product ps
        pm = maximum ps
        qs = filter (`notElem` ps) . takeWhile (<pm) $ primes100

solution m = sum . map (countForTuple m) . tuples 4 $ primes100

solutionSlow = length . filter isGood . elems . factorizationArray
  where isGood = not . null . drop 3 . filter ((<100) . fst)

--divisibleBy :: Integer -> [Integer] -> Integer
divisibleBy n = sum .
  foldl (\rs k -> filter (/=0) (map (`quot` k) (n:map negate rs)) ++ rs) []

main = print $ solution (10^16)
