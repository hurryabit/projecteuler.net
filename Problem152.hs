module Problem152 where

import Data.List
import Data.MemoCombinators
import qualified Data.Numbers.Primes as P

dim :: Integer
dim = 80

small n = n <= dim `div` 4

primeFactors = arrayRange (2,dim) P.primeFactors

numbers = filter (all small . primeFactors) [2 .. dim]

denominator = (foldl lcm 1 numbers)^2

k |? n = n `mod` k == 0

choices = memo2 bits bits choices'
    where
    f 2 = (denominator ==) . (2*)
    f p = let p2m = (last . takeWhile (<= dim) $ iterate (p*) 1)^2
          in  (p2m |?)
    choices' p acc = filter (f p) . map (sum . (acc:)) . subsequences .
                        map ((denominator `div`) . (^2)) . filter (\x -> p |? x && all (p >=) (primeFactors x)) $ numbers
        

solution = foldr (=<<) (return 0) (map choices $ takeWhile small P.primes)

main = print $ length solution
