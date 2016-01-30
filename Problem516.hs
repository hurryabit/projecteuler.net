import Data.List
import qualified Numbers as N

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = case x `compare` y of
    LT -> x:merge xs     (y:ys)
    GT -> y:merge (x:xs) ys
    EQ -> x:merge xs     ys

hammings :: [Int]
hammings =
    let merge2D ((x:xs):xxs) = x:merge2D (insert xs xxs)
        combine xs ys = merge2D [ [ x*y | x <- xs ] | y <- ys ]
        powers n = iterate (*n) 1
    in  (powers 2 `combine` powers 3) `combine` powers 5

hammingPrimes :: [Int]
hammingPrimes = filter isPrime $ dropWhile (<7) $ map succ $ hammings

primes :: [Int]
primes = N.primes

isPrime :: Int -> Bool
isPrime = N.isPrimeBy primes

trim :: [Int] -> [Int]
trim = takeWhile (<=1000000000000)

solutions :: [Int]
solutions = trim $ foldr (\p xs -> merge xs (map (*p) xs)) hammings $ trim hammingPrimes

main = print $ sum solutions `mod` (2^32)
