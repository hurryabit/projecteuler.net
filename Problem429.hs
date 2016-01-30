import Data.Bits
import Data.Numbers.Primes

-- powMod x k m = x^k `mod` m
powMod :: Integral a => a -> Int -> a -> a
powMod x k m =
    let run x k y
          | k' == 0   = y'
          | otherwise = run x' k' y'
          where k' = k `shiftR` 1
                x' = x*x `mod` m
                y' = if k `testBit` 0 then x*y `mod` m else y
    in  run x k 1

exponentInFactorial :: Int -> Int -> Int
exponentInFactorial p = sum . takeWhile (0 <) . tail . iterate (`div` p)

factorizationOfFactorial :: Int -> [(Int,Int)]
factorizationOfFactorial n = takeWhile ((0 <) . snd) $ map (\p -> (p,exponentInFactorial p n)) primes

modulus = 1000000009 :: Int

solutionFromFactorization :: [(Int,Int)] -> Int
solutionFromFactorization =
    foldr (\x y -> x*y `mod` modulus) 1 . map (\(p,k) -> 1 + powMod p (2*k) modulus)

main = print $ solutionFromFactorization (factorizationOfFactorial 100000000)