module Problem437 where
    
import Data.Numbers.Primes

powMod :: Int -> Int -> Int -> Int
powMod m x k = run k x 1
    where
    run 0 x y = y
    run k x y = let (q,r) = k `divMod` 2
                in  run q ((x*x) `mod` m) (if r == 0 then y else (x*y) `mod` m)