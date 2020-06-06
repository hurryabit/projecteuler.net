import Data.List
import Data.Numbers.Primes

-- k_ p i is the maximal k such that p^k divides i!
k_ :: Int -> Int -> Int
k_ p = sum . takeWhile (0 <) . tail . iterate (`div` p)

-- j_ p k is the minimal j such that p^k divides j!
j_ :: Int -> Int -> Int
j_ p k =
  let ls = takeWhile (k >=) $ iterate ((1+) . (p*)) 1
      combine k' l =
        let (q,r) = k' `quotRem` l
        in  (r,q*(l*(p-1)+1))
  in  sum . snd $ mapAccumR combine k ls

-- [N(0), N(1), N(2), ...], m shall capture the number 1234567890
valuesN :: Int -> [Int]
valuesN m =
  let combine n i =
        let ps = map head . group $ primeFactors i
        in  maximum (n:[ j_ p (m * k_ p i) | p <- ps ])
  in  scanl combine 0 [1 ..]

solution :: Int
solution = 
  let sumMod m = foldl (\x y -> (x+y) `mod` m) 0
  in  sumMod 1000000000000000000 $ drop 10 $ take 1000001 $ valuesN 1234567890

main = print solution
