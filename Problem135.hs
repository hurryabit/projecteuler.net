module Problem135 where

import Numbers (isPrimeBy)

primes :: [Int]
primes = sieve [2..]
    where sieve (p:qs) = p:sieve (filter (\q -> q `mod` p /= 0) qs)

factorize :: Int -> [(Int,Int)]
factorize = run primes
    where run _ 1 = []
          run (p:ps) n =
              let mp m = case m `divMod` p of
                           (q,0) -> let (e,s) = mp q in (e+1,s)
                           _     -> (0,m)
              in  case mp n of
                    (0,_) -> run ps n
                    (e,m) -> (p,e):run ps m

divisors :: Int -> [Int]
divisors = run . factorize
    where run [] = [1]
          run ((p,e):fs) = [ k*l | k <- take (e+1) (iterate (*p) 1), l <- run fs ]

solutions :: Int -> [Int]
solutions n = filter p (divisors n)
    where p a = case (n `div` a + a) `divMod` 4 of
                  (q,0) -> q < a
                  _     -> False

hasLength :: Int -> [a] -> Bool
hasLength 0 xs = null xs
hasLength k [] = False
hasLength k (x:xs) = hasLength (k-1) xs

numbers :: [Int]
numbers = 3:(filter (hasLength 1 . solutions) $ concat [ [4*k,4*k+3] | k <- [1..249999] ])

isSol :: Int -> Bool
isSol n = or
    [ r4 == 3 && isPrimeBy primes n
    , r4 == 0 && q4 /= 2 && (q4 == 1 || isPrimeBy primes q4)
    , r16 == 0 && q16 /= 2 && (q16 == 1 || isPrimeBy primes q16) ]
    where (q4,r4) = n `divMod` 4
          (q16,r16) = n `divMod` 16

numbers2 :: [Int]
numbers2 = filter isSol [1..]

main = do
    mapM_ print numbers
    putStrLn "======"
    print $ length numbers