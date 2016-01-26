module Problem73 where

numeratorsFor :: Int -> [Int]
numeratorsFor n = filter ((1==) . gcd n) [(n+2) `div` 3..(n-1) `div` 2]

count :: Int -> Int
count n = length . concatMap numeratorsFor $ [5..n]
