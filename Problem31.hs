module Problem31 where

coins :: [Int]
coins = reverse [1,2,5,10,20,50,100,200]

possibleSums :: [Int] -> Int -> Int
possibleSums [] 0 = 1
possibleSums [] _ = 0
possibleSums (c:cs) n = sum [ possibleSums cs (n-k*c) | k <- [0..n `div` c] ]
