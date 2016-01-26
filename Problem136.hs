module Problem136 where

import Numbers

limit :: Int
limit = 50000000

main = do
    let ar = isPrimeArray limit
        ps = primesFrom ar
        n1 = length . takeWhile (< limit) . filter (\p -> p `mod` 4 ==3) $ ps
        n2 = length . takeWhile (\p -> 4*p < limit) $ ps
        n3 = length . takeWhile (\p -> 16*p < limit) $ ps
        -- 8 and 16 are bad solutions, but 4 and 16 are missing
    print $ n1+n2+n3
    