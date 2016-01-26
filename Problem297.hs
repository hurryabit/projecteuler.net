module Problem297 where

fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)

rep :: Integer -> [Integer]
rep n = run n . reverse . takeWhile (<= n) $ fibs
    where run :: Integer -> [Integer] -> [Integer]
          run 0 _ = []
          run m fs = let g:gs = dropWhile (> m) fs in g:run (m-g) gs

binomial :: Integer -> Integer -> Integer
binomial n k = foldl (\b (r,s) -> (b*r) `div` s) 1 $ zip [n,n-1..] [1..k]

zeckfn :: Integer -> Integer
zeckfn n = sum $ map (\k -> k * binomial (n-k) k) [1..n `div` 2]

zeck :: Integer -> Integer
zeck 0          = 0
zeck n
    | m == 0    = 1 + zeckfn (fromIntegral . length $ fs)
    | otherwise = m + zeck m + zeck f
     where fs = takeWhile (<= n) $ fibs
           f = last fs
           m = n - f