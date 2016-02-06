import Data.List

r, r' :: Int -> Double
r 0 = 0
r n = 1 + (sum [ (k'-1) * r (k-1) + (n'-k') * r (n-k) | k <- [1 .. n], let k' = fromIntegral k ] ) / (n'*n')
  where n' = fromIntegral n

r' 0 = 0
r' n = (2*n'-1) / n'^2 + (n'^2-1)/n'^2*r (n-1)
  where n' = fromIntegral n

rs, as, ps :: [Double]
rs = 0 : zipWith (/) (tail as) [1..]
as = 0 : zipWith (\n a -> n + 2*a/n) [1..] ps
ps = scanl1 (+) as