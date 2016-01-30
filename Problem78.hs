module Problem78 where

p :: Integer -> Integer
p n = q n n

q :: Integer -> Integer -> Integer
q n k
  | n < 0  = 0
  | n == 0 = 1
  | otherwise = sum [ q (n-l) l | l <- [1..min k n] ]

{-import qualified Data.MemoCombinators as Memo

ser = [ (n,k) | n <- [1..], let k = f n n, k `mod` 100000 == 0 ]
  where
  g :: Int -> Int
  g n = f n n
  f :: Int -> Int -> Int
  f = Memo.memo2 Memo.integral Memo.integral f'
  f' :: Int -> Int -> Int
  f' 0 0 = 1
  f' n 0 = 0
  f' n k
    | k > n     = f n n
    | otherwise = (f (n-k) k + f n (k-1)) `mod` 1000000

diags :: [[a]] -> [[a]]
diags = diags2 (repeat [])

diags2 :: [[a]] -> [[a]] -> [[a]]
diags2 (ys:yss) ((x:xs):xss) = (x:ys):diags2 (zipWith (:) xs yss) xss

add :: Int -> Int -> Int
add x y = (x+y) --`mod` 10000

piles :: [[Int]]
piles = (repeat 1):map (scanl1 add . (++repeat 0)) (diags piles)

trace :: [[a]] -> [a]
trace ((x:_):xss) = x:trace (map tail xss)

numbers = {-filter ((0==) . snd) . zip [1..] .-} trace . tail $ piles-}
