module Problem45 where

n_gonals :: Integer -> [Integer]
n_gonals n = tail $ scanl (+) 0 [1,n-1..]

trigonals :: [Integer]
trigonals = n_gonals 3

pentagonals :: [Integer]
pentagonals = n_gonals 5

hexagonals :: [Integer]
hexagonals = n_gonals 6

intersect :: [Integer] -> [Integer] -> [Integer]
intersect (x:xs) (y:ys) = case compare x y of
  LT -> intersect xs (y:ys)
  EQ -> x:intersect xs ys
  GT -> intersect (x:xs) ys
