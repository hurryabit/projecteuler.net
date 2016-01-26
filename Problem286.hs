module Problem286 where

prob q = last $ foldl f (1.0:replicate 20 0.0) [1.0 .. 50.0]
    where
    f ps x = zipWith (+) (map ((x/q)*) ps) (map ((1-x/q)*) (0.0:ps))

binsearch l r
    | r - l < 1e-12 = m
    | otherwise    = if prob m > 0.02 then binsearch m r else binsearch l m
    where m = (l+r) / 2

solve = binsearch 52 53