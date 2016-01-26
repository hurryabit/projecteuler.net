module Problem359 where

p :: Integer -> Integer -> Integer
p f r
    | f == 1           = r * (r+1) `div` 2
    | even f && odd  r = 2 * (f'+r')^2 + r'
    | even f && even r = 2 * (f'+r'+1)^2 - r' - 1
    | odd  f && odd  r = 2 * (f'+r')^2 + 2 * (f'+r') - r'
    | odd  f && even r = 2 * (f'+r')^2 + 2 * (f'+r') + r' + 1
    where
        f' = f `div` 2
        r' = (r-1) `div` 2

solution = sum $ do
    f2 <- take 28 $ iterate (*2) 1
    f3 <- take 13 $ iterate (*3) 1
    let f = f2*f3
        r = 71328803586048 `div` f
    return $ p f r