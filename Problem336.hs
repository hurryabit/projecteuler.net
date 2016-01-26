module Problem336 where

solution = filter (isMaximix 11) (map ("CA" ++) . permutations $ 'B':['D'..'K']) !! 2010

isMaximix :: Int -> String -> Bool
isMaximix n xs = rotations 'A' xs == 2*n-3

maximix :: Int -> [String]
maximix n = filter (isMaximix n) (permutations (take n ['A'..]))

rotations :: Char -> String -> Int
rotations x [] = 0
rotations x xs = case break (==x) xs of
                   ([],zs)   -> rotations (succ x) (tail xs)
                   (ys,[_])  -> 1 + rotations (succ x) (reverse ys)
                   (ys,_:zs) -> 2 + rotations (succ x) (zs ++ reverse ys)

select :: [a] -> [(a,[a])]
select [x]    = [(x,[])]
select (x:xs) = (x,xs):map (\(y,ys) -> (y,x:ys)) (select xs)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
    (y,ys) <- select xs
    zs <- permutations ys
    return $ y:zs
