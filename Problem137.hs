module Problem137 where


pell5pos, pell5neg :: [(Integer,Integer)]
pell5pos = iterate (\(xk,yk) -> (9*xk+20*yk,4*xk+9*yk)) (9,4)
pell5neg = iterate (\(xk,yk) -> (9*xk+20*yk,4*xk+9*yk)) (2,1)

infixl 0 `merge`

merge :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) = case x `compare` y of
    LT -> x:merge xs (y:ys)
    EQ -> x:y:merge xs ys
    GT -> y:merge (x:xs) ys

goldenNuggets :: [Integer]
goldenNuggets = map (\(x,y) -> 2*(x-y)*y) pell5neg `merge` map (\(x,y) -> 2*(x+y)*y) pell5pos `merge` map (\(x,y) -> 4*(x-2*y)*y-1) pell5pos

solution = goldenNuggets !! 14