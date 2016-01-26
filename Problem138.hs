module Problem138 where

triangles :: [(Integer,Integer,Integer)]
triangles = mergeBy (\(_,b1,h1) (_,b2,h2) -> (b1*h1) `compare` (b2*h2)) (map tr1 pell) (map tr2 pell)
    where (x1,y1) = (9,4)
          pell = iterate (\(xk,yk) -> (x1*xk+5*y1*yk,x1*yk+y1*xk)) (x1,y1)
          tr1 (x,y) = let v = y
                          u = x-2*y
                      in  (u*u+v*v,4*u*v,v*v-u*u)
          tr2 (x,y) = let u = y
                          v = x+2*y
                      in  (u*u+v*v,4*u*v,v*v-u*u)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp []     ys     = ys
mergeBy cmp xs     []     = xs
mergeBy cmp (x:xs) (y:ys) = case x `cmp` y of
    LT -> x:mergeBy cmp xs (y:ys)
    EQ -> x:y:mergeBy cmp xs ys
    GT -> y:mergeBy cmp (x:xs) ys

solution :: Integer
solution = sum . map (\(l,_,_) -> l) . take 12 $ triangles