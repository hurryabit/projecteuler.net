module Problem247b where

data Tree = Branch Tree Double Tree

retrieve h v (Branch l s r)
    | h == 0 && v == 0 = [s]
    | h >= 0 && v >= 0 = retrieve (h-1) v l ++ retrieve h (v-1) r
    | otherwise        = []

tree x y = let s = (sqrt ((x+y)^2 - 4*(x*y-1)) -(x+y)) / 2
           in  Branch (tree (x+s) y) s (tree x (y+s))

count (Branch l s r) t
    | s >= t    = let c = 1 + count l t + count r t in c -- `seq` c
    | otherwise = 0

solve h v = let t = tree 1.0 0.0
            in  count t (minimum $ retrieve h v t)

main = print $ solve 3 3