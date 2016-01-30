module Problem24 where

choices :: [a] -> [(a,[a])]
choices []     = []
choices (x:xs) = (x,xs):[ (y,x:ys) | (y,ys) <- choices xs ]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = concatMap (\(y,ys) -> map (y:) (permutations ys)) (choices xs)
