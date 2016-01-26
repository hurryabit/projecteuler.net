module Problem250 where

import Data.Array
import Data.List
import Data.MemoCombinators

setsize = 250250
modulus = 250
cut = (`mod` (10^16))

powMod :: Integer -> Integer -> Integer -> Integer
powMod m x 0 = 1
powMod m x e = if r == 0 then (y*y) `mod` m else (x*y*y) `mod` m
  where (q,r) = e `divMod` 2
        y = powMod m x q

histogram :: Array Integer Integer
histogram = accumArray (+) 0 (0,modulus-1) [ (powMod modulus n n,1) | n <- [1 .. setsize] ]

choicesMod :: Integer -> [Integer]
choicesMod = bits choicesMod'

choicesMod' n = map cut $ scanl (\p (i,j) -> (p*i) `div` j) 1 $ zip [n',n'-1 ..] [1 .. n']
    where n' = toInteger n

solve :: Integer -> Integer -> Integer
solve = memo2 (unsafeArrayRange (0,modulus-1)) (unsafeArrayRange (0,modulus)) solve'

solve' :: Integer -> Integer -> Integer
solve' acc r
    | r < modulus = foldr (\x y -> cut $ x+y) 0 $
                        let h = histogram ! r
                            f i hCi = cut $ hCi * solve ((acc+i*r) `mod` modulus) (r+1)
                        in   zipWith f [0 ..] (choicesMod h)
    | acc == 0    = 1
    | otherwise   = 0

--main = print $ nub $ elems histogram
main = print $ solve 0 0 - 1

subsets = foldr (\x yss  -> yss ++ map (x:) yss) [[]]

solveSpec = cut $ genericLength $ filter (\xss -> sum xss `mod` modulus == 0) $ subsets [ n^n | n <- [1..setsize] ]