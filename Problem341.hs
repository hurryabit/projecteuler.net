module Problem341 where

import Data.List
import Data.Maybe

gs :: [Integer]
gs = 1:2:2:concat (zipWith genericReplicate (drop 2 gs) [3 ..])

hs :: [Integer]
hs = scanl1 (+) gs

ks :: [Integer]
ks = scanl1 (+) $ zipWith3 (\i h' h -> i*(h-h')) [1..] (0:hs) hs

ls :: [Integer]
ls = scanl1 (+) $ zipWith3 (\i h' h -> i*(h*(h+1)-h'*(h'+1)) `div` 2) [1..] (0:hs) hs

ihkls = zip4 [1..] hs ks ls

g :: Integer -> Integer
g n = g_aux n $ fromJust $ find (\(_,_,_,l) -> n <= l) ihkls

g_aux n (i,h,k,l) =
    let h' = ceiling $ -0.5 + sqrt (0.25 - 2*(fromInteger l-fromInteger n)/fromInteger i + fromInteger h*(fromInteger h+1))
        k' = k - i*(h-h')
        l' = l - i*(h*(h+1)-h'*(h'+1)) `div` 2
    in  k' - (l'-n) `div` h'

firsts :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
firsts _ []     _  = []
firsts p (x:xs) ys =
    let ys'@(y:_) = dropWhile (not . p x) ys
    in  (x,y):firsts p xs ys'

g_sum ns = sum $ map (uncurry g_aux) $ firsts (\n (_,_,_,l) -> n <= l) ns ihkls

main = print $ g_sum [ n^3 | n <- [1..999999] ]
