import Control.Monad
import Data.Array
import Data.List
import Data.Tuple

type Digit = Int
type DigitTable = [Int]

select :: DigitTable -> [(Digit,DigitTable)]
select = aux . zip [0..]
  where
    aux [] = []
    aux ((d,n):dns)
      | n > 0     = (d,(n-1):map snd dns):delay
      | otherwise = delay
      where
        delay = map (fmap (n:)) (aux dns) -- fmap f (a,b) = (a,f b)

pack :: Int -> DigitTable -> Int
pack b = foldr (\d n -> b*n+d) 0

unpack :: Int -> Int -> DigitTable
unpack b = unfoldr aux
  where
    aux n = guard (n > 0) >> return (swap $ n `divMod` b)

solution :: Int
solution =
  let bnds  = ((0,0),(3^10-1,10))
      table = listArray bnds $ 1:replicate 10 0 ++ map sol (drop 11 $ range bnds)
      sol (ns,r) = sum [ table ! (pack 3 ns',(d-r) `mod` 11) | (d,ns') <- select (unpack 3 ns) ]
  in  table ! (pack 3 (replicate 10 2),0) - table ! (pack 3 (1:replicate 9 2),0)

main = print solution
