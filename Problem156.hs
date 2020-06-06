import Data.List
import Data.Maybe
import Data.Ratio
import Numbers

main = print $ sum $ map funcS [1 .. 9]

funcS = sum . solutions

solutions :: Int -> [Int]
solutions d =
  let ncs = takeWhile (\(n,c) -> n < 10^11) $ iterate (next d) (0,0)
  in  [ n | (n,c) <- ncs, n == c]

next :: Int -> (Int,Int) -> (Int,Int)
next d (n,c) =
  let opts = map (fmap (+c)) $ options d n
      pred (n',c') = (n < c) && (n' <= c) || (n >= c) && (n+1 >= c')
  in  fromMaybe (last opts) $ find pred opts

-- options digit number
options :: Int -> Int -> [(Int,Int)]
options d n = reverse $ map (\(n',dc) -> (n'-1,numerator dc)) $ options' d 0 (n+1)

-- options digit pos number
options' :: Int -> Int -> Int -> [(Int,Ratio Int)]
options' d p n =
  let (n',r) = n `quotRem` 10
      hd = (n+1,elemCount d (digits n) % 1 + p % 10)
      tl
        | r > 0     = []
        | otherwise = [ (10*m,10*dc)| (m,dc) <- options' d (p+1) n' ]
  in  hd:tl

elemCount :: Eq a => a -> [a] -> Int
elemCount x = length . filter (x ==)
