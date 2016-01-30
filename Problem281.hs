import Data.List
import Numbers

multinomial :: [Integer] -> Integer
multinomial = foldr (\(p,q) m -> m*p `div` q) 1 . zip [1 ..] . concatMap (\k -> [k,k-1 .. 1])

p, f :: Integer -> Integer -> Integer
p m n = (multinomial (genericReplicate m n) - sum [ m*d*p m d | d <- divisors n, d < n]) `div` (m*n)
f m n = sum $ map (p m) (divisors n)

solution = sum . concat $ takeWhile (not . null) [ takeWhile (<= 1000000000000000) (map (f m) [1..]) | m <- [2..] ]

-- specification

type MultiSet a = [(a,Int)]

variationsMS :: MultiSet a -> [[a]]
variationsMS [] = [[]]
variationsMS xns = [ y:ys | (y,yns) <- selectMS xns, ys <- variationsMS yns ]

selectMS :: MultiSet a -> [(a,MultiSet a)]
selectMS [] = []
selectMS ((x,n):xns) = (x,dropMS 1 ((x,n):xns)):[ (y,(x,n):yns)| (y,yns) <- selectMS xns]

dropMS :: Int -> MultiSet a -> MultiSet a
dropMS k [] = []
dropMS k ((x,n):xns) = case k `compare` n of
    LT -> (x,n-max 0 k):xns
    EQ -> xns
    GT -> dropMS (k-n) xns

rotations :: [a] -> [[a]]
rotations xs =
    let n = length xs
    in  take n . map (take n) . tails $ cycle xs

f_spec :: Int -> Int -> Int
f_spec m n =
    let xns = map (flip (,) n) (take m ['a' ..])
    in  length . map head . group . sort . map (minimum . rotations) $ variationsMS xns

isCyclic :: Eq a => Int -> [a] -> Bool
isCyclic n xs = and $ zipWith (==) xs (cycle $ take n xs)
