module Problem86 where

isSquare :: Int -> Bool
isSquare n = let k = floor . sqrt . fromIntegral $ n in n == k*k

countFor c = sum [min k (2*(c+1)-k) `div` 2 | k <- [2..2*c], isSquare $ k*k+c*c]

solution = fst . head . dropWhile ((<1000000) . snd) . zip [0..] . scanl (+) 0 . map countFor $ [1..]

main = print solution
