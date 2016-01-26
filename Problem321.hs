module Problem321 where

import Data.List

isTriangle :: Integer -> Bool
isTriangle n =
    let m = floor $ (1 + sqrt (1 + 8*fromIntegral n)) / 2
    in  2*n == m*(m-1)

camels :: [Integer]
camels = map (\n -> (n+1)^2 - 1) [1..]

-- http://oeis.org/A006454
triangleCamels :: [Integer]
triangleCamels =
    let tcs = (take 6 $ filter isTriangle camels) ++ (map (\[x,_,y,_,z,_] -> 35*(z-y)+x) $ transpose $ take 6 $ tails tcs)
    in  tcs

solution = take 40 $ map (pred . floor . sqrt . fromIntegral . succ) triangleCamels