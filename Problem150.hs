module Problem150 where

import Data.List

randT, randS :: [Integer]
randT = tail $ iterate (\t -> (615949*t + 797807) `mod` 1048576) 0
randS = map (flip (-) 524288) randT

triangulate :: [a] -> [[a]]
triangulate xs = snd $ mapAccumL f xs [1..]
    where f xs' n = let (ys,xs'') = splitAt n xs' in (xs'',ys)

triangleS :: [[Integer]]
triangleS = take 1000 $ triangulate randS

triangleEx :: [[Integer]]
triangleEx = [[15],[-14,-7],[20,-13,-5],[-3,8,23,-26],[1,-4,-5,-18,5],[-16,31,2,9,28,3]]

minimize :: [[Integer]] -> Integer
minimize = minimum . map (minimum . concat) . evolution

evolution :: [[Integer]] -> [[[Integer]]]
evolution tr1 = let tr0 = iterate (0:) [0]
                    evo = tr0:tr1:zipWith (evolve tr1) (tail evo) evo
                in  takeWhile (not . null) (tail evo)

evolve :: [[Integer]] -> [[Integer]] -> [[Integer]] -> [[Integer]]
evolve tr1 trk trk'   = zipWith3 f tr1 (drop 1 trk) (drop 2 trk')
    where f l1 lk lk' = zipWith4 g l1 lk (drop 1 lk) (drop 1 lk')
          g a b c d   = a+b+c-d

main = print . minimize $ triangleS