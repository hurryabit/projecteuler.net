module Problem199 where

import Data.Tree

r0 :: Double
r0 = 2 * sqrt 3 - 3

root :: Tree Double
root = Node { rootLabel = 3*r0*r0, subForest = node (r0,r0,r0) : replicate 3 (node (-1,r0,r0)) }

-- it's about Soddy circles
node :: (Double,Double,Double) -> Tree Double
node (r1,r2,r3) =
    let r4 = r1*r2*r3 / (r1*r2 + r1*r3 + r2*r3 + 2*signum r1*sqrt (r1*r2*r3*(r1+r2+r3)))
    in  Node { rootLabel = r4*r4, subForest = map node [(r1,r2,r4),(r1,r3,r4),(r2,r3,r4)] }

solution n = 1 - (sum $ concat $ take (n+1) $ levels root)

main = print $ solution 10
