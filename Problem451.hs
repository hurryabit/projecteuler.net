module Problem451 where

import Data.Array

import Numbers

factorizations :: Array Int [(Int,Int)]
factorizations = factorizationArray 1000 -- 20000000

idempotentsFF :: FF Int Int ([Int],Int)
idempotentsFF = FF
    { start   = ([0],1)
    , combine = cmb
    }
    where
        cmb p k (isn,n) =
            let pk = p^k
                ispk
                    | p == 2 && k == 1 = [1]
                    | p == 2 && k > 2  = let pk' = pk `div` 2 in [1,pk'-1,pk'+1,pk-1]
                    | otherwise        = [1,pk-1]
                n' = pk * n
                isn' = [ fst $ chinese2 (i1,pk) (i2,n) | i1 <- ispk, i2 <- isn ]
            in  (isn',n')

solution =
    let idemps = foldFactorizationArray idempotentsFF (2*10^7)
    in  sum $ map (maximum . init . fst) $ drop 2 $ elems idemps

main = print solution