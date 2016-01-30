import Data.Array

type Table = Array (Int,Int) Int -- index: (digit sum, remainded mod 23)

initial :: Table
initial = accumArray (+) 0 ((0,0),(23,22)) [ ((d,d),1) | d <- [0 .. 9] ]

combine :: Int -> Table -> Table -> Table
combine m tab1 tab0 = accumArray (\x y -> (x+y) `mod` 1000000000) 0 ((0,0),(23,22))
    [ ((d,r),c)
    | ((d1,r1),c1) <- assocs tab1
    , ((d0,r0),c0) <- assocs tab0
    , let d = d1+d0
          r = (m*r1+r0) `mod` 23
          c = c1*c0 `mod` 1000000000
    , d <= 23
    ]

table :: Int -> Table
table 1 = initial
table n
    | r == 0    = tab2
    | otherwise = combine 10 tab2 initial
    where
        (n',r) = n `divMod` 2
        tab1   = table n'
        tab2   = combine (powerMod 10 (n' `mod` 22) 23) tab1 tab1

-- powerMod b k m = b^k `mod` m
powerMod :: Int -> Int -> Int -> Int
powerMod b 0 m = 1
powerMod b k m
    | r == 0    = p*p `mod` m
    | otherwise = b*p*p `mod` m
    where
        (k',r) = k `divMod` 2
        p = powerMod b k' m

main = print $ table (11^12) ! (23,0)