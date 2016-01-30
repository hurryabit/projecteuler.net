import Control.Monad
import Data.Array
import Data.Array.ST

import Numbers

type Index = (Int,Int) -- overflow, digit sum difference

initialTable :: Array Index Int
initialTable = listArray ((0,0),(136,0)) (repeat 0) // [((0,0),1)]

addDigitToTable :: Array Index Int -> Array Index Int
addDigitToTable table =
    let ((low1,low2),(up1,up2)) = bounds table
    in  accumArray (+) 0 ((low1,low2-9),(up1,up2+9))
            [ ((overflow',diff + digit - digit137),count)
            | ((overflow,diff),count) <- assocs table
            , digit <- [0 .. 9]
            , let (overflow',digit137) = (137*digit + overflow) `divMod` 10
            ]

resultsFromTable :: Array Index Int -> Int
resultsFromTable table = sum
    [ count
    | ((overflow,diff),count) <- assocs table
    , diff == digitSum overflow
    ]

main = print $ resultsFromTable $ iterate addDigitToTable initialTable !! 18
