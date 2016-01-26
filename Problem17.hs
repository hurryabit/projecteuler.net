module Problem17 where

import Control.Monad
import Data.Array
import Data.Char

digits = ["one","two","three","four","five","six","seven","eight","nine"]

names1_99 = digits ++ names10_19 ++
  liftM2 (++) names20_90 ("":map ('-':) digits)

names10_19 =
  ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen"
  ,"seventeen","eighteen","nineteen"]

names20_90 =
  ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

names1_999 = names1_99 ++
  liftM2 (\x y -> x ++ " hundred" ++ y) digits ("":map (" and " ++) names1_99)

names1_1000 = names1_999 ++ ["one thousand"]

size = length . filter isLower . concat
