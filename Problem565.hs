import Data.Maybe
import Numbers

order :: Integer -> Integer -> Integer
order x m = fromJust $ lookup 1 $ zip (iterate ((`mod` m) . (* x)) x) [1..]
