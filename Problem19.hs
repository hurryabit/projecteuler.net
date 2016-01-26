module Problem19 where

import Control.Applicative
import Data.Time.Calendar

refDay = fromGregorian 1900 1 1

isSunday d = diffDays d refDay `mod` 7 == 6

days = (\y d -> fromGregorian y d 1) <$> [1901..2000] <*> [1..12]
