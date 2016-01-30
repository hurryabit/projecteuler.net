module Problem109 where

import Control.Applicative
import Control.Monad
import Data.List

sol= let x=0:25:50:liftM2 (*) [1..3] [1..20] in length . filter (<100) .
  liftM2 (+) (50:[2,4..40]) . concat . zipWith (map . (+)) x . tails $ x
