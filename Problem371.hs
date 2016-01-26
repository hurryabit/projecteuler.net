module Problem371 where

import qualified Data.MemoCombinators as Memo

e :: Bool -> Int -> Double
e = Memo.memo2 Memo.bool (Memo.arrayRange (0,499)) $ \b n -> 
    let n' = fromIntegral n
    in  case (b,n) of
            (False,499) -> 2.004
            (True ,499) -> 2.0
            (False,_  ) -> (1000 + (998 - 2*n') * e False (n+1) + e True n) / (999 - n')
            (True ,_  ) -> (1000 + (998 - 2*n') * e True  (n+1)           ) / (999 - n')

solution = e False 0
