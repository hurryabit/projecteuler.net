module Problem232 where

import qualified Data.MemoCombinators as Memo

pr :: Int -> Int -> Bool -> Double
pr = Memo.memo3 (Memo.arrayRange (0,100)) (Memo.arrayRange (0,100)) Memo.bool pr'
    where pr' m   n   p
              | n > 100     = pr m 100 p
          pr' 100 100 _     = error "100 & 100 should not happen!"
          pr' 100 n   _     = 0
          pr' m   100 _     = 1
          pr' m   n   True  = (pr m n False + pr (m+1) n False) / 2
          pr' m   n   False = maximum [ ((0.5-0.5^(t+1))*pr (m+1) n False + 0.5^t*pr m (n+2^(t-1)) True) / (0.5+0.5^(t+1)) | t <- 1:takeWhile (\t' -> n+2^(t'-2) < 100) [2..] ]

solution = pr 0 0 True