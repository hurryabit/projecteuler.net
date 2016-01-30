{-# LANGUAGE TupleSections #-}
module Problem298 where

--import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.MemoCombinators (Memo)
import qualified Data.MemoCombinators as Memo

memSize :: Int
memSize = 5

maxDigit :: Int
maxDigit = 10

probDigit :: Double
probDigit = 1 / fromIntegral maxDigit

type Larry = (Int,[Int])

larry :: Larry -> Int -> (Larry,Bool)
larry (sz,mem) n = case break (n'==) mem' of
                     (_,[])    -> (clean (if sz == memSize then (memSize,n':init mem') else (sz+1,n':mem')),False)
                     (xs,_:ys) -> ((sz,n':xs++ys),True)
    where (mem',n') = if n <= sz then (mem,n) else (map (rename n) mem,1)
          rename n k = case k `compare` n of
                         LT -> k+1
                         EQ -> 1
                         GT -> k

clean :: Larry -> Larry
clean (sz,mem) = (sz,snd $ mapAccumL (\d k -> if k > sz then (d+1,d) else (d,k)) (sz+1) mem)

type LarryX = Int

encode :: Larry -> LarryX
encode (0,[])   = 0
encode (sz,mem) = foldr (\d r -> 16*r+d) 0 (maximum mem:sz:mem)

decode :: LarryX -> Larry
decode 0 = (0,[])
decode x = let f 0 = Nothing
               f n = let (q,r) = n `divMod` 16 in Just (r,q)
               _:sz:mem = unfoldr f x
           in  (sz,mem)
    
    
larryX :: LarryX -> Int -> (LarryX,Bool)
larryX = Memo.memo2 Memo.integral (Memo.arrayRange (0,memSize)) larryX'
    where larryX' x n = let (mem,pnt) = larry (decode x) n
                        in  (encode mem,pnt)

larryXMemo :: Memo LarryX
larryXMemo = Memo.integral

type Distribution = IntMap Double

recurse :: Int -> LarryX -> Distribution
recurse = Memo.memo2 Memo.integral larryXMemo recurse'
    where recurse' :: Int -> LarryX -> Distribution
          recurse' 0 _    = IntMap.singleton 0 1
          recurse' r lmem = IntMap.unionsWith (+) $ do
              let m = lmem `mod` 16
              (n,pr_n) <- (if m < maxDigit then ((m+1,fromIntegral (maxDigit-m) * probDigit):) else id) $ zip [1..m] (repeat probDigit)
              let (lmem',lpnt) = larryX lmem n
                  rpnt = n <= ((lmem `div` 16) `mod` 16)--fst lmem
                  dist' = recurse (r-1) lmem'
                  off = fromEnum lpnt - fromEnum rpnt
              return $ IntMap.map (pr_n*) $ if off == 0 then dist' else IntMap.mapKeysMonotonic (+off) dist'

main = print $ IntMap.foldrWithKey' (\k p e -> e+fromIntegral (abs k)*p) 0 $ recurse 50 (encode (0,[]))

points :: (mem -> Int -> (mem,Bool)) -> mem -> [Int] -> Int
points player empty = length . filter id . snd . mapAccumL player empty