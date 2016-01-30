module Problem166 where

import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.Function
import Data.IORef
import Data.List
import Data.List.Split

type Field = (Int,Int)

data Action = Guess Field | Check Field
  deriving (Show)

strategy :: [Action]
strategy = map (Guess . (,) 0) [0..3] ++ 
  [Guess (1,0),Guess (2,0),Check (3,0),Guess (1,2),Check (2,1),Guess (1,1)
  ,Check (1,3),Check (3,1),Guess (2,2),Check (2,3),Check (3,2),Check (3,3)]

check :: UArray Field Int -> Field -> Maybe Int
check arr field
  | field == (3,3) && (digit /= digit2 || digit /= digit3)  = Nothing
  | 0 <= digit && digit <= 9                                = Just digit
  | otherwise                                               = Nothing
  where known = case field of
                  (3,c) -> [(0,c),(1,c),(2,c)]
                  (2,1) -> [(0,3),(1,2),(3,0)]
                  (r,3) -> [(r,0),(r,1),(r,2)]
        fieldSum = sum . map (arr !)
        total = fieldSum $ map ((,) 0) [0..3]
        digit = total - fieldSum known
        digit2 = total - fieldSum [(0,0),(1,1),(2,2)]
        digit3 = total - fieldSum [(3,0),(3,1),(3,2)]

run :: IORef Int-> [Action] -> IOUArray Field Int -> IO ()
run cnt [] arr = do
  modifyIORef cnt (+1)
  c <- readIORef cnt
  when (c `mod` 1000 == 0) $ print c
  --getElems arr >>= putStrLn . unlines . chunk 4 . concatMap show
run cnt (Guess f:acts) arr = forM_ [0..9] $ \d -> do
  writeArray arr f d
  run cnt acts arr
run cnt (Check f:acts) arr = do
  arr' <- unsafeFreeze arr
  case check arr' f of
    Just d -> writeArray arr f d >> run cnt acts arr
    Nothing -> return ()

main = do
  arr <- newArray ((0,0),(3,3)) 0
  cnt <- newIORef 0
  run cnt strategy arr
  putStrLn $ replicate 10 '='
  readIORef cnt >>= print


{-rows :: Int -> Int -> [[Int]]
rows 0 0 = [[]]
rows 0 _ = []
rows l s = [ d:ds | d <- [0..9], ds <- rows (l-1) (s-d) ]

--squaresBySum :: Int -> [[[Int]]]
squaresBySum s = do
  r <- rs
  sq <- foldr (\c -> liftM2 (:) (grs ! c)) [[]] r
  let sqT = transpose sq
      d1 = zipWith (!!) sq [0..3]
      d2 = zipWith (!!) sq [3,2..0]
  guard $ all (s==) . map sum $ d1:d2:sqT
  return sq
  where
    rs = rows 4 s
    grs :: Array Int [[Int]]
    grs = listArray (0,9) . snd $
            mapAccumL (\xss n -> uncurry (flip (,)) $ span ((n==) . head) xss)
            rs [0..9]-}
