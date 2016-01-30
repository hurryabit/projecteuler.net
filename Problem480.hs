import Data.Array
import Data.List
import Data.MemoCombinators


type Histogram = [(Char,Int)]

histogram :: String -> Histogram
histogram = map ((,) <$> head <*> length) . group . sort

select :: Histogram -> [(Char,Histogram)]
select [] = []
select ((c,n):cns)
  | n > 1     = (c,(c,n-1):cns) : delay
  | otherwise = (c,cns) : delay
  where
    delay = map (fmap ((c,n):)) (select cns) -- fmap = second


type Config = [Int]

config :: Histogram -> Config
config = map length . fillGaps . group . sort . map snd
  where
    fillGaps :: [[Int]] -> [[Int]]
    fillGaps = concat . snd . mapAccumL (\k xs@(x:_) -> (x+1,replicate (x-k) [] ++ [xs])) 1

decrements :: Config -> [(Config,Int)]
decrements cfg =
  let upds  = iterate (id:) (succ:pred:repeat id)
  in  [ (tail (zipWith ($) upd (0:cfg)),n) | (n,upd) <- zip cfg upds, n > 0 ]

size :: Config -> Int
size = sum . zipWith (*) [1 ..]


converters :: String -> (String -> Integer, Integer -> String)
converters phrase = (word2index hst0 conts, index2word hst0 conts)
  where
    hst0 :: Histogram
    hst0 = histogram phrase
    conts :: Histogram -> Integer
    conts = continuations hst0

continuations :: Histogram -> Histogram -> Integer
continuations hst0 = conts . config
  where
    base :: [Int]
    base = map (+1) $ scanr1 (+) (config hst0)
    encode :: Config -> Int
    encode = foldr (\(b,d) n -> b*n+d) 0 . zip base
    decode :: Int -> Config
    decode code = snd $ mapAccumL (\n b -> let (n',d) = n `divMod` b in (n',d)) code base
    maxCode :: Int
    maxCode = product base - 1

    conts, conts' :: Config -> Integer
    conts = wrap decode encode (unsafeArrayRange (0,maxCode - 1)) conts'
    conts' cfg
      | size cfg == 33 = 1
      | otherwise      = 1 + sum [ toInteger k * conts cfg' | (cfg',k) <- decrements cfg ]

word2index :: Histogram -> (Histogram -> Integer) -> String -> Integer
word2index hst0 conts = sum . snd . mapAccumL combine hst0
  where
    combine hst c
      | c /= c'   = error "string does not match histogram"
      | otherwise = (hst',1 + sum (map (conts . snd) skp))
      where (skp,(c',hst'):_) = span ((c >) . fst) (select hst)

index2word :: Histogram -> (Histogram -> Integer) -> Integer -> String
index2word hst0 conts = unfoldr uncombine . (,) hst0
  where
    uncombine :: (Histogram,Integer) -> Maybe (Char,(Histogram,Integer))
    uncombine (_  ,0  ) = Nothing
    uncombine (hst,idx) =
      let sels  = select hst
          psums = scanl (+) 0 (map (conts . snd) sels)
          (psum,(c,hst')) = last $ takeWhile ((idx >) . fst) $ zip psums sels
      in  Just (c,(hst',idx-psum-1))

main = do
  let (w2i, i2w) = converters "thereisasyetinsufficientdataforameaningfulanswer"
      solution = i2w $
        w2i "legionary"
          + w2i "calorimeters"
          - w2i "annihilate"
          + w2i "orchestrated"
          - w2i "fluttering"
  print solution
