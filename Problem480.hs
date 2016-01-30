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

numContinuations :: Histogram -> Histogram -> Integer
numContinuations hst0 = numConts . metaHistogram
  where
    metaHistogram :: Histogram -> [Int]
    metaHistogram = map length . fillGaps . group . sort . map snd
      where
        fillGaps = concat . snd . mapAccumL (\k xs@(x:_) -> (x+1,replicate (x-k) [] ++ [xs])) 1

    numConts, numConts' :: [Int] -> Integer
    numConts = wrap decode encode (unsafeArrayRange (0,product base - 1)) numConts'
      where
        base = map (+1) $ scanr1 (+) $ metaHistogram hst0
        encode = foldr (\(b,d) n -> b*n+d) 0 . zip base
        decode code = snd $ mapAccumL (\n b -> let (n',d) = n `divMod` b in (n',d)) code base
    numConts' cfg
      | size == 33 = 1
      | otherwise  = 1 + sum [ numConts cfg'  * toInteger n | (cfg',n) <- cfg'ns ]
      where
        size   = sum $ zipWith (*) [1 ..] cfg
        upds   = iterate (id:) (succ:pred:repeat id)
        cfg'ns = [ (tail (zipWith ($) upd (0:cfg)),n) | (n,upd) <- zip cfg upds, n > 0 ]

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
    uncombine (_  ,0  ) = Nothing
    uncombine (hst,idx) =
      let sels  = select hst
          psums = scanl (+) 0 (map (conts . snd) sels)
          (psum,(c,hst')) = last $ takeWhile ((idx >) . fst) $ zip psums sels
      in  Just (c,(hst',idx-psum-1))

main = do
  let histo = histogram "thereisasyetinsufficientdataforameaningfulanswer"
      conts = numContinuations histo
      w2i   = word2index histo conts
      i2w   = index2word histo conts
  print $ i2w (w2i "legionary" + w2i "calorimeters" - w2i "annihilate" + w2i "orchestrated" - w2i "fluttering")
