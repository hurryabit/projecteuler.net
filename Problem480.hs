import Data.Array
import Data.List
import Data.MemoCombinators

phrase = "thereisasyetinsufficientdataforameaningfulanswer"

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

initial :: Config
initial = config (histogram phrase)

decrements :: Config -> [(Config,Int)]
decrements cfg =
  let upds  = iterate (id:) (succ:pred:repeat id)
  in  [ (tail (zipWith ($) upd (0:cfg)),n) | (n,upd) <- zip cfg upds, n > 0 ]

size :: Config -> Int
size = sum . zipWith (*) [1 ..]

type Base = [Int]
type Code = Int

base :: Base
base = map (+1) $ scanr1 (+) initial 

encode :: Base -> Config -> Code
encode bs = foldr (\(b,d) n -> b*n+d) 0 . zip bs

decode :: Base -> Code -> Config
decode bs code = snd $ mapAccumL (\n b -> let (n',d) = n `divMod` b in (n',d)) code bs

maxCode :: Base -> Code
maxCode bs = product bs - 1

continuations :: Config -> Integer
continuations = wrap (decode base) (encode base) (unsafeArrayRange (0,maxCode base - 1)) continuations'

continuations' :: Config -> Integer
continuations' cfg
  | size cfg == 33 = 1
  | otherwise      = 1 + sum [ toInteger k * continuations cfg' | (cfg',k) <- decrements cfg ]

word2index :: String -> Integer
word2index = sum . snd . mapAccumL combine (histogram phrase)
  where
    combine hst c
      | c /= c'   = error "string does not match histogram"
      | otherwise = (hst',1 + sum (map (continuations . config . snd) skp))
      where (skp,(c',hst'):_) = span ((c >) . fst) (select hst)

index2word :: Integer -> String
index2word = unfoldr uncombine . (,) (histogram phrase)

uncombine :: (Histogram,Integer) -> Maybe (Char,(Histogram,Integer))
uncombine (_  ,0  ) = Nothing
uncombine (hst,idx) =
  let sels  = select hst
      psums = scanl (+) 0 (map (continuations . config . snd) sels)
      (psum,(c,hst')) = last $ takeWhile ((idx >) . fst) $ zip psums sels
  in Just (c,(hst',idx-psum-1))

solution =  index2word $
  word2index "legionary"
    + word2index "calorimeters"
    - word2index "annihilate"
    + word2index "orchestrated"
    - word2index "fluttering"

main = print solution