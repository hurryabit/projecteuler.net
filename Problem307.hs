{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf

main = printf "%.10f\n" $ solve2 20000 1000000 (1e-10)

solve2 :: Int -> Int -> Double -> Double
solve2 k n prec =
  let ((tab,del),_) = iterate (processAndCutTable n) (initTable,prec) !! k
  in  del

initTable :: Table
initTable = ([(0,[(0,1)])],0)

processAndCutTable :: Int -> (Table,Double) -> (Table,Double)
processAndCutTable n (tab,eps) = cutTable eps (processTable n tab)

cutTable :: Double -> Table -> (Table,Double)
cutTable eps tab@(ls,del) =
  let thr = eps / fromIntegral (size tab)
      (keep,cut) = unzip $ map (cutLine thr) ls
  in  ((catMaybes keep,del),eps-sum cut)

cutLine :: Double -> Line -> (Maybe Line,Double)
cutLine thr (q,cs) =
  let (keep,cut) = partition ((thr <=) . snd) cs
      line = guard (not (null keep)) >> return (q,keep)
  in  (line,sum (map snd cut))


type Cell = (Int,Double) -- (p,prob)

processCell :: Int -> Int -> Cell -> (Cell,Maybe Cell,Double)
processCell n q (p,prob) =
  let new = (p+1,fromIntegral (n-q-p) / fromIntegral n * prob)
      old = guard (p>0) >> return (p-1,fromIntegral p / fromIntegral n * prob)
      del = fromIntegral q / fromIntegral n * prob
  in  (new,old,del)


type Line = (Int,[Cell]) -- (q,[(p,prob)])

processLine :: Int -> Line -> (Line,Maybe Line,Double)
processLine n (q,cs) =
  let (news,olds0,dels) = unzip3 $ map (processCell n q) cs
      olds = catMaybes olds0
      old = guard (not (null olds)) >> return (q+1,olds)
  in  ((q,news),old,sum dels)

type Table = ([Line],Double)

processTable :: Int -> Table -> Table
processTable n (ls,del) =
  let (newss,oldss,dels) = unzip3 $ map (processLine n) ls
  in  (mergeWith (mergeWith (+)) newss (catMaybes oldss),del + sum dels)

size :: Table -> Int
size = sum . map (length . snd) . fst

mergeWith :: Ord a => (b -> b -> b) -> [(a,b)] -> [(a,b)] -> [(a,b)]
mergeWith op []          jys         = jys
mergeWith op ixs         []          = ixs
mergeWith op ((i,x):ixs) ((j,y):jys) = case i `compare` j of
  LT -> (i,x)        : mergeWith op ixs         ((j,y):jys)
  EQ -> (i,x `op` y) : mergeWith op ixs         jys
  GT -> (j,y)        : mergeWith op ((i,x):ixs) jys
