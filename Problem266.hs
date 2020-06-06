import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Numbers.Primes
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Search.Range
import System.Random

primes190 :: [Integer]
primes190 = takeWhile (<190) primes

root190 :: Integer
root190 = isqrt (product primes190)

isqrt :: Integer -> Integer
isqrt n = maybe n pred (searchFromTo (\k -> k*k > n) 0 n)

subsets :: [a] -> [[a]]
subsets = foldr (\x -> concatMap (\ys -> [x:ys,ys])) [[]]

solve =
  let (ps,qs) = splitAt 21 primes190
      xs = sort $ map product (subsets ps)
      ys = sortBy (flip compare) $ map product (subsets qs)
      combine zs x =
        let m = root190 `div` x
            zs'@(z:_) = dropWhile (m <) zs
        in  (zs',x*z)
  in maximum $ snd $ mapAccumL combine ys xs

main = print solve `mod` 10^16



boundedSubsets :: Int -> [a] -> [[a]]
boundedSubsets k []     = [[]]
boundedSubsets 0 xs     = [[]]
boundedSubsets k (x:xs) = map (x:) (boundedSubsets (k-1) xs) ++ boundedSubsets k xs

data Splitting = Splitting { _in, _out :: Set Integer }

instance Show Splitting where
  show spl = "Splitting " ++ show (product (_in spl)) ++ " " ++ show (product (_out spl))

initial :: Integer -> [Integer] -> Splitting
initial goal ps =
  let (pre,post) = span ((goal >=) . snd) $ zip ps (scanl1 (*) ps)
      toSet = Set.fromList . map fst
  in  Splitting { _in = toSet pre, _out = toSet post }

improve :: Integer -> Splitting -> Maybe Splitting
improve goal spl = do
  let candidates = sort . map (\ps -> (product ps, Set.fromDistinctAscList ps)) . boundedSubsets 5 . Set.toAscList
      xs = candidates (_in spl)
      ys = candidates (_out spl)
      bnd :: Double
      bnd = fromRational (goal % product (_in spl))
      combine zs (x,ps) =
        let (pre,zs') = span (\(z,_) -> fromRational (z % x) <= bnd) zs
            best = case reverse pre of
              []       -> Nothing
              (y,qs):_ -> Just (fromRational (y % x), ps, qs)
        in  (zs',best)
      (rate, del, ins) = maximum $ catMaybes $ snd $ mapAccumL combine ys xs
  guard (rate > 1.0)
  return $ Splitting
    { _in  = (_in spl  `Set.difference` del) `Set.union` ins
    , _out = (_out spl `Set.difference` ins) `Set.union` del
    }

improvement :: Integer -> Splitting -> Splitting
improvement goal = whileJust (improve goal)

whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = case f x of
  Nothing -> x
  Just y  -> whileJust f y

main' = do
  gen <- getStdGen
  run gen 0

run gen mx = do
  let (ps,gen') = fisherYates gen primes190
      spl = improvement root190 (initial root190 ps)
      val = product (_in spl)
  when (val > mx) $
    putStrLn $ "new max: " ++ show val ++ " " ++ show (val `mod` 10^16)
  run gen' (max mx val)


fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)