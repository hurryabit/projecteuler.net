module Problem98 where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import System.IO.Unsafe

words2k :: [String]
words2k = read . unsafePerformIO $ readFile "words.txt"

occurences :: String -> [(Char,Int)]
occurences = Map.toAscList . Map.fromListWith (+) . map (flip (,) 1)

anagrams :: [String] -> [[String]]
anagrams = map Set.toList . filter ((1 <) . Set.size) . Map.elems .
  Map.fromListWith Set.union . map (\w -> (occurences w,Set.singleton w))

searchSpace :: [[String]]
searchSpace = sortBy (flip (comparing (length . head))) . anagrams $ words2k

squares :: Set.Set Integer
squares = Set.fromList . filter (all ((1==) . snd) . occurences . show) .
  takeWhile (<1000000) . map (^2) $ [317..]

translate :: String -> String -> Integer -> Integer
translate t1 t2 n1 = read . map (\c -> fromJust $ Map.lookup c m) $ t2
  where m = Map.fromList (zip t1 (show n1))

candidates :: [(Integer,Integer)]
candidates = do
  [t1,t2] <- searchSpace
  let n = length t1
      k = 10^n
      squares = Set.fromAscList . filter (all ((1==) . snd) . occurences . show) .
                  takeWhile (<k) . map (^2) . enumFrom . ceiling . sqrt $ fromIntegral k / 10
  q1 <- Set.toList squares
  let q2 = translate t1 t2 q1
  guard $ q2 `Set.member` squares
  return (q1,q2)

solution = maximum $ concatMap (\(x,y) -> [x,y]) candidates
