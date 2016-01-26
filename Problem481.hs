module Problem481 where

import Data.List hiding (maximumBy)
import qualified Data.MemoCombinators as Memo
import Data.Ord
import Data.Ratio

type Chef = Int
type Prob = Double

lastChef :: Chef
lastChef = 14

skill :: Chef -> Prob
skill = Memo.arrayRange (1,lastChef) skill'
    where skill' :: Chef -> Prob
          skill' n = fromRational $ fibs !! n % fibs !! (lastChef+1)
              where fibs = 0:1:zipWith (+) fibs (tail fibs)

win :: [Chef] -> Chef -> Prob
win = Memo.memo2 (Memo.list $ Memo.arrayRange (1,lastChef)) (Memo.arrayRange (1,lastChef)) win'
    where win' :: [Chef] -> Chef -> Prob
          win' []                 _ = error "win: empty list"
          win' xs                 z
              | z `notElem` xs      = 0
          win' [x]                z = 1
          win' xs@(x1:xs'@(x2:_)) z      
              | x1 > x2             = let step (ai,bi) ys@(y:_) = (ai * (1 - skill y), bi + ai * skill y * win (eliminate ys) z)
                                          (an,bn) = foldl step (1,0) (rotations xs)
                                      in  bn / (1-an)
              | otherwise           = skill x1 * win (eliminate xs) z + (1 - skill x1) * win (xs' ++ [x1]) z

dishes :: [Chef] -> Prob
dishes = Memo.list (Memo.arrayRange (1,lastChef)) dishes'
    where dishes' :: [Chef] -> Prob
          dishes' []      = error "dishes: empty list"
          dishes' [x]     = 0
          dishes' xs@(x1:xs'@(x2:_))
              | x1 > x2   = let step (ai,bi) ys@(y:_) = (ai * (1 - skill y), bi + ai * (1 + skill y * dishes (eliminate ys)))
                                (an,bn) = foldl step (1,0) (rotations xs)
                            in  bn / (1-an)
              | otherwise = 1 + skill x1 * dishes (eliminate xs) + (1 - skill x1) * dishes (xs' ++ [x1])

eliminate :: [Chef] -> [Chef]
eliminate []     = error "eliminate: empty list"
eliminate [_]    = error "eliminate: singleton list"
eliminate [x,y]  = [x]
eliminate (x:xs) = maximumBy (comparing $ flip win x) $ delete1 (xs ++ [x])

maximumBy        :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []   =  error "maximumBy: empty list"
maximumBy cmp xs =  foldl1 maxBy xs
    where maxBy x y = case cmp x y of
                          LT -> y
                          _  -> x

delete1 :: [a] -> [[a]]
delete1 []     = error "delete1: empty list"
delete1 [x]    = []
delete1 (x:xs) = xs:map (x:) (delete1 xs)

rotations :: [a] -> [[a]]
rotations xs = trim (map trim $ tails $ cycle xs)
    where trim = zipWith (flip const) xs

main = print $ dishes [1..lastChef]