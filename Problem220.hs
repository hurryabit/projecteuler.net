module Problem220 where

import qualified Data.MemoCombinators as Memo
import Data.Monoid

data Action = F | L | R | A Int | B Int
    deriving (Show)

reduce1 :: Action -> [Action]
reduce1 (A 0) = []
reduce1 (A n) = [A (n-1),R,B (n-1),F,R]
reduce1 (B 0) = []
reduce1 (B n) = [L,F,A (n-1),L,B (n-1)]
reduce1 act   = [act]


data Effect = Effect { moveX, moveY :: Integer
                     , rotate       :: Int
                     , steps        :: Integer
                     }
    deriving (Show)

instance Monoid Effect where
    mempty = Effect 0 0 0 0
    e1 `mappend` e2 = let (dx,dy) = case rotate e1 of
                                      0 -> (         moveX,          moveY)
                                      1 -> (negate . moveY,          moveX)
                                      2 -> (negate . moveX, negate . moveY)
                                      3 -> (         moveY, negate . moveX)
                      in  Effect { moveX  = moveX e1 + dx e2
                                 , moveY  = moveY e1 + dy e2
                                 , rotate = (rotate e1 + rotate e2) `mod` 4
                                 , steps  = steps e1 + steps e2
                                 }

effect1 :: Action -> Effect
effect1 F     = Effect 0 1 0 1
effect1 L     = Effect 0 0 1 0
effect1 R     = Effect 0 0 3 0
effect1 (A n) = Memo.integral (effect . reduce1 . A) n
effect1 (B n) = Memo.integral (effect . reduce1 . B) n

effect :: [Action] -> Effect
effect = mconcat . map effect1

locate :: [Action] -> Integer -> (Integer,Integer)
locate = locate' mempty
    where locate' :: Effect -> [Action] -> Integer -> (Integer,Integer)
          locate' e _      0 = (moveX e,moveY e)
          locate' _ []     _ = error "too many steps"
          locate' e (a:as) t = let ea = effect1 a
                                   ta = steps ea
                               in  case ta `compare` t of
                                     GT -> locate' e (reduce1 a) t
                                     EQ -> locate' (e `mappend` ea) [] 0
                                     LT -> locate' (e `mappend` ea) as (t-ta)

solution :: (Integer,Integer)
solution = locate [F,A 50] (10^12)

main :: IO ()
main = print solution