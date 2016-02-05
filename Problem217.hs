{-# LANGUAGE DataKinds, TypeOperators #-}
import Data.MemoCombinators
import Modulo
import Numbers

infixl 6 .+.
infixr 7 *.

main = print $ (t 47 :: MultiSet (Int % 14348907))

data MultiSet n = MultiSet { _size, _sum :: !n }
  deriving (Show)

empty :: Num n => MultiSet n
empty = MultiSet { _size = 0, _sum = 0 }

singleton :: Num n => n -> MultiSet n
singleton d = MultiSet { _size = 1, _sum = d }

union :: Num n => MultiSet n -> MultiSet n -> MultiSet n
union x y = MultiSet { _size = _size x + _size y, _sum = _sum x + _sum y }

(*.) :: Num n => n -> MultiSet n -> MultiSet n
k *. x = x { _sum = k * _sum x }

(.+.) :: Num n => MultiSet n -> MultiSet n -> MultiSet n
x .+. y = MultiSet { _size = _size x * _size y, _sum = _size x*_sum y + _size y*_sum x }

x, y :: Num n => Int -> Int -> MultiSet n
x = memo2 (arrayRange (1,23)) (arrayRange (0,23*9)) $ x'
x' 1 d
  | 1 <= d && d <= 9 = singleton (fromIntegral d)
  | otherwise        = empty
x' k d = foldl union empty [ 10 *. x (k-1) (d-l) .+. singleton (fromIntegral l) | l <- [0 .. min 9 d] ]
y = memo2 (arrayRange (1,23)) (arrayRange (0,23*9)) $ y'
y' 1 d
  | 0 <= d && d <= 9 = singleton (fromIntegral d)
  | otherwise        = empty
y' k d = y (k-1) d `union` x k d

t :: Num n => Int -> MultiSet n
t = arrayRange (1,23) t'
t' 1 = foldl union empty (map (singleton . fromInteger) [0 .. 9])
t' n =
  let k = n `div` 2
      digits = foldl union empty (map (singleton . fromInteger) [0..9])
      new
        | even n = [ 10^k *. x k d .+. y k d | d <- [1 .. k*9] ]
        | odd  n = [ 10^(k+1) *. x k d .+. 10^k *. digits .+. y k d | d <- [1 .. k*9] ]
  in  foldl union empty (t (n-1) : new)
