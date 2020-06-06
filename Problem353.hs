import Control.Monad

type Point = (Int,Int,Int)

stations :: Int -> [Point]
stations r = do
  x <- [0 .. r]
  y <- takeWhile (\y -> x^2 + y^2 <= r^2) [x ..]
  let z = floor $ sqrt $ fromIntegral $ r^2-x^2-y^2
  guard $ x^2 + y^2 + z^2 == r^2
  return (x,y,z)

main = print $ length $ stations $ 2^15 - 1