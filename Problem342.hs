import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import Data.Numbers.Primes (primes,primeFactors)

run :: Int -> Int -> IntMap Int -> [Int] -> [Int]
run _     n _ [] = return n
run limit n req (p:ps) = do
  let req_p = Map.findWithDefault 0 p req
      ks' = filter (\k -> (2*k-1+req_p) `mod` 3 == 0) $ takeWhile (\k -> n*p^k < limit) [1 ..]
      ks  = if req_p `mod` 3 == 0 then 0:ks' else ks'
      req' = Map.unionWith (+) req (Map.fromAscListWith (+) $ map (flip (,) 1) $ primeFactors (p-1))
  k <- ks
  run limit (n*p^k) (Map.delete p $ if k == 0 then req else req') ps

solve :: Int -> [Int]
-- The list returned by run always starts with 1, which we don't want.
solve limit = tail $ run limit 1 Map.empty (reverse $ takeWhile (\p -> p*p < limit) primes)

main = print $ sum $ solve (10^10)