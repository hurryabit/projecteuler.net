import Data.Bits
import Prelude hiding ((^^))
import Numbers (factorization, chinese2, invMod, powMod, divMaxPow, divides)

solution = eval (c $ c $ c $ 10000) (13^8)

c :: Mod -> Mod
c m = 2 ^^ (3 ^^ (m-2)) * 3 ^^ (3 * ((3 ^^ (m-3) - 1) // 2))

newtype Nat = Nat [(Int,Int)]
  deriving (Show, Eq)

prime :: Int -> Nat
prime p = Nat [(p,1)]

primePower :: Int -> Int -> Nat
primePower p k = Nat [(p,k)]

fromInt :: Int -> Nat
fromInt n
  | n > 0     = Nat (factorization n)
  | otherwise = error $ show n ++ " is not a natural number"

toInt :: Nat -> Int
toInt (Nat pks) = product $ map (uncurry (^)) pks

instance Num Nat where
  fromInteger = fromInt . fromInteger
  (+) = error $ "+ undefined for Nat"
  (-) = error $ "- undefined for Nat"
  Nat pks * Nat qls = Nat (pks `merge` qls)
    where
      pks         `merge` []          = pks
      []          `merge` qls         = qls
      ((p,k):pks) `merge` ((q,l):qls) = case p `compare` q of
        LT -> (p,k)   : (pks         `merge` ((q,l):qls))
        EQ -> (p,k+l) : (pks         `merge` qls        )
        GT -> (q,l)   : (((p,k):pks) `merge` qls        )
  negate = error $ "negate undefined for Nat"
  abs = error $ "abs undefined for Nat"
  signum = error $ "signum undefined for Nat"

phi :: Nat -> Nat
phi (Nat pks) =
  let pks' = [ (p,k-1) | (p,k) <- pks, k > 1 ]
  in  Nat pks' * product (map (fromInt . pred . fst) pks)

(%) :: Int -> Nat -> Int
a % n = a `mod` toInt n


newtype Mod = Mod { eval :: Nat -> Int }

instance Num Mod where
  fromInteger a = Mod $ \n -> fromInteger a % n
  Mod f + Mod g = Mod $ \n -> (f n + g n) % n
  Mod f * Mod g = Mod $ \n -> (f n * g n) % n
  Mod f - Mod g = Mod $ \n -> (f n - g n) % n
  abs = error $ "abs undefined for Mod"
  signum = error $ "signum undefined for Mod"

chinese :: (Int -> Int -> Int) -> Mod
chinese base = Mod $ \(Nat pks) -> fst $ foldl chinese2 (0,1) [ (base p k ,p^k) | (p,k) <- pks ]

(//) :: Mod -> Int -> Mod
Mod f // r = chinese $ \p k ->
  let (s,l) = divMaxPow r p
      p'k = p^k
  in  (f (primePower p (k+l)) `div` (p^l) * invMod s p'k) `mod` p'k

(^^) :: Mod -> Mod -> Mod
Mod f ^^ Mod g = chinese $ \p k ->
  let n   = primePower p k
      a   = f n
  in  if p `divides` a then 0 else powMod a (g (phi n)) (p^k)
