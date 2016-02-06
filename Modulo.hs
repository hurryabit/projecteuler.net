{-# LANGUAGE DataKinds, KindSignatures,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, TypeOperators #-}
module Modulo
  ( (%) ()
  , Modulo
  , modulo
  , fromModulo
  , modulus
  , compose
  , decompose
  , Nat
  , KnownNat ()
  , KnownPrime ()
  , CoPrime()
  )
where

import Data.Ix (Ix (range, index, inRange))
import Data.Ratio (denominator, numerator)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)
import GHC.TypeLits

newtype a % (n :: Nat) = Modulo a
  deriving (Eq, Ord)

type Modulo a n = a % n

instance (Show a, KnownNat n) => Show (a % n) where
  show x@(Modulo a) = show a ++ " (mod " ++ show (natVal x) ++ ")"

{-# INLINE modulo #-}
modulo :: (Integral a, KnownNat n, 2 <= n) => a -> a % n
modulo a =
  let x = Modulo (a `mod` modulus x)
  in  x

{-# INLINE fromModulo #-}
fromModulo :: (Integral a, KnownNat n, 2 <= n) => a % n -> a
fromModulo (Modulo a) = a

{-# INLINE modulus #-}
modulus :: (Integral a, KnownNat n, 2 <= n) => a % n -> a
modulus = fromInteger . natVal

{-# INLINE lift1 #-}
lift1 :: (Integral a, KnownNat n, 2 <= n) => (a -> a) -> (a % n -> a % n)
lift1 fun (Modulo a) = modulo (fun a)

{-# INLINE lift2 #-}
lift2 :: (Integral a, KnownNat n, 2 <= n) => (a -> a -> a) -> (a % n -> a % n -> a % n)
lift2 op (Modulo a) (Modulo b) = modulo (a `op` b)

instance (Integral a, KnownNat n, 2 <= n) => Num (a % n) where
  {-# INLINE fromInteger #-}
  fromInteger = modulo . fromInteger
  {-# INLINE (+) #-}
  (+) = lift2 (+)
  {-# INLINE (*) #-}
  (*) = lift2 (*)
  {-# INLINE (-) #-}
  (-) = lift2 (-)
  {-# INLINE negate #-}
  negate = lift1 negate
  {-# INLINE abs #-}
  abs x = x
  {-# INLINE signum #-}
  signum = lift1 signum

instance (Integral a, KnownNat n, 2 <= n) => Enum (a % n) where
  succ x@(Modulo a)
    | x == maxBound = badArgument "succ"
    | otherwise     = Modulo (succ a)
  pred x@(Modulo a)
    | x == minBound = badArgument "pred"
    | otherwise     = Modulo (pred a)
  toEnum n
    | a < 0 || modulus x <= a = badArgument "toEnum"
    | otherwise               = x
    where a = toEnum n
          x = Modulo a
  fromEnum (Modulo a) = fromEnum a
  {-# INLINE enumFrom #-}
  enumFrom = boundedEnumFrom
  {-# INLINE enumFromThen #-}
  enumFromThen = boundedEnumFromThen

instance (Integral a, KnownNat n, 2 <= n) => Bounded (a % n) where
  minBound = Modulo 0
  maxBound = modulo (-1)

instance (Integral a, KnownNat n, 2 <= n) => Ix (a % n) where
  {-# INLINE range #-}
  range (l,h) = [l .. h]
  {-# INLINE index #-}
  index r@(l,_) i
    | inRange r i = fromEnum i - fromEnum l
    | otherwise   = error "Modulo.index: index out of range"
  inRange (l,h) i = l <= i && i <= h



class (KnownNat n, 2 <= n) => KnownPrime n where

instance KnownPrime  2 where
instance KnownPrime  3 where
instance KnownPrime  5 where
instance KnownPrime  7 where
instance KnownPrime 11 where
instance KnownPrime 13 where
instance KnownPrime 17 where
instance KnownPrime 19 where

instance (Integral a, KnownPrime p, 2 <= p) => Fractional (a % p) where
  {-# INLINE recip #-}
  recip (Modulo a) =
    let (_,b,_) = euclid a (modulus x)
        x = modulo b
    in  x
  {-# INLINE fromRational #-}
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)


class (KnownNat m, KnownNat n) => CoPrime m n where


compose :: (Integral a, CoPrime m n, KnownNat (m*n), 2 <= m, 2 <= n, 2 <= (m*n)) => a % m -> a % n -> a % (m*n)
compose x@(Modulo a) y@(Modulo b) =
  let m = modulus x
      n = modulus y
      (_,r,s) = euclid m n 
  in  modulo (s*n*a+r*m*b)

decompose :: (Integral a, KnownNat m, KnownNat n, KnownNat (m*n), 2 <= m, 2 <= n, 2 <= (m*n)) => a % (m*n) -> (a % m,a % n)
decompose (Modulo a) = (modulo a,modulo a)


euclid :: Integral a => a -> a -> (a,a,a)
euclid a b =
  let (q,c) = a `quotRem` b
  in  case c of
        0 -> (b,0,1)
        _ -> let (d,r,s) = euclid b c
             in  (d,s,r-s*q)

badArgument :: String -> a
badArgument fun = error ("Modulo." ++ fun ++ ": bad argument")
