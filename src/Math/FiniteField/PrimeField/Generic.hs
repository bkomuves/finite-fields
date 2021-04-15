
-- | Prime fields, naive implementation

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures #-}
module Math.FiniteField.PrimeField.Generic where

--------------------------------------------------------------------------------

import Data.Bits
import GHC.TypeNats (Nat)

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel

--------------------------------------------------------------------------------

-- | The prime field @F_p@ 
data Fp (p :: Nat) 
  = Fp !(IsPrime p) !Integer

-- | Constructing elements
fp :: Integer -> IsPrime p -> Fp p
fp n p = Fp p (modp n p)

-- | The order of the field
fpOrder :: Fp p -> Integer
fpOrder (Fp p _) = fromPrime p

modp :: Integer -> IsPrime p -> Integer
modp x p = mod x (fromPrime p)

instance Eq (Fp p) where
  (==) (Fp _ x) (Fp _ y) = x == y

instance Show (Fp p) where
  show (Fp p k) = "(" ++ show k ++ " mod " ++ show (fromPrime p) ++ ")"

instance Num (Fp p) where
  fromInteger = error "Fp/fromInteger: not defined; use `fp` instead" 
  negate fp@(Fp p x) = if x == 0 then fp else Fp p (fromPrime p - x)
  (+) (Fp p x) (Fp _ y) = let { a = x + y ; q = fromPrime p } in if a < q then Fp p a else Fp p (a - q)
  (-) (Fp p x) (Fp _ y) = let a = x - y in if a >= 0 then Fp p a else Fp p (fromPrime p + a)
  (*) (Fp p x) (Fp _ y) = Fp p (mod (x*y) (fromPrime p))
  abs = id
  signum (Fp p _) = Fp p 1

instance Fractional (Fp p) where
  fromRational = error "Fp/fromRational: not defined; use `fp` instead" 
  recip = fpInv
  (/)   = fpDiv

--------------------------------------------------------------------------------
-- * Nontrivial operations

fpPow :: Fp p -> Integer -> Fp p
fpPow z e 
  | e < 0     = fpPow (fpInv z) (negate e)
  | e >= pm1  = go 1 z (mod e pm1)
  | otherwise = go 1 z e
  where
    pm1 = fpOrder z - 1

    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go  acc    (y*y) (shiftR e 1)
        _ -> go (acc*y) (y*y) (shiftR e 1)

-- | Inversion (using Euclid's algorithm)
fpInv :: Fp p -> Fp p
fpInv (Fp p a) 
  | a == 0    = Fp p 0
  | otherwise = Fp p (euclid q 1 0 a q) 
  where
    q = fromPrime p

-- | Division via Euclid's algorithm
fpDiv :: Fp p -> Fp p -> Fp p
fpDiv (Fp p a) (Fp _ b)
  | b == 0    = Fp p 0
  | otherwise = Fp p (euclid q a 0 b q) 
  where
    q = fromPrime p

-- | Division via multiplying by the inverse
fpDiv' :: Fp p -> Fp p -> Fp p
fpDiv' a b = a * fpInv b

--------------------------------------------------------------------------------
-- * Euclidean algorithm

-- | Extended binary Euclidean algorithm
euclid :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 
euclid p x1 x2 u v = go x1 x2 u v where

  halfp1 = shiftR (p+1) 1

  modp :: Integer -> Integer
  modp n = mod n p

  -- Inverse using the binary Euclidean algorithm 
  euclid :: Integer -> Integer
  euclid a 
    | a == 0     = 0
    | otherwise  = go 1 0 a p
  
  go :: Integer -> Integer -> Integer -> Integer -> Integer
  go !x1 !x2 !u !v 
    | u==1       = x1
    | v==1       = x2
    | otherwise  = stepU x1 x2 u v

  stepU :: Integer -> Integer -> Integer -> Integer -> Integer
  stepU !x1 !x2 !u !v = if even u 
    then let u'  = shiftR u 1
             x1' = if even x1 then shiftR x1 1 else shiftR x1 1 + halfp1
         in  stepU x1' x2 u' v
    else     stepV x1  x2 u  v

  stepV :: Integer -> Integer -> Integer -> Integer -> Integer
  stepV !x1 !x2 !u !v = if even v
    then let v'  = shiftR v 1
             x2' = if even x2 then shiftR x2 1 else shiftR x2 1 + halfp1
         in  stepV x1 x2' u v' 
    else     final x1 x2  u v

  final :: Integer -> Integer -> Integer -> Integer -> Integer
  final !x1 !x2 !u !v = if u>=v

    then let u'  = u-v
             x1' = if x1 >= x2 then modp (x1-x2) else modp (x1+p-x2)               
         in  go x1' x2  u' v 

    else let v'  = v-u
             x2' = if x2 >= x1 then modp (x2-x1) else modp (x2+p-x1)
         in  go x1  x2' u  v'

--------------------------------------------------------------------------------
