
-- | Small prime fields, up to @p < 2^31@ (using @Word64@ under the hood).
--
-- This should be faster than the generic implementation which
-- uses @Integer@ under the hood.
--
-- NB: Because the multiplication of two 32 bit integers needs 64 bits,
-- the limit is @2^32@ and not @2^64@. And because I'm lazy right now
-- to check if everything works properly unsigned, the actual limit 
-- is @2^31@ instead :)
--

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures #-}
module Math.FiniteField.PrimeField.Small where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word
import GHC.TypeNats (Nat)

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel

--------------------------------------------------------------------------------

data Fp (p :: Nat) 
  = Fp {-# UNPACK #-} !(IsSmallPrime p) {-# UNPACK #-} !Word64

-- | Constructing elements
fp :: Word64 -> IsSmallPrime p -> Fp p
fp n p = Fp p (modp n p)

-- | The order of the field
fpOrder :: Fp p -> Word64
fpOrder (Fp p _) = fromIntegral (fromSmallPrime p)

modp :: Word64 -> IsSmallPrime p -> Word64
modp x p = mod x (fromSmallPrime p)

modpInteger :: Integer -> IsSmallPrime p -> Word64
modpInteger x p = fromIntegral (mod x (fromIntegral (fromSmallPrime p)))

modpSigned :: Int64 -> IsSmallPrime p -> Int64
modpSigned x p = mod x (fromSmallPrimeSigned p)

instance Eq (Fp p) where
  (==) (Fp _ x) (Fp _ y) = x == y

instance Show (Fp p) where
  show (Fp p k) = "(" ++ show k ++ " mod " ++ show (fromSmallPrime p) ++ ")"

instance Num (Fp p) where
  fromInteger = error "Fp/fromInteger: not defined; use `fp` instead" 
  negate fp@(Fp p x) = if x == 0 then fp else Fp p (fromSmallPrime p - x)
  (+) (Fp p x) (Fp _ y) = let { a = x + y ; q = fromSmallPrime p } in if a < q then Fp p a else Fp p (a - q)
  (-) (Fp p x) (Fp _ y) = if x >= y then Fp p (x-y) else Fp p (fromSmallPrime p + x-y)
  (*) (Fp p x) (Fp _ y) = Fp p (mod (x*y) (fromSmallPrime p))
  abs = id
  signum (Fp p _) = Fp p 1

instance Fractional (Fp p) where
  fromRational = error "Fp/fromRational: not defined; use `fp` instead" 
  recip = fpInv
  (/)   = fpDiv

--------------------------------------------------------------------------------
-- * Nontrivial operations

fpPow :: Fp p -> Int64 -> Fp p
fpPow z e 
  | e < 0     = fpPow (fpInv z) (negate e)
  | e >= pm1i = go 1 z (mod e pm1i)
  | otherwise = go 1 z e
  where
    pm1  = fpOrder z - 1
    pm1i = fromIntegral pm1 :: Int64
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go  acc    (y*y) (shiftR e 1)
        _ -> go (acc*y) (y*y) (shiftR e 1)

fpPow' :: Fp p -> Integer -> Fp p
fpPow' z e 
  | e < 0       = fpPow' (fpInv z) (negate e)
  | e >= pm1big = go 1 z (mod e pm1big)
  | otherwise   = go 1 z e
  where
    pm1 = fpOrder z - 1
    pm1big = fromIntegral pm1 :: Integer
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go  acc    (y*y) (shiftR e 1)
        _ -> go (acc*y) (y*y) (shiftR e 1)

-- | Inversion (using Euclid's algorithm)
fpInv :: Fp p -> Fp p
fpInv (Fp p a) 
  | a == 0    = Fp p 0
  | otherwise = Fp p (euclid64 q 1 0 a q) 
  where
    q = fromSmallPrime p

-- | Division via Euclid's algorithm
fpDiv :: Fp p -> Fp p -> Fp p
fpDiv (Fp p a) (Fp _ b)
  | b == 0    = Fp p 0
  | otherwise = Fp p (euclid64 q a 0 b q) 
  where
    q = fromSmallPrime p

-- | Division via multiplying by the inverse
fpDiv' :: Fp p -> Fp p -> Fp p
fpDiv' a b = a * fpInv b

--------------------------------------------------------------------------------
-- * Euclidean algorithm

-- | Extended binary Euclidean algorithm
euclid64 :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 
euclid64 p x1 x2 u v = go x1 x2 u v where

  halfp1 = shiftR (p+1) 1

  modp :: Word64 -> Word64
  modp n = mod n p

  -- Inverse using the binary Euclidean algorithm 
  euclid :: Word64 -> Word64
  euclid a 
    | a == 0     = 0
    | otherwise  = go 1 0 a p
  
  go :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  go !x1 !x2 !u !v 
    | u==1       = x1
    | v==1       = x2
    | otherwise  = stepU x1 x2 u v

  stepU :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  stepU !x1 !x2 !u !v = if even u 
    then let u'  = shiftR u 1
             x1' = if even x1 then shiftR x1 1 else shiftR x1 1 + halfp1
         in  stepU x1' x2 u' v
    else     stepV x1  x2 u  v

  stepV :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  stepV !x1 !x2 !u !v = if even v
    then let v'  = shiftR v 1
             x2' = if even x2 then shiftR x2 1 else shiftR x2 1 + halfp1
         in  stepV x1 x2' u v' 
    else     final x1 x2  u v

  final :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  final !x1 !x2 !u !v = if u>=v

    then let u'  = u-v
             x1' = if x1 >= x2 then modp (x1-x2) else modp (x1+p-x2)               
         in  go x1' x2  u' v 

    else let v'  = v-u
             x2' = if x2 >= x1 then modp (x2-x1) else modp (x2+p-x1)
         in  go x1  x2' u  v'

--------------------------------------------------------------------------------
