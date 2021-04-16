
-- | Small Galois fields via a precomputed table of Conway polynomials.
--
-- This covers:
--
-- * all fields with order <= 2^30
--
-- * all fields with characteristic < 2^16 and order < 2^64 (?)
--
-- * higher powers for very small prime characteristic
--
-- * some more
--
-- To look up Conway polynomials, see the module "Math.FiniteField.Conway".


{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, GADTs #-}
module Math.FiniteField.Galois.Small where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word
import Data.List

import GHC.TypeNats (Nat)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel
import Math.FiniteField.Conway
import Math.FiniteField.Misc

import qualified Math.FiniteField.PrimeField.Small.Raw  as Raw
import qualified Math.FiniteField.Galois.Small.Internal as Quo

--------------------------------------------------------------------------------  

-- | An element of the finite field of order @q = p^m@
data Fq (p :: Nat) (m :: Nat) where
  Fp :: {-# UNPACK #-} !(IsSmallPrime  p  ) -> {-# UNPACK #-} !Word64          -> Fq p 1
  Fq :: {-# UNPACK #-} !(HasConwayPoly p m) ->                !(Vector Word32) -> Fq q n

-- | An element of the prime field
fp :: HasConwayPoly p m -> Word64 -> Fq p m
fp conway x = Fq conway (Vec.fromListN m (y : replicate (m-1) 0)) where
  (p,m) = conwayParams conway
  y = fromIntegral (mod x p) :: Word32

-- | For @m = 1@ we don't need a Conway polynomial
fp_ :: IsSmallPrime p -> Word64 -> Fq p 1
fp_ prime x = Fp prime y where
  y = mod x (fromSmallPrime prime)

-- | The field element corresponding to the polynomial @X@ (which is a generator
-- of the (cyclic) multiplicative group)
gen :: HasConwayPoly p m -> Fq p m
gen conway = gen' conway 1

-- | The field element corresponding to the polynomial @c*X@
gen' :: HasConwayPoly p m -> Word64 -> Fq p m
gen' conway x = Fq conway (Vec.fromListN m (0 : y : replicate (m-2) 0)) where
  (p,m) = conwayParams conway
  y = fromIntegral (mod x p) :: Word32

--------------------------------------------------------------------------------  

instance Eq (Fq p m) where
  (==) (Fp _ x ) (Fp _ y ) = x == y
  (==) (Fq _ xs) (Fq _ ys) = xs == ys

instance Show (Fq p m) where
  show (Fp prime   x  ) = "<" ++ show x ++ " mod " ++ show (fromSmallPrime prime) ++ ">"
  show (Fq witness vec) = "<" ++ intercalate " + " list ++ " mod " ++ show p ++ ">" where
    (p,m) = conwayParams witness
    list = zipWith f [0..] (Vec.toList vec) 
    f  0 !v = show v
    f  1 !v = show v ++ "*X"
    f !e !v = show v ++ "*X^" ++ show e

instance Num (Fq p m) where
  fromInteger = error "Fq/fromInteger: cannot be defined; use `fq` instead" 
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  signum = const1

-- instance Fractional (Fq p) where
--   fromRational = error "Fp/fromRational: cannot be defined; use `fq` instead" 
--   recip = inv
--   (/)   = div

--------------------------------------------------------------------------------  
-- * Enumerations

-- | Enumerate all field elements (in a lexicographic order)
enumerateFq :: HasConwayPoly p m -> [Fq p m]
enumerateFq conway = [ Fq conway vec | vec <- vecs ] where
  (p,m) = conwayParams conway
  shape = replicate m (fromIntegral (p-1) :: Word32)
  vecs = map (Vec.fromListN m) (word32Tuples' shape)

enumerateFp :: IsSmallPrime p -> [Fq p 1]
enumerateFp prime = [ Fp prime k | k <- [0..fromSmallPrime prime-1] ]

-- | Enumerate all non-zero field elements, in cyclic order;
-- this is the multiplicative group of GF(p^m).
multiplicativeGroupFq :: HasConwayPoly p m -> [Fq p m]
multiplicativeGroupFq conway = scanl1 (*) (replicate (n-1) g) where
  g     = gen conway
  (p,m) = conwayParams conway
  n     = (fromIntegral p) ^ m :: Int

-- NOTE: we would need a table of primitive roots for this...
--
-- multiplicativeGroupFp :: IsSmallPrime p -> [Fq p 1]
-- multiplicativeGroupFp conway = scanl1 (*) (replicate (n-1) g) where
--   g     = gen conway
--   (p,m) = conwayParams conway
--   n     = (fromIntegral p) ^ m :: Int

--------------------------------------------------------------------------------  
-- * Field operations

const1 :: Fq p m -> Fq p m
const1 what = case what of
  Fp p _ -> fp_ p 1
  -- Fq c _ -> fp  c 1

neg :: Fq p m -> Fq p m 
neg (Fp p x ) = Fp p (Raw.neg (fromSmallPrime p) x ) 
neg (Fq c xs) = Fq c (Quo.neg (conwayPrime_   c) xs)

add :: Fq p m -> Fq p m -> Fq p m
add (Fp p x ) (Fp _ y ) = Fp p (Raw.add (fromSmallPrime p) x  y )
add (Fq c xs) (Fq _ ys) = Fq c (Quo.add (conwayPrime_   c) xs ys)

sub :: Fq p m -> Fq p m -> Fq p m
sub (Fp p x ) (Fp _ y ) = Fp p (Raw.sub (fromSmallPrime p) x  y ) 
sub (Fq c xs) (Fq _ ys) = Fq c (Quo.sub (conwayPrime_   c) xs ys)

mul :: Fq p m -> Fq p m -> Fq p m
mul (Fp p x ) (Fp _ y ) = Fp p (Raw.mul (fromSmallPrime p) x  y ) 
mul (Fq c xs) (Fq _ ys) = Fq c (Quo.mul (fromConwayPoly c) xs ys)

--------------------------------------------------------------------------------
