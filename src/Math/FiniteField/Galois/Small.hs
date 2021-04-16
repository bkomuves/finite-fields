
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
--

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, GADTs, TypeFamilies #-}
module Math.FiniteField.Galois.Small where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word
import Data.List

import GHC.TypeNats (Nat)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec

import Math.FiniteField.Class 
import Math.FiniteField.TypeLevel
import Math.FiniteField.Conway
import Math.FiniteField.Primes
import Math.FiniteField.Misc

import qualified Math.FiniteField.PrimeField.Small.Raw  as Raw
import qualified Math.FiniteField.Galois.Small.Internal as Quo

--------------------------------------------------------------------------------  

-- | We need either a Conway polynomial, or in the @m=1@ case, a proof that @p@ is prime
data WitnessGF (p :: Nat) (m :: Nat) where
  WitnessFp :: IsSmallPrime  p   -> WitnessGF p 1
  WitnessFq :: HasConwayPoly p m -> WitnessGF p m

fromWitnessGF :: WitnessGF p m -> (Word64,Int)
fromWitnessGF w = case w of
  WitnessFp p -> (fromSmallPrime p, 1)
  WitnessFq c -> conwayParams c

--------------------------------------------------------------------------------  

-- | An element of the finite field of order @q = p^m@
data Fq (p :: Nat) (m :: Nat) where
  Fp :: {-# UNPACK #-} !(IsSmallPrime  p  ) -> {-# UNPACK #-} !Word64          -> Fq p 1
  Fq :: {-# UNPACK #-} !(HasConwayPoly p m) ->                !(Vector Word32) -> Fq p m

fqWitness :: Fq p m -> WitnessGF p m
fqWitness element = case element of
  Fp p _ -> WitnessFp p
  Fq c _ -> WitnessFq c

-- | An element of the prime field
fp :: WitnessGF p m -> Word64 -> Fq p m
fp witness x = 
  case witness of
    WitnessFp p -> fp1 p x 
    WitnessFq c -> fpM c x

  where
    fpM :: HasConwayPoly p m -> Word64 -> Fq p m
    fpM conway x = Fq conway (Vec.fromListN m (y : replicate (m-1) 0)) where
      (p,m) = conwayParams conway
      y = if x >= 0 && x < p then fromIntegral x else fromIntegral (mod x p) :: Word32
    
    fp1 :: IsSmallPrime p -> Word64 -> Fq p 1
    fp1 prime x = Fp prime y where
      p = fromSmallPrime prime 
      y = if x >= 0 && x < p then x else mod x p

-- | The field element corresponding to the polynomial @X@ (which is a primitive generator)
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
  fromInteger = error "Fq/fromInteger: cannot be defined; use `embed` instead" 
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  signum = const1

instance Fractional (Fq p m) where
  fromRational = error "Fp/fromRational: cannot be defined; use `embed` instead" 
  recip = error "Fp/recip: not implemented yet" -- inv
  (/)   = error "Fp/div: not implemented yet"   -- div

instance Field (Fq p m) where
  type Witness (Fq p m) = WitnessGF p m
  characteristic  _ w = fromIntegral (fst (fromWitnessGF w))
  dimension       _ w = fromIntegral (snd (fromWitnessGF w))
  fieldSize       _ w = case fromWitnessGF w of (p,m) -> (fromIntegral p :: Integer) ^ m
  enumerate         w = enumerateFq w
  witnessOf         x = fqWitness x
  embed           w x = fp w (fromInteger  x)
  embedSmall      w x = fp w (fromIntegral x)
  power               = error "Fq/power: not implemented yet" -- fqPow
  primGen           w = case w of
                          WitnessFp p  -> error "GaloisField/Small/Fp: primGen: not implemented"
                          WitnessFq c  -> gen c

--------------------------------------------------------------------------------  
-- * Enumerations

-- | Enumerate all field elements (in a lexicographic order)
enumerateFq :: WitnessGF p m -> [Fq p m]
enumerateFq witness = 
  case witness of
    WitnessFp p -> enumerateFp1 p
    WitnessFq c -> enumerateFpM c

  where
    enumerateFpM :: HasConwayPoly p m -> [Fq p m]
    enumerateFpM conway = [ Fq conway vec | vec <- vecs ] where
      (p,m) = conwayParams conway
      shape = replicate m (fromIntegral (p-1) :: Word32)
      vecs = map (Vec.fromListN m) (word32Tuples' shape)
    
    enumerateFp1 :: IsSmallPrime p -> [Fq p 1]
    enumerateFp1 prime = [ Fp prime k | k <- [0..fromSmallPrime prime-1] ]

--------------------------------------------------------------------------------  
-- * Field operations

const1 :: Fq p m -> Fq p m
const1 what = case what of
  Fp p _ -> fp (WitnessFp p) 1
  Fq c _ -> fp (WitnessFq c) 1

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
