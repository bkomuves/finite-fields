
-- | Prime fields, naive implementation

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, TypeFamilies #-}
module Math.FiniteField.PrimeField.Generic where

--------------------------------------------------------------------------------

import Data.Bits
import GHC.TypeNats (Nat)

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel
import Math.FiniteField.Class

import qualified Math.FiniteField.PrimeField.Generic.Raw as Raw

--------------------------------------------------------------------------------

-- | An element of the prime field @F_p@ 
data Fp (p :: Nat) 
  = Fp !(IsPrime p) !Integer

fpWitness :: Fp p -> IsPrime p
fpWitness (Fp p _) = p

-- | Constructing elements
fp :: IsPrime p -> Integer -> Fp p
fp !p !n 
  | n >= 0 && n < q  = Fp p n
  | otherwise        = Fp p (mod n q)
  where
    !q = fromPrime p
    
-- | The order of the field
fpOrder :: Fp p -> Integer
fpOrder (Fp p _) = fromPrime p

modp :: Integer -> IsPrime p -> Integer
modp !x !p = mod x (fromPrime p)

instance Eq (Fp p) where
  (==) (Fp _ x) (Fp _ y) = x == y

instance Show (Fp p) where
  show (Fp p k) = "(" ++ show k ++ " mod " ++ show (fromPrime p) ++ ")"

instance Num (Fp p) where
  fromInteger = error "Fp/fromInteger: not defined; use `fp` instead" 
  negate (Fp p x) = Fp p (Raw.neg (fromPrime p) x)
  (+) (Fp p x) (Fp _ y) = Fp p (Raw.add (fromPrime p) x y)
  (-) (Fp p x) (Fp _ y) = Fp p (Raw.sub (fromPrime p) x y)
  (*) (Fp p x) (Fp _ y) = Fp p (Raw.mul (fromPrime p) x y)
  abs = id
  signum (Fp p _) = Fp p 1

instance Fractional (Fp p) where
  fromRational = error "Fp/fromRational: not defined; use `fp` instead" 
  recip (Fp p x)          = Fp p (Raw.inv (fromPrime p) x)
  (/)   (Fp p x) (Fp _ y) = Fp p (Raw.div (fromPrime p) x y)

instance Field (Fp p) where
  type Witness (Fp p) = IsPrime p
  characteristic _ p = fromPrime p
  dimension      _ _ = 1
  fieldSize      _ p = fromPrime p
  enumerate        p = let q = fromPrime p in [ fp p k | k<-[0..q-1] ]
  embed              = fp
  primGen            = error "PrimeField/Generic/Fp: primGen: not implemented"
  witnessOf          = fpWitness
  power              = fpPow

--------------------------------------------------------------------------------
-- * Nontrivial operations

-- | Powers
fpPow :: Fp p -> Integer -> Fp p
fpPow (Fp p x) e = Fp p (Raw.pow (fromPrime p) x e)

-- | Inversion (using Euclid's algorithm)
fpInv :: Fp p -> Fp p
fpInv (Fp p a) = Fp p (Raw.inv (fromPrime p) a)

-- | Division via Euclid's algorithm
fpDiv :: Fp p -> Fp p -> Fp p
fpDiv (Fp p a) (Fp _ b) = Fp p (Raw.div (fromPrime p) a b)

-- | Division via multiplying by the inverse
fpDiv2 :: Fp p -> Fp p -> Fp p
fpDiv2 (Fp p a) (Fp _ b) = Fp p (Raw.div2 (fromPrime p) a b)

--------------------------------------------------------------------------------
