
-- | Arbitrary prime fields, naive implementation

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Math.FiniteField.PrimeField.Generic 
  ( -- * Witness for the existence of the field
    WitnessFp(..)
  , SomeWitnessFp(..)
  , mkPrimeField
  , unsafePrimeField
    -- * Field elements
  , Fp
  )
  where

--------------------------------------------------------------------------------

import Data.Bits

import GHC.TypeNats (Nat)

import System.Random ( RandomGen , randomR )

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel
import Math.FiniteField.Class

import qualified Math.FiniteField.PrimeField.Generic.Raw as Raw

--------------------------------------------------------------------------------

-- | A witness for the existence of the prime field @F_p@
newtype WitnessFp (p :: Nat) 
  = WitnessFp { fromWitnessFp :: IsPrime p }
  deriving Show

data SomeWitnessFp 
  = forall p. SomeWitnessFp (WitnessFp p) 

deriving instance Show SomeWitnessFp

-- | Note: currently this checks the primality of the input using
-- trial division, so it's not really practical...
--
-- But you can use 'unsafePrimeField' to cheat.
mkPrimeField :: Integer -> Maybe SomeWitnessFp
mkPrimeField p = case someSNat p of
  SomeSNat sp -> (SomeWitnessFp . WitnessFp) <$> isPrime sp 

-- | You are responsible for guaranteeing that the input is a prime.
unsafePrimeField :: Integer -> SomeWitnessFp
unsafePrimeField p = case someSNat p of
  SomeSNat sp -> SomeWitnessFp (WitnessFp (believeMeItsPrime sp))

--------------------------------------------------------------------------------

-- | An element of the prime field @F_p@ 
data Fp (p :: Nat) 
  = Fp !(IsPrime p) !Integer

fpWitness :: Fp p -> WitnessFp p
fpWitness (Fp p _) = WitnessFp p

-- | Constructing elements
fp :: IsPrime p -> Integer -> Fp p
fp !p !n 
  | n >= 0 && n < q  = Fp p n
  | otherwise        = Fp p (mod n q)
  where
    !q = fromPrime p

randomFp :: RandomGen gen => IsPrime p -> gen -> (Fp p, gen)
randomFp !p !gen = case randomR (0,q-1) gen of { (x, gen') -> (Fp p x, gen') } where !q = fromPrime p

randomInvFp :: RandomGen gen => IsPrime p -> gen -> (Fp p, gen)
randomInvFp !p !gen = case randomR (1,q-1) gen of { (x, gen') -> (Fp p x, gen') } where !q = fromPrime p

-- | The order of the field
fpOrder :: Fp p -> Integer
fpOrder (Fp p _) = fromPrime p

modp :: Integer -> IsPrime p -> Integer
modp !x !p = mod x (fromPrime p)

instance Eq (Fp p) where
  (==) (Fp _ x) (Fp _ y) = x == y

-- | Note: the @Ord@ instance is present only so that you can use 'GF' as key
-- in @Maps@ - the ordering is kind of arbitrary! 
instance Ord (Fp p) where
  compare (Fp _ x) (Fp _ y) = compare x y

instance Show (Fp p) where
  show (Fp p k) = "(" ++ show k ++ " mod " ++ show (fromPrime p) ++ ")"

instance Num (Fp p) where
  fromInteger = error "PrimeField/Generic/Fp/fromInteger: not defined; use `fp` instead" 
  negate (Fp p x) = Fp p (Raw.neg (fromPrime p) x)
  (+) (Fp p x) (Fp _ y) = Fp p (Raw.add (fromPrime p) x y)
  (-) (Fp p x) (Fp _ y) = Fp p (Raw.sub (fromPrime p) x y)
  (*) (Fp p x) (Fp _ y) = Fp p (Raw.mul (fromPrime p) x y)
  abs = id
  signum (Fp p _) = Fp p 1

instance Fractional (Fp p) where
  fromRational = error "PrimeField/Generic/Fp/fromRational: not defined; use `fp` instead" 
  recip (Fp p x)          = Fp p (Raw.inv (fromPrime p) x)
  (/)   (Fp p x) (Fp _ y) = Fp p (Raw.div (fromPrime p) x y)

instance Field (Fp p) where
  type Witness (Fp p) = WitnessFp p
  characteristic    w = case w of { WitnessFp p -> fromPrime p }
  dimension         _ = 1
  fieldSize         w = case w of { WitnessFp p -> fromPrime p }
  enumerate         w = case w of { WitnessFp p -> let q = fromPrime p in [ fp p k | k<-[0..q-1] ] }
  embed           w x = fp (fromWitnessFp w) x
  primGen             = error "PrimeField/Generic/Fp: primGen: not implemented"
  witnessOf           = fpWitness
  power               = fpPow
  randomFieldElem   w = case w of { WitnessFp p -> randomFp    p } 
  randomInvertible  w = case w of { WitnessFp p -> randomInvFp p }
  zero w = Fp (fromWitnessFp w) 0
  one  w = Fp (fromWitnessFp w) 1
  isZero (Fp _ x) = (x == 0)
  isOne  (Fp _ x) = (x == 1)

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
