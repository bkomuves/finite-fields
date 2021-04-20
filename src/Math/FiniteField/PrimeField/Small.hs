
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

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Math.FiniteField.PrimeField.Small
  ( -- * Witness for the existence of the field
    WitnessFp(..)
  , SomeWitnessFp(..)
  , mkSmallPrimeField
  , unsafeSmallPrimeField
    -- * Field elements
  , Fp
  , primRoot
  )
  where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word

import GHC.TypeNats (Nat)

import System.Random ( RandomGen , randomR )

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel
import Math.FiniteField.Class 
import Math.FiniteField.Conway.Internal ( lookupConwayPrimRoot_ )

import qualified Math.FiniteField.PrimeField.Small.Raw as Raw

--------------------------------------------------------------------------------

-- | A witness for the existence of the prime field @F_p@
newtype WitnessFp (p :: Nat) 
  = WitnessFp { fromWitnessFp :: IsSmallPrime p }
  deriving Show

data SomeWitnessFp 
  = forall p. SomeWitnessFp (WitnessFp p) 

deriving instance Show SomeWitnessFp

-- | Note: currently this checks the primality of the input using
-- trial division, so it's only practical for (very) small primes...
--
-- But you can use 'unsafeSmallPrimeField' to cheat.
mkSmallPrimeField :: Int -> Maybe SomeWitnessFp
mkSmallPrimeField p = case someSNat64 (fromIntegral p) of
  SomeSNat64 sp -> (SomeWitnessFp . WitnessFp) <$> isSmallPrime sp 

-- | You are responsible for guaranteeing that the input is a prime.
unsafeSmallPrimeField :: Int -> SomeWitnessFp
unsafeSmallPrimeField p = case someSNat64 (fromIntegral p) of
  SomeSNat64 sp -> SomeWitnessFp (WitnessFp (believeMeItsASmallPrime sp))

--------------------------------------------------------------------------------

-- | An element of the prime field @F_p@
data Fp (p :: Nat) 
  = Fp {-# UNPACK #-} !(IsSmallPrime p) {-# UNPACK #-} !Word64

fpWitness :: Fp p -> WitnessFp p
fpWitness (Fp p _) = WitnessFp p

-- | Constructing elements
fp :: IsSmallPrime p -> Word64 -> Fp p
fp !p !n 
  | n >= 0 && n < q  = Fp p n
  | otherwise        = Fp p (mod n q)
  where
    !q = fromSmallPrime p

randomFp :: RandomGen gen => IsSmallPrime p -> gen -> (Fp p, gen)
randomFp !p !gen = case randomR (0,q-1) gen of { (x, gen') -> (Fp p x, gen') } where !q = fromSmallPrime p

randomInvFp :: RandomGen gen => IsSmallPrime p -> gen -> (Fp p, gen)
randomInvFp !p !gen = case randomR (1,q-1) gen of { (x, gen') -> (Fp p x, gen') } where !q = fromSmallPrime p

-- | The order of the field
fpOrder :: Fp p -> Word64
fpOrder (Fp p _) = fromIntegral (fromSmallPrime p)

modp :: Word64 -> IsSmallPrime p -> Word64
modp !x !p = mod x (fromSmallPrime p)

modpInteger :: Integer -> IsSmallPrime p -> Word64
modpInteger x p = fromIntegral (mod x (fromIntegral (fromSmallPrime p)))

modpSigned :: Int64 -> IsSmallPrime p -> Int64
modpSigned x p = mod x (fromSmallPrimeSigned p)

primRoot :: IsSmallPrime p -> Fp p 
primRoot p = case lookupConwayPrimRoot_ (fromIntegral (fromSmallPrime p)) of
  Just g     -> fp p (fromIntegral g)
  Nothing    -> error "PrimeField/Small/Fp/primRoot: primitive generator not found in the Conway table"

-- | Enumarte all field elements
enumerateFp :: IsSmallPrime p -> [Fp p]
enumerateFp p = [ Fp p k | k <- [0..fromSmallPrime p-1] ]

--------------------------------------------------------------------------------

instance Eq (Fp p) where
  (==) (Fp _ x) (Fp _ y) = x == y

-- | Note: the @Ord@ instance is present only so that you can use 'GF' as key
-- in @Maps@ - the ordering is kind of arbitrary! 
instance Ord (Fp p) where
  compare (Fp _ x) (Fp _ y) = compare x y

instance Show (Fp p) where
  show (Fp p k) = "(" ++ show k ++ " mod " ++ show (fromSmallPrime p) ++ ")"

instance Num (Fp p) where
  fromInteger = error "PrimeField/Small/Fp/fromInteger: cannot be defined; use `embed` instead" 
  negate (Fp p x) = Fp p (Raw.neg (fromSmallPrime p) x)
  (+) (Fp p x) (Fp _ y) = Fp p (Raw.add (fromSmallPrime p) x y)
  (-) (Fp p x) (Fp _ y) = Fp p (Raw.sub (fromSmallPrime p) x y)
  (*) (Fp p x) (Fp _ y) = Fp p (Raw.mul (fromSmallPrime p) x y)
  abs = id
  signum (Fp p _) = Fp p 1

instance Fractional (Fp p) where
  fromRational = error "PrimeField/Small/Fp/fromRational: cannot be defined; use `embed` instead" 
  recip (Fp p x)          = Fp p (Raw.inv (fromSmallPrime p) x)
  (/)   (Fp p x) (Fp _ y) = Fp p (Raw.div (fromSmallPrime p) x y)

instance Field (Fp p) where
  type Witness (Fp p) = WitnessFp p
  characteristic     w = case w of { WitnessFp p -> fromSmallPrimeInteger p }
  dimension          _ = 1
  fieldSize          w = case w of { WitnessFp p -> fromSmallPrimeInteger p }
  enumerate          w = case w of { WitnessFp p -> let q = fromSmallPrime p in [ fp p k | k<-[0..q-1] ] }
  embed            w x = fp (fromWitnessFp w) (fromInteger  x)
  embedSmall       w x = fp (fromWitnessFp w) (fromIntegral x)
  primGen            w = primRoot (fromWitnessFp w)
  witnessOf            = fpWitness
  power                = fpPow
  powerSmall       x e = fpPow_ x (fromIntegral e)
  frobenius            = id
  randomFieldElem    w = case w of { WitnessFp p -> randomFp    p } 
  randomInvertible   w = case w of { WitnessFp p -> randomInvFp p }
  zero w = Fp (fromWitnessFp w) 0
  one  w = Fp (fromWitnessFp w) 1
  isZero (Fp _ x) = (x == 0)
  isOne  (Fp _ x) = (x == 1)

--------------------------------------------------------------------------------
-- * Nontrivial operations

-- | Powers
fpPow_ :: Fp p -> Int64 -> Fp p
fpPow_ (Fp p x) e = Fp p (Raw.pow (fromSmallPrime p) x e)

fpPow :: Fp p -> Integer -> Fp p
fpPow (Fp p x) e = Fp p (Raw.pow' (fromSmallPrime p) x e)

-- | Inversion (using Euclid's algorithm)
fpInv :: Fp p -> Fp p
fpInv (Fp p a) = Fp p (Raw.inv (fromSmallPrime p) a)

-- | Division via Euclid's algorithm
fpDiv :: Fp p -> Fp p -> Fp p
fpDiv (Fp p a) (Fp _ b) = Fp p (Raw.div (fromSmallPrime p) a b)

-- | Division via multiplying by the inverse
fpDiv2 :: Fp p -> Fp p -> Fp p
fpDiv2 (Fp p a) (Fp _ b) = Fp p (Raw.div2 (fromSmallPrime p) a b)

--------------------------------------------------------------------------------
