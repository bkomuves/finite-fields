
-- | Type level naturals, singletons, prime witnesses and stuff.
--
-- This would be much simpler in a dependently typed language;
-- alas, we are in Haskell, so for now we have to feel the pain.
--
-- How to use this, the over-complicated way:
--
-- * create a 'SomeSNat' from an integer using the function 'someSNat'
-- 
-- * pattern match on it with a @case@ expression. Within the matched
--   scope, the type parameter is \"instantiated\". So all your program
--   will be \"inside\" this case branch (of course you can call out
--   to functions)
--
-- * create witnesses for this being prime and\/or being small using
--   the functions 'isPrime' and 'fits31Bits' 
--
-- * if you want small primes, create a \"small prime witness\" using
--   'mkSmallPrime'
--
-- * now you are ready to use the resulting witness (of type @IsPrime n@
--   or @IsSmallPrime n@) to create finite fields.
--
-- Or you can just the functions provided with each field implementation
-- to create the fields directly (in form of existantials, ie. sigma types,
-- so you still have to do the @case _ of@ thing).
--

{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, StandaloneDeriving #-}

module Math.FiniteField.TypeLevel
  ( -- * Singleton types
    SNat   , fromSNat   , proxyOfSNat   , proxyToSNat   , unsafeSNat
  , SNat64 , fromSNat64 , proxyOfSNat64 , proxyToSNat64 , unsafeSNat64
    -- * Creating singleton types
  , SomeSNat(..) , someSNat , SomeSNat64(..) , someSNat64 , someSNat64_
    -- * Small numbers
  , Fits31Bits , from31Bit , from31BitSigned , from31Bit' , fits31Bits 
    -- * Primes
  , IsPrime , fromPrime , fromPrime' 
  , isPrime , believeMeItsPrime
    -- * Small primes
  , IsSmallPrime , fromSmallPrime , fromSmallPrimeSigned , fromSmallPrimeInteger , fromSmallPrime' 
  , isSmallPrime , believeMeItsASmallPrime
  , smallPrimeIsPrime , smallPrimeIsSmall , mkSmallPrime
    -- * Divisors
  , Divides , _dividend , _divisor , _quotient 
  , dividendSNat , divisorSNat 
  , divides
  , Divisor(..) , constructDivisor , divisors
    -- * Proxy
  , proxyOf, proxyOf1
    -- * Sanity checking
  , checkSomeSNat , checkSomeSNat64
  ) 
  where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Proxy
import Data.List

import GHC.TypeNats
import Data.Proxy

import qualified Math.FiniteField.Primes as Primes
import Math.FiniteField.TypeLevel.Singleton

--------------------------------------------------------------------------------
-- * Primes

-- | Prime witness
newtype IsPrime (n :: Nat) where
  PrimeWitness :: SNat n -> IsPrime n

deriving instance Show (IsPrime n)

fromPrime' :: IsPrime n -> SNat n
fromPrime' (PrimeWitness sn) = sn

fromPrime :: IsPrime n -> Integer
fromPrime (PrimeWitness sn) = fromSNat sn

-- | Prime testing.
--
-- Note: this uses trial division at the moment, 
-- so it's only good for small numbers for now
--
isPrime :: SNat n -> Maybe (IsPrime n)
isPrime sn = if (fromSNat sn > 1) && Primes.isPrimeTrialDivision (fromSNat sn)
  then Just (PrimeWitness sn)
  else Nothing

-- | Escape hatch
believeMeItsPrime :: SNat n -> IsPrime n
believeMeItsPrime sn = PrimeWitness sn

--------------------------------------------------------------------------------
-- * Small numbers

newtype Fits31Bits (n :: Nat) where
  Witness31 :: SNat64 n -> Fits31Bits n

deriving instance Show (Fits31Bits n)

from31Bit' :: Fits31Bits n -> SNat64 n
from31Bit' (Witness31 sn) = sn

from31Bit :: Fits31Bits n -> Word64
from31Bit (Witness31 sn) = fromSNat64 sn

from31BitSigned :: Fits31Bits n -> Int64
from31BitSigned (Witness31 sn) = fromIntegral (fromSNat64 sn)

-- | Creating a witness for a number being small (less than @2^31@)
fits31Bits :: SNat64 n -> Maybe (Fits31Bits n)
fits31Bits sn@(SNat64 n) 
  | n >= 0 && n < 2^31   = Just (Witness31 sn)
  | otherwise            = Nothing

--------------------------------------------------------------------------------
-- * Small primes

newtype IsSmallPrime (n :: Nat) where
  SmallPrimeWitness :: SNat64 n -> IsSmallPrime n

deriving instance Show (IsSmallPrime n)

fromSmallPrime' :: IsSmallPrime n -> SNat64 n
fromSmallPrime' (SmallPrimeWitness sn) = sn

fromSmallPrime :: IsSmallPrime n -> Word64
fromSmallPrime (SmallPrimeWitness sn) = fromSNat64 sn

fromSmallPrimeInteger :: IsSmallPrime n -> Integer
fromSmallPrimeInteger (SmallPrimeWitness sn) = fromIntegral (fromSNat64 sn)

fromSmallPrimeSigned :: IsSmallPrime n -> Int64
fromSmallPrimeSigned (SmallPrimeWitness sn) = fromIntegral (fromSNat64 sn)

-- | Prime testing.
--
-- Note: this uses trial division at the moment, 
-- so it's only good for small numbers for now
--
isSmallPrime :: SNat64 n -> Maybe (IsSmallPrime n)
isSmallPrime sn = 
  if (n > 1) && (n < 2^31) && Primes.isPrimeTrialDivision (fromIntegral n)
    then Just (SmallPrimeWitness sn)
    else Nothing
  where
    n = fromSNat64 sn

smallPrimeIsPrime :: IsSmallPrime n -> IsPrime n
smallPrimeIsPrime (SmallPrimeWitness (SNat64 n)) = PrimeWitness (SNat (fromIntegral n))

smallPrimeIsSmall :: IsSmallPrime n -> Fits31Bits n
smallPrimeIsSmall (SmallPrimeWitness sn) = Witness31 sn

-- | Creating small primes
mkSmallPrime :: IsPrime p -> Fits31Bits p -> IsSmallPrime p
mkSmallPrime _ (Witness31 sn) = SmallPrimeWitness sn

-- | Escape hatch
believeMeItsASmallPrime :: SNat64 n -> IsSmallPrime n
believeMeItsASmallPrime sn = SmallPrimeWitness sn

--------------------------------------------------------------------------------
-- * Divisors

-- | A proof that @k|n@
data Divides (k :: Nat) (n :: Nat) = Divides 
  { _dividend :: {-# UNPACK #-} !Word64    -- ^ @n@
  , _divisor  :: {-# UNPACK #-} !Word64    -- ^ @k@
  , _quotient :: {-# UNPACK #-} !Word64    -- ^ @q=n/k@
  } 

dividendSNat :: Divides k n -> SNat64 n
dividendSNat (Divides n _ _) = SNat64 n

divisorSNat :: Divides k n -> SNat64 k
divisorSNat (Divides _ k _) = SNat64 k

divides :: SNat64 k -> SNat64 n -> Maybe (Divides k n)
divides (SNat64 k) (SNat64 n) = case divMod n k of
  (q,r) -> if r == 0 then Just (Divides n k q) else Nothing

instance Show (Divides k n) where
  show (Divides n k q) = "(" ++ show k ++ "|" ++ show n ++ ")"

data Divisor (n :: Nat) 
  = forall k. Divisor (Divides k n)

deriving instance Show (Divisor n)

constructDivisor :: SNat64 n -> SNat64 k -> Maybe (Divisor n)
constructDivisor sn sk = case divides sk sn of 
  Nothing -> Nothing
  Just d  -> Just (Divisor d)

divisors :: forall n. SNat64 n -> [Divisor n]
divisors sn@(SNat64 nn) = map worker ds where
    ds = sort (map fromIntegral $ Primes.divisors (fromIntegral nn)) :: [Word64]
    worker :: Word64 -> Divisor n
    worker d = case someSNat64_ d of
     SomeSNat64 sd -> case constructDivisor sn sd of
      Just proof     -> proof
      Nothing        -> error "divisors: fatal error, should not happen"

--------------------------------------------------------------------------------
-- * Proxy

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

proxyOf1 :: f a -> Proxy a
proxyOf1 _ = Proxy

--------------------------------------------------------------------------------
