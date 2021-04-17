
-- | Type level naturals, singletons, prime witnesses and stuff.
--
-- This would be much simpler in a dependently typed language;
-- alas, we are in Haskell, so for now we have to feel the pain.
--
-- How to use this:
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

{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE GADTs, ExistentialQuantification, StandaloneDeriving #-}

module Math.FiniteField.TypeLevel
  ( -- * Singleton types
    SNat   , fromSNat   , proxyOfSNat   , proxyToSNat   
  , SNat64 , fromSNat64 , proxyOfSNat64 , proxyToSNat64
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

import GHC.TypeNats
import Data.Proxy

import Math.FiniteField.Primes
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
isPrime sn = if (fromSNat sn > 1) && isPrimeTrialDivision (fromSNat sn)
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
  if (n > 1) && (n < 2^31) && isPrimeTrialDivision (fromIntegral n)
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
-- * Proxy

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

proxyOf1 :: f a -> Proxy a
proxyOf1 _ = Proxy

--------------------------------------------------------------------------------
