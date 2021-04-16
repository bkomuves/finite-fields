
-- | Small Galois fields via a precomputed table of Conway polynomials.
--
-- This covers:
--
-- * all fields with order <= 2^30
--
-- * all fields with primes < 2^16 and order < 2^64 (?)
--
-- * higher powers for very small primes
--
-- * some more
--
-- To look up Conway polynomials, see the module "Math.FiniteField.Conway".


{-# LANGUAGE BangPatterns, DataKinds, KindSignatures #-}
module Math.FiniteField.Galois.Small where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word
import GHC.TypeNats (Nat)

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel
import Math.FiniteField.Conway
import Math.FiniteField.PrimeField.Small

--------------------------------------------------------------------------------  

data Fq (p :: Nat) (m :: Nat)
  = Fq {-# UNPACK #-} !(HasConwayPoly p m) !(Vector Word32)

--------------------------------------------------------------------------------  

-- instance Show (Fq p m) where
--   show (Fq witness vector) = "<" ++ intercalate " + " (map f )
