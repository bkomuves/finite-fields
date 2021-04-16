
-- | Table of Conway polynomials
--
-- The data is from <http://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html>
--

{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving #-}
{-# LANGUAGE GADTs, ExistentialQuantification, DataKinds, KindSignatures #-}

module Math.FiniteField.Conway where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import GHC.TypeNats

import qualified Data.IntMap.Strict as IntMap

--import Foreign.C
import Foreign.Ptr
--import Foreign.Storable
--import Foreign.Marshal
--import Foreign.Marshal.Array

import qualified System.IO.Unsafe as Unsafe

import Math.FiniteField.TypeLevel
import Math.FiniteField.TypeLevel.Singleton
import Math.FiniteField.Conway.Internal

-- import Data.Array
-- import Math.FiniteField.PrimeField.Small

--------------------------------------------------------------------------------

-- data ConwayPoly p = ConwayPoly
--   { _prime    :: !(IsPrime p)
--   , _exponent :: !Int
--   , _coeffs   :: !(Array Int (Fp p))
--   }

--------------------------------------------------------------------------------
-- * Witness for the existence of precomputed Conway polynomials

data SomeConwayPoly = forall p m. SomeConwayPoly (HasConwayPoly p m) 

deriving instance Show SomeConwayPoly

newtype HasConwayPoly (p :: Nat) (m :: Nat) where
  ConwayWitness :: Ptr Word32 -> HasConwayPoly p m

instance Show (HasConwayPoly p m) where
  show witness = "ConwayPoly[" ++ show p ++ "^" ++ show m ++ "]" where
    (p,m) = conwayParams witness

-- | The prime characteristic @p@
conwayPrime :: HasConwayPoly p m -> IsSmallPrime p
conwayPrime (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (p,_) <- getConwayEntryParams ptr
  return (believeMeItsASmallPrime (SNat64 p))

conwayPrime_ :: HasConwayPoly p m -> Word64
conwayPrime_ (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (p,_) <- getConwayEntryParams ptr
  return p

-- | The dimension @m@ of @F_q@ over @F_p@
conwayDim :: HasConwayPoly p m -> Int
conwayDim (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (_,m) <- getConwayEntryParams ptr
  return (fromIntegral m)

-- | @(prime,exponent)@
conwayParams :: HasConwayPoly p m -> (Word64,Int)
conwayParams (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (p,m) <- getConwayEntryParams ptr
  return (fromIntegral p, fromIntegral m)

conwayCoefficients :: HasConwayPoly p m -> [Word64]
conwayCoefficients (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (_,_,list) <- marshalConwayEntry ptr
  return list

fromConwayPoly :: HasConwayPoly p m -> Ptr Word32
fromConwayPoly (ConwayWitness ptr) = ptr

-- | Usage: @lookupConwayPoly p m@ for @q = p^m@
lookupConwayPoly :: Int -> Int -> Maybe SomeConwayPoly
lookupConwayPoly p m = case IntMap.lookup (encodePrimeExpo p m) (fromConwayTable theConwayTable) of
  Nothing  -> Nothing
  Just ptr -> Just (SomeConwayPoly (ConwayWitness ptr))

--------------------------------------------------------------------------------
