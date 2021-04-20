
-- | Table of Conway polynomials
--
-- The data is from <http://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html>
--

{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, BangPatterns #-}
{-# LANGUAGE GADTs, ExistentialQuantification, DataKinds, KindSignatures #-}

module Math.FiniteField.Conway 
  ( ConwayPoly
  , SomeConwayPoly(..)
  , conwayPrime  , conwayDim
  , conwayParams , conwayParams'
  , conwayCoefficients
  , lookupSomeConwayPoly   
  , lookupConwayPoly
  , unsafeLookupConwayPoly 
  , lookupConwayPrimRoot
  )
  where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Data.Proxy

import GHC.TypeNats (Nat)

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

data SomeConwayPoly = forall p m. SomeConwayPoly (ConwayPoly p m) 

deriving instance Show SomeConwayPoly

conwayProxies :: ConwayPoly p m -> (Proxy p, Proxy m)
conwayProxies _ = (Proxy, Proxy)

instance Show (ConwayPoly p m) where
  show witness = "ConwayPoly[" ++ show p ++ "^" ++ show m ++ "]" where
    (p,m) = conwayParams_ witness

-- | The prime characteristic @p@
conwayPrime :: ConwayPoly p m -> IsSmallPrime p
conwayPrime (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (p,_) <- getConwayEntryParams ptr
  return (believeMeItsASmallPrime (SNat64 p))

-- | The dimension @m@ of @F_q@ over @F_p@
conwayDim :: ConwayPoly p m -> Int
conwayDim (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (_,m) <- getConwayEntryParams ptr
  return (fromIntegral m)

-- | The pair @(p,m)@
conwayParams :: ConwayPoly p m -> (Int,Int)
conwayParams witness = (fromIntegral p, fromIntegral m) where
  (p,m) = conwayParams_ witness

conwayParams' :: ConwayPoly p m -> (SNat64 p, SNat64 m)
conwayParams' witness = (SNat64 p, SNat64 (fromIntegral m)) where
  (p,m) = conwayParams_ witness

conwayCoefficients :: ConwayPoly p m -> [Word64]
conwayCoefficients (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (_,_,list) <- marshalConwayEntry ptr
  return list

-- | Usage: @lookupConwayPoly sp sm@ for @q = p^m@
lookupConwayPoly :: SNat64 p -> SNat64 m -> Maybe (ConwayPoly p m)
lookupConwayPoly !sp !sm = 
  let p = fromIntegral (fromSNat64 sp)
      m = fromIntegral (fromSNat64 sm)
  in case IntMap.lookup (encodePrimeExpo p m) (fromConwayTable theConwayTable) of
       Nothing  -> Nothing
       Just ptr -> Just (ConwayWitness ptr)

unsafeLookupConwayPoly :: SNat64 p -> SNat64 m -> ConwayPoly p m
unsafeLookupConwayPoly sp sm = case lookupConwayPoly sp sm of
  Nothing  -> error "unsafeLookupConwayPoly: Conway polynomial not found"
  Just cw  -> cw

-- | Usage: @lookupSomeConwayPoly p m@ for @q = p^m@
lookupSomeConwayPoly :: Int -> Int -> Maybe SomeConwayPoly
lookupSomeConwayPoly !p !m = case (someSNat64 (fromIntegral p) , someSNat64 (fromIntegral m)) of
  (SomeSNat64 sp , SomeSNat64 sm) -> SomeConwayPoly <$> lookupConwayPoly sp sm

-- | We have some Conway polynomials for @m=1@ too; the roots of 
-- these linear polynomials are primitive roots in @F_p@
lookupConwayPrimRoot :: Int -> Maybe Int
lookupConwayPrimRoot p = fromIntegral <$> (lookupConwayPrimRoot_ p)

--------------------------------------------------------------------------------
