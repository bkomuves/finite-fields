
-- | Table of Conway polynomials
--
-- The data is from <http://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html>
--

{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, BangPatterns #-}
{-# LANGUAGE GADTs, ExistentialQuantification, DataKinds, KindSignatures #-}

module Math.FiniteField.Conway where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Data.Proxy
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

conwayProxies :: HasConwayPoly p m -> (Proxy p, Proxy m)
conwayProxies _ = (Proxy, Proxy)

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

conwayParams' :: HasConwayPoly p m -> (SNat64 p, SNat64 m)
conwayParams' witness = (SNat64 p, SNat64 (fromIntegral m)) where
  (p,m) = conwayParams witness

conwayCoefficients :: HasConwayPoly p m -> [Word64]
conwayCoefficients (ConwayWitness ptr) = Unsafe.unsafePerformIO $ do
  (_,_,list) <- marshalConwayEntry ptr
  return list

fromConwayPoly :: HasConwayPoly p m -> Ptr Word32
fromConwayPoly (ConwayWitness ptr) = ptr

-- | Usage: @lookupConwayPoly sp sm@ for @q = p^m@
lookupConwayPoly :: SNat64 p -> SNat64 m -> Maybe (HasConwayPoly p m)
lookupConwayPoly !sp !sm = 
  let p = fromIntegral (fromSNat64 sp)
      m = fromIntegral (fromSNat64 sm)
  in case IntMap.lookup (encodePrimeExpo p m) (fromConwayTable theConwayTable) of
       Nothing  -> Nothing
       Just ptr -> Just (ConwayWitness ptr)

-- | Usage: @lookupSomeConwayPoly p m@ for @q = p^m@
lookupSomeConwayPoly :: Int -> Int -> Maybe SomeConwayPoly
lookupSomeConwayPoly !p !m = case (someSNat64 (fromIntegral p) , someSNat64 (fromIntegral m)) of
  (SomeSNat64 sp , SomeSNat64 sm) -> SomeConwayPoly <$> lookupConwayPoly sp sm

-- | We have some Conway polynomials for @m=1@ too; the roots of 
-- these linear polynomials are primitive roots in @F_p@
lookupConwayPrimRoot_ :: Int -> Maybe Word64
lookupConwayPrimRoot_ !p = case IntMap.lookup (encodePrimeExpo p 1) (fromConwayTable theConwayTable) of
  Nothing   -> Nothing
  Just ptr  -> case (Unsafe.unsafePerformIO $ marshalConwayEntry ptr) of
    (_,_,[c,1])  -> Just (fromIntegral p - fromIntegral c)
    _            -> error "lookupConwayPrimRoot: fatal error (should not happen)" 

--------------------------------------------------------------------------------
