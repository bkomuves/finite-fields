
-- | Testing dependently typed stuff with QuickCheck is annoyingly hard...
--
-- Here we dynamically generate and cache witnesses for fields
-- (especially Zech implementation does some precalculation...)
--

module Test.WitnessStore where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Concurrent
import System.IO.Unsafe as Unsafe

import Data.Proxy
import GHC.TypeNats
import Unsafe.Coerce

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Math.FiniteField.Class
import Math.FiniteField.Conway
import Math.FiniteField.TypeLevel

import Math.FiniteField.PrimeField.Generic as Z
import Math.FiniteField.PrimeField.Small   as S
import Math.FiniteField.GaloisField.Small  as G
import Math.FiniteField.GaloisField.Zech   as Zech
import Math.FiniteField.GaloisField.Zech.C as C

--------------------------------------------------------------------------------
-- * easy lookups (to be used with type application)

genericField :: (KnownNat p) => Z.WitnessFp p
genericField = genericField' Proxy

genericField' :: (KnownNat p) => Proxy p -> Z.WitnessFp p
genericField' pp = lookupGenericStore (proxyToSNat pp) 

smallField :: (KnownNat p) => S.WitnessFp p
smallField = smallField' Proxy

smallField' :: (KnownNat p) => Proxy p -> S.WitnessFp p
smallField' pp = lookupSmallStore (proxyToSNat64 pp) 

galoisField :: (KnownNat p, KnownNat m) => WitnessGF p m
galoisField = galoisField' Proxy Proxy

galoisField' :: (KnownNat p, KnownNat m) => Proxy p -> Proxy m -> WitnessGF p m
galoisField' pp pm = lookupGaloisStore (proxyToSNat64 pp) (proxyToSNat64 pm)

zechField :: (KnownNat p, KnownNat m) => WitnessZech p m
zechField = zechField' Proxy Proxy

zechField' :: (KnownNat p, KnownNat m) => Proxy p -> Proxy m -> WitnessZech p m
zechField' pp pm = lookupZechStore (proxyToSNat64 pp) (proxyToSNat64 pm)

cField :: (KnownNat p, KnownNat m) => WitnessC p m
cField = cField' Proxy Proxy

cField' :: (KnownNat p, KnownNat m) => Proxy p -> Proxy m -> WitnessC p m
cField' pp pm = lookupCStore (proxyToSNat64 pp) (proxyToSNat64 pm)

--------------------------------------------------------------------------------
-- * stores

{-# NOINLINE theGaloisStore #-}
theGaloisStore :: MVar (Map (Int,Int) SomeWitnessGF)
theGaloisStore = Unsafe.unsafePerformIO $ newMVar Map.empty

coerceWitnessGF :: WitnessGF p1 m1 -> WitnessGF p2 m2
coerceWitnessGF = unsafeCoerce

{-# NOINLINE lookupGaloisStore #-}
lookupGaloisStore :: SNat64 p -> SNat64 m -> WitnessGF p m
lookupGaloisStore sp sm = Unsafe.unsafePerformIO $ do
  some <- lookupGaloisStoreIO (fromIntegral $ fromSNat64 sp) (fromIntegral $ fromSNat64 sm)
  return $ case some of { SomeWitnessGF w -> coerceWitnessGF w }  

lookupGaloisStoreIO :: Int -> Int -> IO SomeWitnessGF
lookupGaloisStoreIO p m = do
  table <- readMVar theGaloisStore
  case Map.lookup (p,m) table of
    Just some -> return some
    Nothing   -> do
      let mb = mkGaloisField p m
      case mb of
        Nothing   -> error $ "lookupGaloisStoreIO: " ++ show (p,m)
        Just some -> do
          old <- takeMVar theGaloisStore
          putMVar theGaloisStore $! Map.insert (p,m) some old
          return some

--------------------------------------------------------------------------------

{-# NOINLINE theZechStore #-}
theZechStore :: MVar (Map (Int,Int) SomeWitnessZech)
theZechStore = Unsafe.unsafePerformIO $ newMVar Map.empty

coerceWitnessZech :: WitnessZech p1 m1 -> WitnessZech p2 m2
coerceWitnessZech = unsafeCoerce

{-# NOINLINE lookupZechStore #-}
lookupZechStore :: SNat64 p -> SNat64 m -> WitnessZech p m
lookupZechStore sp sm = Unsafe.unsafePerformIO $ do
  some <- lookupZechStoreIO (fromIntegral $ fromSNat64 sp) (fromIntegral $ fromSNat64 sm)
  return $ case some of { SomeWitnessZech w -> coerceWitnessZech w }  

lookupZechStoreIO :: Int -> Int -> IO SomeWitnessZech
lookupZechStoreIO p m = do
  table <- readMVar theZechStore
  case Map.lookup (p,m) table of
    Just some -> return some
    Nothing   -> do
      let mb = mkZechField p m
      case mb of
        Nothing   -> error $ "lookupZechStoreIO: " ++ show (p,m)
        Just some -> do
          old <- takeMVar theZechStore
          putMVar theZechStore $! Map.insert (p,m) some old
          return some

--------------------------------------------------------------------------------

{-# NOINLINE theCStore #-}
theCStore :: MVar (Map (Int,Int) SomeWitnessC)
theCStore = Unsafe.unsafePerformIO $ newMVar Map.empty

coerceWitnessC :: WitnessC p1 m1 -> WitnessC p2 m2
coerceWitnessC = unsafeCoerce

{-# NOINLINE lookupCStore #-}
lookupCStore :: SNat64 p -> SNat64 m -> WitnessC p m
lookupCStore sp sm = Unsafe.unsafePerformIO $ do
  some <- lookupCStoreIO (fromIntegral $ fromSNat64 sp) (fromIntegral $ fromSNat64 sm)
  return $ case some of { SomeWitnessC w -> coerceWitnessC w }  

lookupCStoreIO :: Int -> Int -> IO SomeWitnessC
lookupCStoreIO p m = do
  table <- readMVar theCStore
  case Map.lookup (p,m) table of
    Just some -> return some
    Nothing   -> do
      let mb = mkCField p m
      case mb of
        Nothing   -> error $ "lookupCStoreIO: " ++ show (p,m)
        Just some -> do
          old <- takeMVar theCStore
          putMVar theCStore $! Map.insert (p,m) some old
          return some

--------------------------------------------------------------------------------

{-# NOINLINE theGenericStore #-}
theGenericStore :: MVar (Map Integer Z.SomeWitnessFp)
theGenericStore = Unsafe.unsafePerformIO $ newMVar Map.empty

coerceWitnessFp :: Z.WitnessFp p1 -> Z.WitnessFp p2 
coerceWitnessFp = unsafeCoerce

{-# NOINLINE lookupGenericStore #-}
lookupGenericStore :: SNat p -> Z.WitnessFp p 
lookupGenericStore sp = Unsafe.unsafePerformIO $ do
  some <- lookupGenericStoreIO (fromSNat sp) 
  return $ case some of { Z.SomeWitnessFp w -> coerceWitnessFp w }  

lookupGenericStoreIO :: Integer -> IO Z.SomeWitnessFp
lookupGenericStoreIO p = do
  table <- readMVar theGenericStore
  case Map.lookup (p) table of
    Just some -> return some
    Nothing   -> do
      let mb = mkPrimeField p
      case mb of
        Nothing   -> error $ "lookupGenericStoreIO: " ++ show p
        Just some -> do
          old <- takeMVar theGenericStore
          putMVar theGenericStore $! Map.insert (p) some old
          return some

--------------------------------------------------------------------------------

{-# NOINLINE theSmallStore #-}
theSmallStore :: MVar (Map Int S.SomeWitnessFp)
theSmallStore = Unsafe.unsafePerformIO $ newMVar Map.empty

coerceWitnessSmallFp :: S.WitnessFp p1 -> S.WitnessFp p2 
coerceWitnessSmallFp = unsafeCoerce

{-# NOINLINE lookupSmallStore #-}
lookupSmallStore :: SNat64 p -> S.WitnessFp p 
lookupSmallStore sp = Unsafe.unsafePerformIO $ do
  some <- lookupSmallStoreIO (fromIntegral $ fromSNat64 sp) 
  return $ case some of { S.SomeWitnessFp w -> coerceWitnessSmallFp w }  

lookupSmallStoreIO :: Int -> IO S.SomeWitnessFp
lookupSmallStoreIO p = do
  table <- readMVar theSmallStore
  case Map.lookup (p) table of
    Just some -> return some
    Nothing   -> do
      let mb = mkSmallPrimeField p
      case mb of
        Nothing   -> error $ "lookupSmallStoreIO: " ++ show p
        Just some -> do
          old <- takeMVar theSmallStore
          putMVar theSmallStore $! Map.insert (p) some old
          return some

--------------------------------------------------------------------------------
