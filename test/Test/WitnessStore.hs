
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

import Math.FiniteField.GaloisField.Small as G
import Math.FiniteField.GaloisField.Zech  as Zech

--------------------------------------------------------------------------------

zechfield' :: (KnownNat p, KnownNat m) => Proxy p -> Proxy m -> WitnessZech p m
zechfield' pp pm = lookupZechStore (proxyToSNat64 pp) (proxyToSNat64 pm)

zechfield :: (KnownNat p, KnownNat m) => WitnessZech p m
zechfield = zechfield' Proxy Proxy

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
