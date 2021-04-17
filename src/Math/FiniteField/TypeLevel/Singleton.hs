
-- | Singleton types.
--
-- This is a separate (internal) module, because sometimes we need to create 
-- singletons in other modules than "Math.FiniteField.TypeLevel"
--

{-# LANGUAGE DataKinds, KindSignatures, GADTs, ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.FiniteField.TypeLevel.Singleton where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word

import GHC.TypeNats
import Data.Proxy

--------------------------------------------------------------------------------
-- * Singleton types

-- | Nat-singletons
newtype SNat (n :: Nat) 
  = SNat Integer
  deriving Show

proxyOfSNat :: SNat n -> Proxy n
proxyOfSNat _ = Proxy

fromSNat :: SNat n -> Integer
fromSNat (SNat n) = n

proxyToSNat :: KnownNat n => Proxy n -> SNat n
proxyToSNat proxy = SNat (fromIntegral (natVal proxy))

-- | Word-sized nat-singletons
newtype SNat64 (n :: Nat) 
  = SNat64 Word64
  deriving Show

proxyOfSNat64 :: SNat64 n -> Proxy n
proxyOfSNat64 _ = Proxy

fromSNat64 :: SNat64 n -> Word64
fromSNat64 (SNat64 n) = n

proxyToSNat64 :: KnownNat n => Proxy n -> SNat64 n
proxyToSNat64 proxy = SNat64 (fromIntegral (natVal proxy))

--------------------------------------------------------------------------------
-- * Creating singleton types

data SomeSNat 
  = forall (n :: Nat). KnownNat n => SomeSNat (SNat n)

deriving instance Show SomeSNat

someSNat :: Integer -> SomeSNat
someSNat n 
  | n < 0     = error "someSNat: expecting a nonnegative number"
  | otherwise = case someNatVal (fromIntegral n) of
      SomeNat proxy -> SomeSNat (proxyToSNat proxy)

data SomeSNat64
  = forall (n :: Nat). KnownNat n => SomeSNat64 (SNat64 n)

deriving instance Show SomeSNat64

someSNat64 :: Int64 -> SomeSNat64
someSNat64 n 
  | n < 0     = error "someSNat64: expecting a nonnegative number"
  | otherwise = case someNatVal (fromIntegral n) of
      SomeNat proxy -> SomeSNat64 (proxyToSNat64 proxy)

someSNat64_ :: Word64 -> SomeSNat64
someSNat64_ n = case someNatVal (fromIntegral n) of
  SomeNat proxy -> SomeSNat64 (proxyToSNat64 proxy)

--------------------------------------------------------------------------------
-- * sanity checking

checkSomeSNat :: SomeSNat -> String
checkSomeSNat some = case some of
  SomeSNat snat -> case ( snat , natVal (proxyOfSNat snat) ) of
    (SNat value , tyval) -> "[" ++ show value ++ "=" ++ show tyval ++ "]"

checkSomeSNat64 :: SomeSNat64 -> String
checkSomeSNat64 some = case some of
  SomeSNat64 snat -> case ( snat , natVal (proxyOfSNat64 snat) ) of
    (SNat64 value , tyval) -> "[" ++ show value ++ "=" ++ show tyval ++ "]"


--------------------------------------------------------------------------------
   