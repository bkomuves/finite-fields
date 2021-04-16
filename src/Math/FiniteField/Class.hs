
-- | Type class interface to different implementations of finite fields

{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Math.FiniteField.Class where

--------------------------------------------------------------------------------

import Data.List
import Data.Proxy

import Math.FiniteField.TypeLevel (proxyOf)

--------------------------------------------------------------------------------

class (Eq f, Show f, Num f, Fractional f) => Field f where
  -- | witness for the existence of the field 
  type Witness f :: *                                 
  -- | the prime characteristic
  characteristic :: Proxy f -> Witness f -> Integer   
  -- | dimension over the prime field (the exponent @m@ in @q=p^m@)
  dimension      :: Proxy f -> Witness f -> Integer   
  -- | the size (or order) of the field
  fieldSize      :: Proxy f -> Witness f -> Integer   
  -- | list of field elements (of course it's only useful for very small fields)
  enumerate      :: Witness f -> [f]                  
  -- | an element of the prime field
  embed          :: Witness f -> Integer -> f         
  embedSmall     :: Witness f -> Int     -> f    
  -- | a primitive generator
  primGen        :: Witness f -> f                    
  -- | extract the witness from a field element
  witnessOf      :: f -> Witness f                    
  -- | exponentiation 
  power          :: f -> Integer -> f                 
  powerSmall     :: f -> Int     -> f            

  -- default implementations
  embedSmall w x = embed w (fromIntegral x)
  powerSmall x e = power x (fromIntegral e)
  fieldSize pxy w = characteristic pxy w ^ dimension pxy w

--------------------------------------------------------------------------------

zero :: Field f => Witness f -> f
zero w = embedSmall w 0

one :: Field f => Witness f -> f
one w = embedSmall w 1

inverse :: Field f => f -> f
inverse = recip

-- | Enumerate the elements of the prime field only 
enumPrimeField :: forall f. Field f => Witness f -> [f]
enumPrimeField w = [ embedSmall w i | i<-[0..p-1] ] where
  pxy  = proxyOf (zero w :: f)
  pbig = characteristic pxy w
  p    = fromIntegral pbig :: Int

-- | The nonzero elements in cyclic order, starting from the primitive generator
-- (of course it's only useful for very small fields)
multGroup :: Field f => Witness f -> [f]    
multGroup w = scanl1 (*) list where
  g    = primGen w
  pxy  = proxyOf g
  m    = fieldSize pxy w
  list = replicate (fromIntegral m - 1) g

--------------------------------------------------------------------------------
