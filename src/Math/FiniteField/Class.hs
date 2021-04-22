
-- | Type class interface to different implementations of finite fields

{-# LANGUAGE BangPatterns, FlexibleContexts, TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

module Math.FiniteField.Class where

--------------------------------------------------------------------------------

import Data.Bits
import Data.List

import GHC.TypeNats (Nat)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Random ( RandomGen )

import Math.FiniteField.TypeLevel
import Math.FiniteField.TypeLevel.Singleton

--------------------------------------------------------------------------------
-- * Fields

{-
-- NOTE: recursive classes just cause problems

-- | A class for witness types (these witness the existence of a field)
class 
  ( Show w, Field (FieldElem w)
  , WitnessPrime w ~ Prime (FieldElem w) 
  , WitnessDim   w ~ Dim   (FieldElem w) 
  ) => FieldWitness w 
  where
    type FieldElem    w = f | f -> w
    type WitnessPrime w :: Nat
    type WitnessDim   w :: Nat
-}

-- | A class for field element types 
class (Eq f, Ord f, Show f, Num f, Fractional f, Show (Witness f)) => Field f where
    -- | witness for the existence of the field (this is an injective type family!) 
    type Witness f = w | w -> f   
    -- | the characteristic at type level
    type Prime f :: Nat
    -- | the dimension at type level
    type Dim f :: Nat              
    -- | the prime characteristic
    characteristic   :: Witness f -> Integer   
    -- | dimension over the prime field (the exponent @m@ in @q=p^m@)
    dimension        :: Witness f -> Integer    -- TODO: this should be Int
    -- | the size (or order) of the field
    fieldSize        :: Witness f -> Integer   
    -- | The additive identity of the field
    zero             :: Witness f -> f
    -- | The multiplicative identity of the field
    one              :: Witness f -> f
    -- | check for equality with the additive identity
    isZero           :: f -> Bool
    -- | check for equality with the multiplicative identity
    isOne            :: f -> Bool
    -- | an element of the prime field
    embed            :: Witness f -> Integer -> f         
    embedSmall       :: Witness f -> Int     -> f   
    -- | a uniformly random field element
    randomFieldElem  :: RandomGen gen => Witness f -> gen -> (f,gen) 
    -- | a random invertible element
    randomInvertible :: RandomGen gen => Witness f -> gen -> (f,gen) 
    -- | a primitive generator
    primGen          :: Witness f -> f                    
    -- | extract t  he witness from a field element
    witnessOf        :: f -> Witness f                    
    -- | exponentiation 
    power            :: f -> Integer -> f                 
    powerSmall       :: f -> Int     -> f            
    -- | Frobenius automorphism @x -> x^p@
    frobenius        :: f -> f
    -- | list of field elements (of course it's only useful for very small fields)
    enumerate        :: Witness f -> [f]                  
  
    -- default implementations
  
    embedSmall !w !x = embed w (fromIntegral x)
    powerSmall !x !e = power x (fromIntegral e)      -- it's important not to use powerDefault here, because 'power' may be more efficient
    fieldSize  !w    = characteristic w ^ dimension w
    power            = powerDefault 
    frobenius     !x = power x (characteristic (witnessOf x))
  
    zero       !w    = embedSmall w 0
    one        !w    = embedSmall w 1
    -- isZero     !x    = (x == zero w)   -- we don't have a witness available here...
    -- isOne      !x    = (x == one  w)
  
    randomInvertible !w !g = case randomFieldElem w g of 
      (x,g') -> if isZero x then randomInvertible w g' else (x,g')

fieldPrimeSNat :: Field f => Witness f -> SNat (Prime f)
fieldPrimeSNat w = SNat (characteristic w)

fieldPrimeSNat64 :: Field f => Witness f -> SNat64 (Prime f)
fieldPrimeSNat64 w = SNat64 (fromInteger $ characteristic w)

fieldDimSNat :: Field f => Witness f -> SNat (Dim f)
fieldDimSNat w = SNat (dimension w)

fieldDimSNat64 :: Field f => Witness f -> SNat64 (Dim f)
fieldDimSNat64 w = SNat64 (fromInteger $ dimension w)

--------------------------------------------------------------------------------

data SomeField 
  = forall f. Field f => SomeField (Witness f)

deriving instance Show SomeField

--------------------------------------------------------------------------------

-- -- * Subfields
-- 
-- class 
--   ( FieldWitness (AmbientWitness w)
--   , FieldWitness (SubWitness     w) 
--   ) => SubFieldWitness w 
--   where
--     type AmbientWitness w :: *
--     type SubWitness     w :: *
-- 
-- class (Field (AmbientField s) , Field (SubField s)) => SubField s where
--   type AmbientField s :: *
--   type SubField     s :: *
--   type SubFieldWitness 
--   embedSubField :: SubField s -> AmbientField s 
--   mbSubField    :: AmbientField s -> Maybe (SubField s)
--   isSubField    :: AmbientField s -> Bool
--   coordinates   :: AmbientField s -> [SubField s]

--------------------------------------------------------------------------------
-- * Some generic functions

-- | Returns @"GF(p)"@ or @"GF(p^m)"
fieldName :: Field f => Witness f -> String
fieldName w 
  | m == 1     = "GF(" ++ show p ++ ")"
  | otherwise  = "GF(" ++ show p ++ "^" ++ show m ++ ")"
  where
    p = characteristic w
    m = dimension      w

-- | The multiplicate inverse (synonym for 'recip')
inverse :: Field f => f -> f
inverse = recip

-- | Enumerate the elements of the prime field only 
enumPrimeField :: forall f. Field f => Witness f -> [f]
enumPrimeField w = [ embedSmall w i | i<-[0..p-1] ] where
  pbig = characteristic w
  p    = fromIntegral pbig :: Int

-- | The nonzero elements in cyclic order, starting from the primitive generator
-- (of course this is only useful for very small fields)
multGroup :: Field f => Witness f -> [f]    
multGroup w = scanl1 (*) list where
  g    = primGen w
  m    = fieldSize w
  list = replicate (fromIntegral m - 1) g

--------------------------------------------------------------------------------

-- | Computes a table of discrete logarithms with respect to the primitive 
-- generator. Note: zero (that is, the additive identitiy of the field) 
-- is not present in the resulting map.
discreteLogTable :: forall f. Field f => Witness f -> Map f Int
discreteLogTable witness = Map.fromList (worker 0 (one witness)) where
  g = primGen   witness
  q = fieldSize witness
  qm1 = fromInteger q - 1
  worker :: Int -> f -> [(f,Int)]
  worker !e !acc
    | e < qm1    = (acc,e) : worker (e+1) (acc*g)
    | otherwise  = []

--------------------------------------------------------------------------------

-- | Generic exponentiation
powerDefault :: forall f. (Field f) => f -> Integer -> f
powerDefault !z !e 
  | isZero z  = z 
  | e == 0    = one w
  | e < 0     = powerDefault (recip z) (negate e)
  | e >= pm1  = go (one w) z (mod e pm1)
  | otherwise = go (one w) z e
  where
    w   = witnessOf z
    pm1 = fieldSize w - 1
    go :: f -> f -> Integer -> f
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go  acc    (y*y) (shiftR e 1)
        _ -> go (acc*y) (y*y) (shiftR e 1)

--------------------------------------------------------------------------------
