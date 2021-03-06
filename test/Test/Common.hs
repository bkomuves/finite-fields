
-- | Stuff needed in more than one place

{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts, Rank2Types #-}

module Test.Common where

--------------------------------------------------------------------------------

import System.Random ( RandomGen )

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Test.Tasty
import Test.Tasty.QuickCheck  hiding ( NonZero )
import Test.QuickCheck        hiding ( NonZero )

import Math.FiniteField.Class

--------------------------------------------------------------------------------

mkGen :: (forall g. RandomGen g => (g -> (a,g))) -> Gen a
mkGen f = MkGen (\r _ -> let (x,_) = f r in x)

--------------------------------------------------------------------------------
-- fuck this shit


forAll1 :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll1 = Test.Tasty.QuickCheck.forAll

forAll2 :: (Show a, Show b, Testable prop) => Gen a -> Gen b -> (a -> b -> prop) -> Property
forAll2 gen1 gen2 f = Test.Tasty.QuickCheck.forAll gen12 $ \(x,y) -> f x y where 
  gen12 = (,) <$> gen1 <*> gen2

forAll3 :: (Show a, Show b, Show c, Testable prop) => Gen a -> Gen b -> Gen c -> (a -> b -> c -> prop) -> Property
forAll3 gen1 gen2 gen3 f = Test.Tasty.QuickCheck.forAll gen123 $ \(x,y,z) -> f x y z where 
  gen123 = (,,) <$> gen1 <*> gen2 <*> gen3

--------------------------------------------------------------------------------

newtype NonZero f 
  = NonZero f
  deriving (Eq,Show)

class Field f => RandomField f where
  unaryProp     :: Witness f -> (f                           -> Bool) -> Property
  unaryPropNZ   :: Witness f -> (NonZero f                   -> Bool) -> Property
  binaryProp    :: Witness f -> (f         -> f              -> Bool) -> Property
  binaryPropI   :: Witness f -> (f         -> Int            -> Bool) -> Property
  binaryPropNZ  :: Witness f -> (f         -> NonZero f      -> Bool) -> Property
  ternaryProp   :: Witness f -> (f         -> f         -> f -> Bool) -> Property

--------------------------------------------------------------------------------

nullary :: RandomField f => (Witness f -> Bool) -> Witness f -> Property
nullary f field = property (f field)

unary :: RandomField f => (Witness f -> f -> Bool) -> Witness f -> Property
unary f field = unaryProp field (f field)

unaryI :: RandomField f => (Witness f -> Int -> Bool) -> Witness f -> Property
unaryI f field = property (f field)

unaryNZ :: RandomField f => (Witness f -> NonZero f -> Bool) -> Witness f -> Property
unaryNZ f field = unaryPropNZ field (f field)

binary :: RandomField f => (Witness f -> f -> f -> Bool) -> Witness f -> Property
binary f field = binaryProp field (f field)

binaryI :: RandomField f => (Witness f -> f -> Int -> Bool) -> Witness f -> Property
binaryI f field = binaryPropI field (f field)

binaryNZ :: RandomField f => (Witness f -> f -> NonZero f -> Bool) -> Witness f -> Property
binaryNZ f field = binaryPropNZ field (f field)

ternary :: RandomField f => (Witness f -> f -> f -> f -> Bool) -> Witness f -> Property
ternary f field = ternaryProp field (f field)

--------------------------------------------------------------------------------

{-
-- hacking around newtypes and instances...

nullary :: (b -> a) -> (witness -> result) -> (witness -> result)
nullary unwrap f w = f w  

unary :: (b -> a) -> (witness -> a -> result) -> (witness -> b -> result)
unary unwrap f w x = f w (unwrap x) 

unaryNZ :: (b -> a) -> (witness -> NonZero a -> result) -> (witness -> NonZero b -> result)
unaryNZ unwrap f w (NonZero x) = f w (NonZero $ unwrap x)

unaryI :: (b -> a) -> (witness -> Int -> result) -> (witness -> Int -> result)
unaryI unwrap f w k = f w k

binary :: (b -> a) -> (witness -> a -> a -> result) -> (witness -> b -> b -> result)
binary unwrap f w x y = f w (unwrap x) (unwrap y) 

binaryNZ :: (b -> a) -> (witness -> a -> NonZero a -> result) -> (witness -> b -> NonZero b -> result)
binaryNZ unwrap f w x (NonZero y) = f w (unwrap x) (NonZero $ unwrap y) 

ternary :: (b -> a) -> (witness -> a -> a -> a -> result) -> (witness -> b -> b -> b -> result)
ternary unwrap f w x y z = f w (unwrap x) (unwrap y) (unwrap z)

binaryI :: (b -> a) -> (witness -> a -> Int -> result) -> (witness -> b -> Int -> result)
binaryI unwrap f w x k = f w (unwrap x) k
-}

--------------------------------------------------------------------------------
