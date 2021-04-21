
-- | Tests for subfields
--

{-# LANGUAGE DataKinds, Rank2Types, TypeApplications #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Test.SubFields where

--------------------------------------------------------------------------------

import System.Random ( RandomGen )

import Data.Proxy
import GHC.TypeNats
import Unsafe.Coerce

import Test.Tasty
import Test.Tasty.QuickCheck hiding ( NonZero )

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Math.FiniteField.Class
import Math.FiniteField.Conway
import Math.FiniteField.TypeLevel

import Math.FiniteField.GaloisField.Small as G
import Math.FiniteField.GaloisField.Zech  as Zech

import Test.Common
import Test.WitnessStore


--------------------------------------------------------------------------------

zech_subfieldTests :: TestTree
zech_subfieldTests = testGroup "Subfields (Zech)" (map someSubFieldProps ambient_zechfields)

data TestAmbientField = forall p m. TestAmbientField (Witness (Zech p m))

someSubFieldProps :: (String,TestAmbientField) -> TestTree
someSubFieldProps (name,somefield) = 
  case somefield of
    TestAmbientField field -> 
      testGroup 
        ("subfields of the ambient field " ++ name) 
        (map worker (enumerateSubFields field))
  where
    worker :: forall p m. SomeSubField p m -> TestTree 
    worker somesub = case somesub of
      SomeSubField subfield -> subFieldTestGroup subfield

subFieldTestGroup :: SubField p m k -> TestTree 
subFieldTestGroup subfield = subFieldProperties text subfield where
  text = ("properties of the subfield " ++ fieldName (subFieldWitness subfield) ++
                                 " in " ++ fieldName (ambientWitness  subfield)) 

subFieldProperties :: forall p m k. String -> SubField p m k -> TestTree
subFieldProperties groupName subfield = testGroup groupName 
  [ testProperty "project . embed = id"                  (x_unary    prop_embed_project        subfield)
  , testProperty "embed is in subfield"                  (x_unary    prop_embed_is_subfield    subfield)
  , testProperty "embed 0 = 0"                           (x_nullary  prop_embed_iso_zero       subfield)
  , testProperty "embed 1 = 1"                           (x_nullary  prop_embed_iso_one        subfield)
  , testProperty "embed k = k"                           (x_unaryI   prop_embed_iso_primefield subfield)
  , testProperty "embed respects negation"               (x_unary    prop_embed_iso_neg        subfield)           
  , testProperty "embed respects addition"               (x_binary   prop_embed_iso_add        subfield)            
  , testProperty "embed respects subtraction"            (x_binary   prop_embed_iso_sub        subfield)              
  , testProperty "embed respects inverse"                (x_unaryNZ  prop_embed_iso_inv        subfield)               
  , testProperty "embed respects multiplication"         (x_binary   prop_embed_iso_mul        subfield)             
  , testProperty "embed respects division"               (x_binaryNZ prop_embed_iso_div        subfield)             
  ]

--------------------------------------------------------------------------------

prop_embed_project :: SubField p m k -> Zech p k -> Bool
prop_embed_project subfield x = projectSubField subfield (embedSubField subfield x) == Just x

prop_embed_is_subfield :: SubField p m k -> Zech p k -> Bool
prop_embed_is_subfield subfield x = isInSubField subfield (embedSubField subfield x) 

-- this does not fit into the pattern of the rest :(
prop_project_vs_check :: SubField p m k -> Zech p m -> Bool
prop_project_vs_check subfield x = case projectSubField subfield x of
  Nothing -> not (isInSubField subfield x)
  Just _  ->      isInSubField subfield x

prop_embed_iso_zero :: SubField p m k -> Bool
prop_embed_iso_zero subfield = embedSubField subfield (zero (subFieldWitness subfield)) == zero (ambientWitness subfield)

prop_embed_iso_one :: SubField p m k -> Bool
prop_embed_iso_one subfield = embedSubField subfield (one (subFieldWitness subfield)) == one (ambientWitness subfield)

prop_embed_iso_primefield :: SubField p m k -> Int -> Bool
prop_embed_iso_primefield subfield k = 
  embedSubField subfield (embedSmall (subFieldWitness subfield) k) == embedSmall (ambientWitness subfield) k

prop_embed_iso_neg :: SubField p m k -> Zech p k -> Bool
prop_embed_iso_neg subfield x = embedSubField subfield (negate x) == negate (embedSubField subfield x)

prop_embed_iso_add :: SubField p m k -> Zech p k -> Zech p k -> Bool
prop_embed_iso_add subfield x y = embedSubField subfield (x + y) == embedSubField subfield x + embedSubField subfield y

prop_embed_iso_sub :: SubField p m k -> Zech p k -> Zech p k -> Bool
prop_embed_iso_sub subfield x y = embedSubField subfield (x - y) == embedSubField subfield x - embedSubField subfield y

prop_embed_iso_inv :: SubField p m k -> NonZero (Zech p k) -> Bool
prop_embed_iso_inv subfield (NonZero x) = embedSubField subfield (inverse x) == inverse (embedSubField subfield x)

prop_embed_iso_mul :: SubField p m k -> Zech p k -> Zech p k -> Bool
prop_embed_iso_mul subfield x y = embedSubField subfield (x * y) == embedSubField subfield x * embedSubField subfield y

prop_embed_iso_div :: SubField p m k -> Zech p k -> NonZero (Zech p k) -> Bool
prop_embed_iso_div subfield x (NonZero y) = embedSubField subfield (x / y) == embedSubField subfield x / embedSubField subfield y

--------------------------------------------------------------------------------
-- * QuickCheck boilerplate shit

mkGen :: (forall g. RandomGen g => (g -> (a,g))) -> Gen a
mkGen f = MkGen (\r _ -> let (x,_) = f r in x)

genZech :: SNat64 p -> SNat64 m -> Gen (Zech p m)
genZech sp sm = mkGen (randomFieldElem witness) where
  witness = lookupZechStore sp sm 

genZechNonZero :: SNat64 p -> SNat64 m -> Gen (NonZero (Zech p m))
genZechNonZero sp sm = NonZero <$> mkGen (randomInvertible witness) where
  witness = lookupZechStore sp sm 

--------------------------------------------------------------------------------

mkProperty1 :: WitnessZech p m -> (Zech p m -> Bool) -> Property
mkProperty1 witness f = Test.Tasty.QuickCheck.forAll gen f where
  gen = genZech (fieldPrimeSNat64 witness) (fieldDimSNat64 witness)

mkProperty2 :: WitnessZech p m -> (Zech p m -> Zech p m -> Bool) -> Property
mkProperty2 witness f = forAll2 gen gen f where
  gen = genZech (fieldPrimeSNat64 witness) (fieldDimSNat64 witness)

mkProperty1_NZ :: WitnessZech p m -> (NonZero (Zech p m) -> Bool) -> Property
mkProperty1_NZ witness f = Test.Tasty.QuickCheck.forAll gen f where
  gen = genZechNonZero (fieldPrimeSNat64 witness) (fieldDimSNat64 witness)

mkProperty2_NZ :: WitnessZech p m -> (Zech p m -> NonZero (Zech p m) -> Bool) -> Property
mkProperty2_NZ witness f = forAll2 gen1 gen2 f where
  gen1 = genZech        (fieldPrimeSNat64 witness) (fieldDimSNat64 witness)
  gen2 = genZechNonZero (fieldPrimeSNat64 witness) (fieldDimSNat64 witness)

--------------------------------------------------------------------------------

x_nullary :: (SubField p m k -> Bool) -> SubField p m k -> Property
x_nullary f subfield = property (f subfield)

x_unary :: (SubField p m k -> Zech p k -> Bool) -> SubField p m k -> Property
x_unary f subfield = mkProperty1 (subFieldWitness subfield) (f subfield)

x_unaryI :: (SubField p m k -> Int -> Bool) -> SubField p m k -> Property
x_unaryI f subfield = property (f subfield)

x_unaryNZ :: (SubField p m k -> NonZero (Zech p k) -> Bool) -> SubField p m k -> Property
x_unaryNZ f subfield = mkProperty1_NZ (subFieldWitness subfield) (f subfield)

x_binary :: (SubField p m k -> Zech p k -> Zech p k -> Bool) -> SubField p m k -> Property
x_binary f subfield = mkProperty2 (subFieldWitness subfield) (f subfield)

x_binaryNZ :: (SubField p m k -> Zech p k -> NonZero (Zech p k) -> Bool) -> SubField p m k -> Property
x_binaryNZ f subfield = mkProperty2_NZ (subFieldWitness subfield) (f subfield)

--------------------------------------------------------------------------------

ambient_zechfields :: [(String,TestAmbientField)]
ambient_zechfields = 
  [ -- 2^m
    ( "Zech(2^1)"   , TestAmbientField (zechfield @2 @1 ) )
  , ( "Zech(2^2)"   , TestAmbientField (zechfield @2 @2 ) )
  , ( "Zech(2^3)"   , TestAmbientField (zechfield @2 @3 ) )
  , ( "Zech(2^4)"   , TestAmbientField (zechfield @2 @4 ) )
  , ( "Zech(2^5)"   , TestAmbientField (zechfield @2 @5 ) )
  , ( "Zech(2^6)"   , TestAmbientField (zechfield @2 @6 ) )
  , ( "Zech(2^7)"   , TestAmbientField (zechfield @2 @7 ) )
  , ( "Zech(2^8)"   , TestAmbientField (zechfield @2 @8 ) )
  , ( "Zech(2^9)"   , TestAmbientField (zechfield @2 @9 ) )
  , ( "Zech(2^10)"  , TestAmbientField (zechfield @2 @10) )
  , ( "Zech(2^12)"  , TestAmbientField (zechfield @2 @12) )
  , ( "Zech(2^14)"  , TestAmbientField (zechfield @2 @14) )
  , ( "Zech(2^16)"  , TestAmbientField (zechfield @2 @16) )
    -- 3^m
  , ( "Zech(3^3)"   , TestAmbientField (zechfield @3 @3 ) )
  , ( "Zech(3^4)"   , TestAmbientField (zechfield @3 @4 ) )
  , ( "Zech(3^6)"   , TestAmbientField (zechfield @3 @6 ) )
  , ( "Zech(3^8)"   , TestAmbientField (zechfield @3 @8 ) )
  , ( "Zech(3^9)"   , TestAmbientField (zechfield @3 @9 ) )
    -- 5^m
  , ( "Zech(5^1)"   , TestAmbientField (zechfield @5 @1 ) )
  , ( "Zech(5^2)"   , TestAmbientField (zechfield @5 @2 ) )
  , ( "Zech(5^3)"   , TestAmbientField (zechfield @5 @3 ) )
  , ( "Zech(5^4)"   , TestAmbientField (zechfield @5 @4 ) )
  , ( "Zech(5^5)"   , TestAmbientField (zechfield @5 @5 ) )
  , ( "Zech(5^6)"   , TestAmbientField (zechfield @5 @6 ) )
  ]

--------------------------------------------------------------------------------

