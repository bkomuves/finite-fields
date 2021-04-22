
-- | Some concrete fields
--
-- It's problematic to have an Arbitrary instance to depend on some
-- external data, so to start with something I'll just have some concrete fields 
-- to test in...
--

{-# LANGUAGE DataKinds, Rank2Types, TypeApplications #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Test.ConcreteFields where

--------------------------------------------------------------------------------

import System.Random ( RandomGen )

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Math.FiniteField.Class
import Math.FiniteField.Conway
import Math.FiniteField.TypeLevel

import Math.FiniteField.PrimeField.Generic as Z
import Math.FiniteField.PrimeField.Small   as S
import Math.FiniteField.GaloisField.Small  as G
import Math.FiniteField.GaloisField.Zech   as Zech
import Math.FiniteField.GaloisField.Zech.C as C

import Test.WitnessStore
import Test.Common

--------------------------------------------------------------------------------

-- data TestField = forall g f. (Field f, Show g, Arbitrary g, Arbitrary (NonZero g)) => TestField (g -> f) (Witness f)

data TestField = forall f. (RandomField f) => TestField (Witness f)

generic_primefields :: [(String,TestField)]
generic_primefields = 
  [ ( "F_2"     , TestField (genericField @2    ) )
  , ( "F_3"     , TestField (genericField @3    ) )
  , ( "F_5"     , TestField (genericField @5    ) )
  , ( "F_7"     , TestField (genericField @7    ) )
  , ( "F_11"    , TestField (genericField @11   ) )
  , ( "F_13"    , TestField (genericField @13   ) )
  , ( "F_17"    , TestField (genericField @17   ) )
  , ( "F_229"   , TestField (genericField @229  ) )
  , ( "F_257"   , TestField (genericField @257  ) )
  , ( "F_7919"  , TestField (genericField @7919 ) )
  , ( "F_32003" , TestField (genericField @32003) )
  ]

small_primefields :: [(String,TestField)]
small_primefields = 
  [ ( "F_2"     , TestField (smallField @2    ) )
  , ( "F_3"     , TestField (smallField @3    ) )
  , ( "F_5"     , TestField (smallField @5    ) )
  , ( "F_7"     , TestField (smallField @7    ) )
  , ( "F_11"    , TestField (smallField @11   ) )
  , ( "F_13"    , TestField (smallField @13   ) )
  , ( "F_17"    , TestField (smallField @17   ) )
  , ( "F_229"   , TestField (smallField @229  ) )
  , ( "F_257"   , TestField (smallField @257  ) )
  , ( "F_7919"  , TestField (smallField @7919 ) )
  , ( "F_32003" , TestField (smallField @32003) )
  ]

small_galoisfields :: [(String,TestField)]
small_galoisfields = 
  [ -- p^1
    ( "GF(2)"     , TestField (galoisField  @2   @1) ) 
  , ( "GF(3)"     , TestField (galoisField  @3   @1) )
  , ( "GF(5)"     , TestField (galoisField  @5   @1) )
  , ( "GF(7)"     , TestField (galoisField  @7   @1) )
  , ( "GF(17)"    , TestField (galoisField  @17  @1) )
  , ( "GF(257)"   , TestField (galoisField  @257 @1) )
    -- 2^m
  , ( "GF(2^1)"   , TestField (galoisField  @2   @1) )
  , ( "GF(2^2)"   , TestField (galoisField  @2   @2) )
  , ( "GF(2^3)"   , TestField (galoisField  @2   @3) )
  , ( "GF(2^4)"   , TestField (galoisField  @2   @4) )
  , ( "GF(2^5)"   , TestField (galoisField  @2   @5) )
  , ( "GF(2^6)"   , TestField (galoisField  @2   @6) )
    -- 3^m
  , ( "GF(3^1)"   , TestField (galoisField  @3   @1) )
  , ( "GF(3^2)"   , TestField (galoisField  @3   @2) )
  , ( "GF(3^3)"   , TestField (galoisField  @3   @3) )
  , ( "GF(3^4)"   , TestField (galoisField  @3   @4) )
  , ( "GF(3^5)"   , TestField (galoisField  @3   @5) )
  , ( "GF(3^6)"   , TestField (galoisField  @3   @6) )
    -- 5^m
  , ( "GF(5^1)"   , TestField (galoisField  @5   @1) )
  , ( "GF(5^2)"   , TestField (galoisField  @5   @2) )
  , ( "GF(5^3)"   , TestField (galoisField  @5   @3) )
  , ( "GF(5^4)"   , TestField (galoisField  @5   @4) )
  , ( "GF(5^5)"   , TestField (galoisField  @5   @5) )
  , ( "GF(5^6)"   , TestField (galoisField  @5   @6) )
  ]

small_zechfields :: [(String,TestField)]
small_zechfields = 
  [ -- 2^m
    ( "Zech(2^1)"   , TestField (zechField  @2  @1 )  )
  , ( "Zech(2^2)"   , TestField (zechField  @2  @2 )  )
  , ( "Zech(2^3)"   , TestField (zechField  @2  @3 )  )
  , ( "Zech(2^4)"   , TestField (zechField  @2  @4 )  )
  , ( "Zech(2^5)"   , TestField (zechField  @2  @5 )  )
  , ( "Zech(2^6)"   , TestField (zechField  @2  @6 )  )
    -- 3^m
  , ( "Zech(3^1)"   , TestField (zechField  @3  @1 )  )
  , ( "Zech(3^2)"   , TestField (zechField  @3  @2 )  )
  , ( "Zech(3^3)"   , TestField (zechField  @3  @3 )  )
  , ( "Zech(3^4)"   , TestField (zechField  @3  @4 )  )
  , ( "Zech(3^5)"   , TestField (zechField  @3  @5 )  )
  , ( "Zech(3^6)"   , TestField (zechField  @3  @6 )  )
    -- 7^m
  , ( "Zech(7^1)"   , TestField (zechField  @7  @1 )  )
  , ( "Zech(7^2)"   , TestField (zechField  @7  @2 )  )
  , ( "Zech(7^3)"   , TestField (zechField  @7  @3 )  )
  , ( "Zech(7^4)"   , TestField (zechField  @7  @4 )  )
  , ( "Zech(7^5)"   , TestField (zechField  @7  @5 )  )
  , ( "Zech(7^6)"   , TestField (zechField  @7  @6 )  )
    -- 13^m
  , ( "Zech(13^1)"  , TestField (zechField  @13 @1 )  )
  , ( "Zech(13^2)"  , TestField (zechField  @13 @2 )  )
  , ( "Zech(13^3)"  , TestField (zechField  @13 @3 )  )
  , ( "Zech(13^4)"  , TestField (zechField  @13 @4 )  )
  , ( "Zech(13^5)"  , TestField (zechField  @13 @5 )  )
  -- , ( "Zech(13^6)"  , TestField (zechField  @13 @6 )  )
  ]

small_cfields :: [(String,TestField)]
small_cfields = 
  [ -- 2^m
    ( "CFq(2^1)"   , TestField (cField  @2  @1  )  )
  , ( "CFq(2^2)"   , TestField (cField  @2  @2  )  )
  , ( "CFq(2^3)"   , TestField (cField  @2  @3  )  )
  , ( "CFq(2^4)"   , TestField (cField  @2  @4  )  )
  , ( "CFq(2^5)"   , TestField (cField  @2  @5  )  )
  , ( "CFq(2^6)"   , TestField (cField  @2  @6  )  )
    -- 5^m
  , ( "CFq(5^1)"   , TestField (cField  @5  @1  )  )
  , ( "CFq(5^2)"   , TestField (cField  @5  @2  )  )
  , ( "CFq(5^3)"   , TestField (cField  @5  @3  )  )
  , ( "CFq(5^4)"   , TestField (cField  @5  @4  )  )
  , ( "CFq(5^5)"   , TestField (cField  @5  @5  )  )
  , ( "CFq(5^6)"   , TestField (cField  @5  @6  )  )
    -- 11^m
  , ( "CFq(11^1)"  , TestField (cField  @11 @1  )  )
  , ( "CFq(11^2)"  , TestField (cField  @11 @2  )  )
  , ( "CFq(11^3)"  , TestField (cField  @11 @3  )  )
  , ( "CFq(11^4)"  , TestField (cField  @11 @4  )  )
  , ( "CFq(11^5)"  , TestField (cField  @11 @5  )  )
  -- , ( "CFq(11^6)"  , TestField (cField  @11 @6  )  )
  ]

--------------------------------------------------------------------------------
-- * type safe quickceck generators

genFp :: SNat p -> Gen (Z.Fp p)
genFp sp = mkGen (randomFieldElem witness) where witness = lookupGenericStore sp

genSmall :: SNat64 p -> Gen (S.Fp p)
genSmall sp = mkGen (randomFieldElem witness) where witness = lookupSmallStore sp

genGF :: SNat64 p -> SNat64 m -> Gen (GF p m)
genGF sp sm = mkGen (randomFieldElem witness) where witness = lookupGaloisStore sp sm 

genZech :: SNat64 p -> SNat64 m -> Gen (Zech p m)
genZech sp sm = mkGen (randomFieldElem witness) where witness = lookupZechStore sp sm 

genCFq :: SNat64 p -> SNat64 m -> Gen (CFq p m)
genCFq sp sm = mkGen (randomFieldElem witness) where witness = lookupCStore sp sm 

genFpNonZero :: SNat p -> Gen (NonZero (Z.Fp p))
genFpNonZero sp = NonZero <$> mkGen (randomInvertible witness) where witness = lookupGenericStore sp 

genSmallNonZero :: SNat64 p -> Gen (NonZero (S.Fp p))
genSmallNonZero sp = NonZero <$> mkGen (randomInvertible witness) where witness = lookupSmallStore sp 

genGFNonZero :: SNat64 p -> SNat64 m -> Gen (NonZero (GF p m))
genGFNonZero sp sm = NonZero <$> mkGen (randomInvertible witness) where witness = lookupGaloisStore sp sm 

genZechNonZero :: SNat64 p -> SNat64 m -> Gen (NonZero (Zech p m))
genZechNonZero sp sm = NonZero <$> mkGen (randomInvertible witness) where witness = lookupZechStore sp sm 

genCFqNonZero :: SNat64 p -> SNat64 m -> Gen (NonZero (CFq p m))
genCFqNonZero sp sm = NonZero <$> mkGen (randomInvertible witness) where witness = lookupCStore sp sm 

--------------------------------------------------------------------------------

instance RandomField (Z.Fp p) where
  unaryProp    w f = forAll1 gen           f where gen = genFp        (fieldPrimeSNat w)
  binaryProp   w f = forAll2 gen gen       f where gen = genFp        (fieldPrimeSNat w)
  ternaryProp  w f = forAll3 gen gen gen   f where gen = genFp        (fieldPrimeSNat w)
  unaryPropNZ  w f = forAll1 gen           f where gen = genFpNonZero (fieldPrimeSNat w)
  binaryPropI  w f = forAll2 gen arbitrary f where gen = genFp        (fieldPrimeSNat w)
  binaryPropNZ w f = forAll2 gen1 gen2     f where  
    gen1 = genFp        (fieldPrimeSNat w) 
    gen2 = genFpNonZero (fieldPrimeSNat w) 

instance RandomField (S.Fp p) where
  unaryProp    w f = forAll1 gen           f where gen = genSmall        (fieldPrimeSNat64 w) 
  binaryProp   w f = forAll2 gen gen       f where gen = genSmall        (fieldPrimeSNat64 w) 
  ternaryProp  w f = forAll3 gen gen gen   f where gen = genSmall        (fieldPrimeSNat64 w) 
  unaryPropNZ  w f = forAll1 gen           f where gen = genSmallNonZero (fieldPrimeSNat64 w) 
  binaryPropI  w f = forAll2 gen arbitrary f where gen = genSmall        (fieldPrimeSNat64 w) 
  binaryPropNZ w f = forAll2 gen1 gen2     f where  
    gen1 = genSmall        (fieldPrimeSNat64 w) 
    gen2 = genSmallNonZero (fieldPrimeSNat64 w) 

instance RandomField (GF p m) where
  unaryProp    w f = forAll1 gen           f where gen = genGF        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryProp   w f = forAll2 gen gen       f where gen = genGF        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  ternaryProp  w f = forAll3 gen gen gen   f where gen = genGF        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  unaryPropNZ  w f = forAll1 gen           f where gen = genGFNonZero (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryPropI  w f = forAll2 gen arbitrary f where gen = genGF        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryPropNZ w f = forAll2 gen1 gen2     f where  
    gen1 = genGF        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
    gen2 = genGFNonZero (fieldPrimeSNat64 w) (fieldDimSNat64 w)

instance RandomField (Zech p m) where
  unaryProp    w f = forAll1 gen           f where gen = genZech        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryProp   w f = forAll2 gen gen       f where gen = genZech        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  ternaryProp  w f = forAll3 gen gen gen   f where gen = genZech        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  unaryPropNZ  w f = forAll1 gen           f where gen = genZechNonZero (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryPropI  w f = forAll2 gen arbitrary f where gen = genZech        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryPropNZ w f = forAll2 gen1 gen2     f where  
    gen1 = genZech        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
    gen2 = genZechNonZero (fieldPrimeSNat64 w) (fieldDimSNat64 w)

instance RandomField (CFq p m) where
  unaryProp    w f = forAll1 gen           f where gen = genCFq        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryProp   w f = forAll2 gen gen       f where gen = genCFq        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  ternaryProp  w f = forAll3 gen gen gen   f where gen = genCFq        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  unaryPropNZ  w f = forAll1 gen           f where gen = genCFqNonZero (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryPropI  w f = forAll2 gen arbitrary f where gen = genCFq        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
  binaryPropNZ w f = forAll2 gen1 gen2     f where  
    gen1 = genCFq        (fieldPrimeSNat64 w) (fieldDimSNat64 w)
    gen2 = genCFqNonZero (fieldPrimeSNat64 w) (fieldDimSNat64 w)

--------------------------------------------------------------------------------

-- TODO: find out what was wrong with gf_2_1 here
-- zech_2_1 = Zech.WitnessZech (makeZechTable gf_2  ) :: Zech.WitnessZech 2 1   

--------------------------------------------------------------------------------

