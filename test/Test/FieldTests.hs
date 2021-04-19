
-- | Test the field properties on some conrete fields

module Test.FieldTests where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck        

import Math.FiniteField.Class

import Test.FieldProperties
import Test.ConcreteFields

--------------------------------------------------------------------------------

generic_primefield_tests :: TestTree
generic_primefield_tests = testGroup "PrimeField/Generic" (map someFieldProps generic_primefields)

small_primefield_tests :: TestTree
small_primefield_tests = testGroup "PrimeField/Small" (map someFieldProps small_primefields)

small_galoisfield_tests :: TestTree
small_galoisfield_tests = testGroup "GaloisField/Small" (map someFieldProps small_galoisfields)

small_zechfield_tests :: TestTree
small_zechfield_tests = testGroup "GaloisField/Zech" (map someFieldProps small_zechfields)

--------------------------------------------------------------------------------

someFieldProps :: (String,TestField) -> TestTree
someFieldProps (name,somefield) = case somefield of
  TestField unwrap field -> fieldProperties ("field properties for the field " ++ name) unwrap field


