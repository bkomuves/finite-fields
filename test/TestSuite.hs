

{-# LANGUAGE BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Main where

--------------------------------------------------------------------------------

import Test.Tasty

import Test.FieldTests            
  ( generic_primefield_tests
  , small_primefield_tests
  , small_galoisfield_tests
  , small_zechfield_tests
  )

--------------------------------------------------------------------------------

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ generic_primefield_tests
  , small_primefield_tests
  , small_galoisfield_tests
  , small_zechfield_tests
  ]
        
--------------------------------------------------------------------------------
