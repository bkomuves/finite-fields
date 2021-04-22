
-- | Generic properties of (finite) fields

{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.FieldProperties where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck  hiding ( NonZero )
import Test.QuickCheck        hiding ( NonZero )

import Math.FiniteField.Class

import Test.Common

--------------------------------------------------------------------------------

fieldProperties :: forall f. (RandomField f) => String -> Witness f -> TestTree
fieldProperties groupName field = testGroup groupName 
  [ testProperty "addition is commutative"               (binary    prop_add_comm           field)
  , testProperty "addition is associative"               (ternary   prop_add_assoc          field)
  , testProperty "zero is right additive identity"       (unary     prop_add_zero           field)
  , testProperty "zero is right subtractive identity"    (unary     prop_sub_zero           field)
  , testProperty "addition has left inverse"             (unary     prop_add_left_inverse   field)           
  , testProperty "addition has right inverse"            (unary     prop_add_right_inverse  field)            
  , testProperty "addition cancels subtraction"          (binary    prop_sub_add_cancel     field)              
  , testProperty "subtraction cancels additions"         (binary    prop_add_sub_cancel     field)               
  , testProperty "negation vs. subtraction v1"           (unary     prop_neg_vs_sub_1       field)             
  , testProperty "negation vs. subtraction v2"           (binary    prop_neg_vs_sub_2       field)             
  , testProperty "multiplication is commutative"         (binary    prop_mul_comm           field)               
  , testProperty "multiplication is associative"         (ternary   prop_mul_asssoc         field)               
  , testProperty "multiplication by zero"                (unary     prop_mul_zero           field)        
  , testProperty "multiplication by one"                 (unary     prop_mul_one            field)       
  , testProperty "division by one"                       (unary     prop_div_one            field) 
  , testProperty "multiplication has left inverse"       (unaryNZ   prop_mul_left_inverse   field)                 
  , testProperty "multiplication has right inverse"      (unaryNZ   prop_mul_right_inverse  field)                  
  , testProperty "multiplication cancels division"       (binaryNZ  prop_div_mul_cancel     field)                 
  , testProperty "division cancels multiplication"       (binaryNZ  prop_mul_div_cancel     field)                 
  , testProperty "inversion vs. division v1"             (unaryNZ   prop_inv_vs_div_1       field)           
  , testProperty "inversion vs. division v2"             (binaryNZ  prop_inv_vs_div_2       field)           
  , testProperty "left distributivity"                   (ternary   prop_left_distrib       field)     
  , testProperty "right distributivity"                  (ternary   prop_right_distrib      field)      
  , testProperty "multiply by the characteristic"        (unary     prop_mul_by_char        field)
  , testProperty "x ^ (q-2) == inverse x"                (unaryNZ   prop_inv_vs_pow         field)
  , testProperty "x ^ (q-1) == 1"                        (unaryNZ   prop_mul_order          field)
  , testProperty "x ^ q     == x"                        (unary     prop_pow_vs_id          field)
  , testProperty "power vs. iterated product"            (binaryI   prop_pow_vs_product     field)
  , testProperty "Frobenius vs. definition"              (unary     prop_frobenius          field)
  , testProperty "freshman's dream"                      (binary    prop_freshmans_dream    field)
  ]

--------------------------------------------------------------------------------

prop_add_comm :: Field f => Witness f -> f -> f -> Bool
prop_add_comm field x y = (x + y == y + x)

prop_add_assoc :: Field f => Witness f -> f -> f -> f -> Bool
prop_add_assoc field x y z = (x + y) + z == x + (y + z)

prop_add_zero :: Field f => Witness f -> f -> Bool
prop_add_zero field x = x + zero field == x

prop_sub_zero :: Field f => Witness f -> f -> Bool
prop_sub_zero field x = x - zero field == x

prop_add_left_inverse :: Field f => Witness f -> f -> Bool
prop_add_left_inverse field x = negate x + x == zero field

prop_add_right_inverse :: Field f => Witness f -> f -> Bool
prop_add_right_inverse field x = x + negate x == zero field 

prop_sub_add_cancel :: Field f => Witness f -> f -> f -> Bool
prop_sub_add_cancel field x y = (x - y) + y == x

prop_add_sub_cancel :: Field f => Witness f -> f -> f -> Bool
prop_add_sub_cancel field x y = (x + y) - y == x

prop_neg_vs_sub_1 :: Field f => Witness f -> f -> Bool
prop_neg_vs_sub_1 field x = zero field - x == negate x

prop_neg_vs_sub_2 :: Field f => Witness f -> f -> f -> Bool
prop_neg_vs_sub_2 field x y = x - y == x + negate y

--------------------------------------------------------------------------------

prop_mul_comm :: Field f => Witness f -> f -> f -> Bool
prop_mul_comm field x y = (x * y == y * x)

prop_mul_asssoc :: Field f => Witness f -> f -> f -> f -> Bool
prop_mul_asssoc field x y z = (x * y) * z == x * ( y * z)

prop_mul_zero :: Field f => Witness f -> f -> Bool
prop_mul_zero field x = x * zero field == zero field

prop_mul_one :: Field f => Witness f -> f -> Bool
prop_mul_one field x = x * one field == x

prop_div_one :: Field f => Witness f -> f -> Bool
prop_div_one field x = x / one field == x

prop_mul_left_inverse :: Field f => Witness f -> NonZero f -> Bool
prop_mul_left_inverse field (NonZero x) = inverse x * x == one field 

prop_mul_right_inverse :: Field f => Witness f -> NonZero f -> Bool
prop_mul_right_inverse field (NonZero x) = x * inverse x == one field 

prop_div_mul_cancel :: Field f => Witness f -> f -> NonZero f -> Bool
prop_div_mul_cancel field x (NonZero y) = (x / y) * y == x

prop_mul_div_cancel :: Field f => Witness f -> f -> NonZero f -> Bool
prop_mul_div_cancel field x (NonZero y) = (x * y) / y == x

prop_inv_vs_div_1 :: Field f => Witness f -> NonZero f -> Bool
prop_inv_vs_div_1 field (NonZero x) = one field / x == inverse x

prop_inv_vs_div_2 :: Field f => Witness f -> f -> NonZero f -> Bool
prop_inv_vs_div_2 field x (NonZero y) = x / y == x * inverse y

--------------------------------------------------------------------------------

prop_left_distrib :: Field f => Witness f -> f -> f -> f -> Bool
prop_left_distrib field x y z = x*(y+z) == x*y + x*z

prop_right_distrib :: Field f => Witness f -> f -> f -> f -> Bool
prop_right_distrib field x y z = (x+y)*z == x*z + y*z

--------------------------------------------------------------------------------

prop_mul_by_char :: Field f => Witness f -> f -> Bool
prop_mul_by_char field x = foldl1 (+) (replicate (fromIntegral $ characteristic field) x) == zero field

prop_inv_vs_pow :: Field f => Witness f -> NonZero f -> Bool
prop_inv_vs_pow field (NonZero x) = inverse x == power x (fieldSize field - 2)

prop_mul_order :: Field f => Witness f -> NonZero f -> Bool
prop_mul_order field (NonZero x) = one field == power x (fieldSize field - 1)

prop_pow_vs_id :: Field f => Witness f -> f -> Bool
prop_pow_vs_id field x = x == power x (fieldSize field)

prop_pow_vs_product :: Field f => Witness f -> f -> Int -> Bool
prop_pow_vs_product field x e 
  | e >  0 =  powerSmall x e == foldl1 (*) (replicate      e           x )
  | e <  0 =  powerSmall x e == foldl1 (*) (replicate (abs e) (inverse x))
  | e == 0 =  if isZero x
                then powerSmall x e == zero field    -- it seems this is the "correct" choice
                else powerSmall x e == one  field    -- for example 0 == 0^(q-1) == 0^0 /= 1 

prop_frobenius :: Field f => Witness f -> f -> Bool
prop_frobenius field x = frobenius x == power x (characteristic field)

prop_freshmans_dream :: Field f => Witness f -> f -> f -> Bool
prop_freshmans_dream field x y = frobenius x + frobenius y == frobenius (x+y)

--------------------------------------------------------------------------------
