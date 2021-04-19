
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

fieldProperties :: forall f g. (Field f, Show g, Arbitrary g, Arbitrary (NonZero g)) => String -> (g -> f) -> Witness f -> TestTree
fieldProperties groupName unwrap field = testGroup groupName 
  [ testProperty "addition is commutative"               (binary   unwrap prop_add_comm          field)
  , testProperty "addition is associative"               (ternary  unwrap prop_add_assoc         field)
  , testProperty "zero is right additive identity"       (unary    unwrap prop_add_zero          field)
  , testProperty "zero is right subtractive identity"    (unary    unwrap prop_sub_zero          field)
  , testProperty "addition has left inverse"             (unary    unwrap prop_add_left_inverse  field)           
  , testProperty "addition has right inverse"            (unary    unwrap prop_add_right_inverse field)            
  , testProperty "addition cancels subtraction"          (binary   unwrap prop_sub_add_cancel    field)              
  , testProperty "subtraction cancels additions"         (binary   unwrap prop_add_sub_cancel    field)               
  , testProperty "negation vs. subtraction v1"           (unary    unwrap prop_neg_vs_sub_1      field)             
  , testProperty "negation vs. subtraction v2"           (binary   unwrap prop_neg_vs_sub_2      field)             
  , testProperty "multiplication is commutative"         (binary   unwrap prop_mul_comm          field)               
  , testProperty "multiplication is associative"         (ternary  unwrap prop_mul_asssoc        field)               
  , testProperty "multiplication by zero"                (unary    unwrap prop_mul_zero          field)        
  , testProperty "multiplication by one"                 (unary    unwrap prop_mul_one           field)       
  , testProperty "division by one"                       (unary    unwrap prop_div_one           field) 
  , testProperty "multiplication has left inverse"       (unaryNZ  unwrap prop_mul_left_inverse  field)                 
  , testProperty "multiplication has right inverse"      (unaryNZ  unwrap prop_mul_right_inverse field)                  
  , testProperty "multiplication cancels division"       (binaryNZ unwrap prop_div_mul_cancel    field)                 
  , testProperty "division cancels multiplication"       (binaryNZ unwrap prop_mul_div_cancel    field)                 
  , testProperty "inversion vs. division v1"             (unaryNZ  unwrap prop_inv_vs_div_1      field)           
  , testProperty "inversion vs. division v2"             (binaryNZ unwrap prop_inv_vs_div_2      field)           
  , testProperty "left distributivity"                   (ternary  unwrap prop_left_distrib      field)     
  , testProperty "right distributivity"                  (ternary  unwrap prop_right_distrib     field)      
  ]

--------------------------------------------------------------------------------
-- hacking around newtypes and instances...

unary :: (b -> a) -> (witness -> a -> result) -> (witness -> b -> result)
unary unwrap f w x = f w (unwrap x) 

unaryNZ :: (b -> a) -> (witness -> NonZero a -> result) -> (witness -> NonZero b -> result)
unaryNZ unwrap f w (NonZero x) = f w (NonZero $ unwrap x)

binary :: (b -> a) -> (witness -> a -> a -> result) -> (witness -> b -> b -> result)
binary unwrap f w x y = f w (unwrap x) (unwrap y) 

binaryNZ :: (b -> a) -> (witness -> a -> NonZero a -> result) -> (witness -> b -> NonZero b -> result)
binaryNZ unwrap f w x (NonZero y) = f w (unwrap x) (NonZero $ unwrap y) 

ternary :: (b -> a) -> (witness -> a -> a -> a -> result) -> (witness -> b -> b -> b -> result)
ternary unwrap f w x y z = f w (unwrap x) (unwrap y) (unwrap z)

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
