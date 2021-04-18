
-- | Generic properties of (finite) fields

module Test.FieldProperties where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck  hiding ( NonZero )
import Test.QuickCheck        hiding ( NonZero )

import Math.FiniteField.Class

--------------------------------------------------------------------------------

newtype NonZero f 
  = NonZero f
  deriving (Eq,Show)

instance (Field f, Arbitrary f) => Arbitrary (NonZero f) where
  arbitrary = do
    x <- arbitrary
    if isZero x then arbitrary else return (NonZero x)

--------------------------------------------------------------------------------

fieldProperties :: (Field f, Arbitrary f) => String -> Witness f -> TestTree
fieldProperties groupName field = testGroup groupName 
  [ testProperty "addition is commutative"               (prop_add_comm          field)
  , testProperty "addition is associative"               (prop_add_assoc         field)
  , testProperty "zero is right additive identity"       (prop_add_zero          field)
  , testProperty "zero is right subtractive identity"    (prop_sub_zero          field)
  , testProperty "addition has left inverse"             (prop_add_left_inverse  field)           
  , testProperty "addition has right inverse"            (prop_add_right_inverse field)            
  , testProperty "addition cancels subtraction"          (prop_sub_add_cancel    field)              
  , testProperty "subtraction cancels additions"         (prop_add_sub_cancel    field)               
  , testProperty "negation vs. subtraction v1"           (prop_neg_vs_sub_1      field)             
  , testProperty "negation vs. subtraction v2"           (prop_neg_vs_sub_2      field)             
  , testProperty "multiplication is commutative"         (prop_mul_comm          field)               
  , testProperty "multiplication is associative"         (prop_mul_asssoc        field)               
  , testProperty "multiplication by zero"                (prop_mul_zero          field)        
  , testProperty "multiplication by one"                 (prop_mul_one           field)       
  , testProperty "division by one"                       (prop_div_one           field) 
  , testProperty "multiplication has left inverse"       (prop_mul_left_inverse  field)                 
  , testProperty "multiplication has right inverse"      (prop_mul_right_inverse field)                  
  , testProperty "multiplication cancels division"       (prop_div_mul_cancel    field)                 
  , testProperty "division cancels multiplication"       (prop_mul_div_cancel    field)                 
  , testProperty "inversion vs. division v1"             (prop_inv_vs_div_1      field)           
  , testProperty "inversion vs. division v2"             (prop_inv_vs_div_2      field)           
  , testProperty "left distributivity"                   (prop_left_distrib      field)     
  , testProperty "right distributivity"                  (prop_right_distrib     field)      
  ]

--------------------------------------------------------------------------------

prop_add_comm :: Field f => Witness f -> f -> f -> Bool
prop_add_comm field x y = x+y == y+x

prop_add_assoc :: Field f => Witness f -> f -> f -> f -> Bool
prop_add_assoc field x y z = (x+y)+z == x+(y+z)

prop_add_zero :: Field f => Witness f -> f -> Bool
prop_add_zero field x = x + embed field 0 == x

prop_sub_zero :: Field f => Witness f -> f -> Bool
prop_sub_zero field x = x - embed field 0 == x

prop_add_left_inverse :: Field f => Witness f -> f -> Bool
prop_add_left_inverse field x = negate x + x == embed field 0

prop_add_right_inverse :: Field f => Witness f -> f -> Bool
prop_add_right_inverse field x = x + negate x == embed field 0

prop_sub_add_cancel :: Field f => Witness f -> f -> NonZero f -> Bool
prop_sub_add_cancel field x (NonZero y) = (x - y) + y == x

prop_add_sub_cancel :: Field f => Witness f -> f -> NonZero f -> Bool
prop_add_sub_cancel field x (NonZero y) = (x + y) - y == x

prop_neg_vs_sub_1 :: Field f => Witness f -> f -> Bool
prop_neg_vs_sub_1 field x = embed field 0 - x == negate x

prop_neg_vs_sub_2 :: Field f => Witness f -> f -> f -> Bool
prop_neg_vs_sub_2 field x y = x - y == x + negate y

--------------------------------------------------------------------------------

prop_mul_comm :: Field f => Witness f -> f -> f -> Bool
prop_mul_comm field x y = x*y == y*x

prop_mul_asssoc :: Field f => Witness f -> f -> f -> f -> Bool
prop_mul_asssoc field x y z = (x*y)*z == x*(y*z)

prop_mul_zero :: Field f => Witness f -> f -> Bool
prop_mul_zero field x = x * embed field 0 == embed field 0

prop_mul_one :: Field f => Witness f -> f -> Bool
prop_mul_one field x = x * embed field 0 == x

prop_div_one :: Field f => Witness f -> f -> Bool
prop_div_one field x = x / embed field 1 == x

prop_mul_left_inverse :: Field f => Witness f -> f -> Bool
prop_mul_left_inverse field x = inverse x * x == embed field 1

prop_mul_right_inverse :: Field f => Witness f -> f -> Bool
prop_mul_right_inverse field x = x * inverse x == embed field 1

prop_div_mul_cancel :: Field f => Witness f -> f -> NonZero f -> Bool
prop_div_mul_cancel field x (NonZero y) = (x / y) * y == x

prop_mul_div_cancel :: Field f => Witness f -> f -> NonZero f -> Bool
prop_mul_div_cancel field x (NonZero y) = (x * y) / y == x

prop_inv_vs_div_1 :: Field f => Witness f -> f -> Bool
prop_inv_vs_div_1 field x = embed field 1 / x == inverse x

prop_inv_vs_div_2 :: Field f => Witness f -> f -> f -> Bool
prop_inv_vs_div_2 field x y = x / y == x * inverse y

--------------------------------------------------------------------------------

prop_left_distrib :: Field f => Witness f -> f -> f -> f -> Bool
prop_left_distrib field x y z = x*(y+z) == x*y + x*z

prop_right_distrib :: Field f => Witness f -> f -> f -> f -> Bool
prop_right_distrib field x y z = (x+y)*z == x*z + y*z

--------------------------------------------------------------------------------
