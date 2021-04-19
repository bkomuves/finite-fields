
-- | Stuff needed in more than one place

{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Common where

--------------------------------------------------------------------------------

-- import Data.Proxy

-- import Test.Tasty
-- import Test.Tasty.QuickCheck  hiding ( NonZero )
-- import Test.QuickCheck        hiding ( NonZero )

-- import Math.FiniteField.Class

--------------------------------------------------------------------------------

newtype NonZero f 
  = NonZero f
  deriving (Eq,Show)

{-
-- unfortunately this does not work...
instance forall f g. (Field f, Arbitrary g, Coercible g f) => Arbitrary (NonZero g) where
  arbitrary = do
    x <- arbitrary
    if isZero (coerce @g @f x) then arbitrary else return (NonZero x)
-}

--------------------------------------------------------------------------------

