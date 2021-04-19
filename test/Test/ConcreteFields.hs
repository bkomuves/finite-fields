
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

import Test.Common

--------------------------------------------------------------------------------

data TestField = forall g f. (Field f, Show g, Arbitrary g, Arbitrary (NonZero g)) => TestField (g -> f) (Witness f)

generic_primefields :: [(String,TestField)]
generic_primefields = 
  [ ( "F_2"     , TestField fromZZ2     zz2     )
  , ( "F_3"     , TestField fromZZ3     zz3     )
  , ( "F_5"     , TestField fromZZ5     zz5     )
  , ( "F_7"     , TestField fromZZ7     zz7     )
  , ( "F_11"    , TestField fromZZ11    zz11    )
  , ( "F_13"    , TestField fromZZ13    zz13    )
  , ( "F_17"    , TestField fromZZ17    zz17    )
  , ( "F_229"   , TestField fromZZ229   zz229   )
  , ( "F_257"   , TestField fromZZ257   zz257   )
  , ( "F_7919"  , TestField fromZZ7919  zz7919  )
  , ( "F_32003" , TestField fromZZ32003 zz32003 )
  ]

small_primefields :: [(String,TestField)]
small_primefields = 
  [ ( "F_2"     , TestField fromS2     s2     )
  , ( "F_3"     , TestField fromS3     s3     )
  , ( "F_5"     , TestField fromS5     s5     )
  , ( "F_7"     , TestField fromS7     s7     )
  , ( "F_11"    , TestField fromS11    s11    )
  , ( "F_13"    , TestField fromS13    s13    )
  , ( "F_17"    , TestField fromS17    s17    )
  , ( "F_229"   , TestField fromS229   s229   )
  , ( "F_257"   , TestField fromS257   s257   )
  , ( "F_7919"  , TestField fromS7919  s7919  )
  , ( "F_32003" , TestField fromS32003 s32003 )
  ]

small_galoisfields :: [(String,TestField)]
small_galoisfields = 
  [ -- p^1
    ( "GF(2)"     , TestField fromGF_2    gf_2    ) 
  , ( "GF(3)"     , TestField fromGF_3    gf_3    )
  , ( "GF(5)"     , TestField fromGF_5    gf_5    )
  , ( "GF(7)"     , TestField fromGF_7    gf_7    )
  , ( "GF(17)"    , TestField fromGF_17   gf_17   )
  , ( "GF(257)"   , TestField fromGF_257  gf_257  )
    -- 2^m
  , ( "GF(2^1)"   , TestField fromGF_2_1  gf_2_1  )
  , ( "GF(2^2)"   , TestField fromGF_2_2  gf_2_2  )
  , ( "GF(2^3)"   , TestField fromGF_2_3  gf_2_3  )
  , ( "GF(2^4)"   , TestField fromGF_2_4  gf_2_4  )
  , ( "GF(2^5)"   , TestField fromGF_2_5  gf_2_5  )
  , ( "GF(2^6)"   , TestField fromGF_2_6  gf_2_6  )
    -- 3^m
  , ( "GF(3^1)"   , TestField fromGF_3_1  gf_3_1  )
  , ( "GF(3^2)"   , TestField fromGF_3_2  gf_3_2  )
  , ( "GF(3^3)"   , TestField fromGF_3_3  gf_3_3  )
  , ( "GF(3^4)"   , TestField fromGF_3_4  gf_3_4  )
  , ( "GF(3^5)"   , TestField fromGF_3_5  gf_3_5  )
  , ( "GF(3^6)"   , TestField fromGF_3_6  gf_3_6  )
    -- 5^m
  , ( "GF(5^1)"   , TestField fromGF_5_1  gf_5_1  )
  , ( "GF(5^2)"   , TestField fromGF_5_2  gf_5_2  )
  , ( "GF(5^3)"   , TestField fromGF_5_3  gf_5_3  )
  , ( "GF(5^4)"   , TestField fromGF_5_4  gf_5_4  )
  , ( "GF(5^5)"   , TestField fromGF_5_5  gf_5_5  )
  , ( "GF(5^6)"   , TestField fromGF_5_6  gf_5_6  )
  ]

small_zechfields :: [(String,TestField)]
small_zechfields = 
  [ -- 2^m
    ( "Zech(2^1)"   , TestField fromZech_2_1  zech_2_1  )
  , ( "Zech(2^2)"   , TestField fromZech_2_2  zech_2_2  )
  , ( "Zech(2^3)"   , TestField fromZech_2_3  zech_2_3  )
  , ( "Zech(2^4)"   , TestField fromZech_2_4  zech_2_4  )
  , ( "Zech(2^5)"   , TestField fromZech_2_5  zech_2_5  )
  , ( "Zech(2^6)"   , TestField fromZech_2_6  zech_2_6  )
    -- 3^m
  , ( "Zech(3^1)"   , TestField fromZech_3_1  zech_3_1  )
  , ( "Zech(3^2)"   , TestField fromZech_3_2  zech_3_2  )
  , ( "Zech(3^3)"   , TestField fromZech_3_3  zech_3_3  )
  , ( "Zech(3^4)"   , TestField fromZech_3_4  zech_3_4  )
  , ( "Zech(3^5)"   , TestField fromZech_3_5  zech_3_5  )
  , ( "Zech(3^6)"   , TestField fromZech_3_6  zech_3_6  )
    -- 13^m
  , ( "Zech(13^1)"   , TestField fromZech_13_1  zech_13_1  )
  , ( "Zech(13^2)"   , TestField fromZech_13_2  zech_13_2  )
  , ( "Zech(13^3)"   , TestField fromZech_13_3  zech_13_3  )
  , ( "Zech(13^4)"   , TestField fromZech_13_4  zech_13_4  )
  , ( "Zech(13^5)"   , TestField fromZech_13_5  zech_13_5  )
    --, ( "Zech(13^6)"   , TestField fromZech_13_6  zech_13_6  )
  ]

--------------------------------------------------------------------------------

newtype ZZ2     = ZZ2     { fromZZ2     :: Z.Fp  2     } deriving (Eq,Show)  
newtype ZZ3     = ZZ3     { fromZZ3     :: Z.Fp  3     } deriving (Eq,Show)  
newtype ZZ5     = ZZ5     { fromZZ5     :: Z.Fp  5     } deriving (Eq,Show)  
newtype ZZ7     = ZZ7     { fromZZ7     :: Z.Fp  7     } deriving (Eq,Show)  
newtype ZZ11    = ZZ11    { fromZZ11    :: Z.Fp  11    } deriving (Eq,Show)  
newtype ZZ13    = ZZ13    { fromZZ13    :: Z.Fp  13    } deriving (Eq,Show)  
newtype ZZ17    = ZZ17    { fromZZ17    :: Z.Fp  17    } deriving (Eq,Show)  
newtype ZZ229   = ZZ229   { fromZZ229   :: Z.Fp  229   } deriving (Eq,Show)  
newtype ZZ257   = ZZ257   { fromZZ257   :: Z.Fp  257   } deriving (Eq,Show)  
newtype ZZ7919  = ZZ7919  { fromZZ7919  :: Z.Fp  7919  } deriving (Eq,Show)  
newtype ZZ32003 = ZZ32003 { fromZZ32003 :: Z.Fp  32003 } deriving (Eq,Show)  

newtype S2      = S2      { fromS2      :: S.Fp  2     } deriving (Eq,Show) 
newtype S3      = S3      { fromS3      :: S.Fp  3     } deriving (Eq,Show) 
newtype S5      = S5      { fromS5      :: S.Fp  5     } deriving (Eq,Show) 
newtype S7      = S7      { fromS7      :: S.Fp  7     } deriving (Eq,Show) 
newtype S11     = S11     { fromS11     :: S.Fp  11    } deriving (Eq,Show) 
newtype S13     = S13     { fromS13     :: S.Fp  13    } deriving (Eq,Show) 
newtype S17     = S17     { fromS17     :: S.Fp  17    } deriving (Eq,Show) 
newtype S229    = S229    { fromS229    :: S.Fp  229   } deriving (Eq,Show) 
newtype S257    = S257    { fromS257    :: S.Fp  257   } deriving (Eq,Show) 
newtype S7919   = S7919   { fromS7919   :: S.Fp  7919  } deriving (Eq,Show) 
newtype S32003  = S32003  { fromS32003  :: S.Fp  32003 } deriving (Eq,Show) 

newtype GF_2    = GF_2    { fromGF_2    :: G.GF 2   1  } deriving (Eq,Show) 
newtype GF_3    = GF_3    { fromGF_3    :: G.GF 3   1  } deriving (Eq,Show) 
newtype GF_5    = GF_5    { fromGF_5    :: G.GF 5   1  } deriving (Eq,Show) 
newtype GF_7    = GF_7    { fromGF_7    :: G.GF 7   1  } deriving (Eq,Show) 
newtype GF_17   = GF_17   { fromGF_17   :: G.GF 17  1  } deriving (Eq,Show) 
newtype GF_257  = GF_257  { fromGF_257  :: G.GF 257 1  } deriving (Eq,Show) 

newtype GF_2_1  = GF_2_1  { fromGF_2_1  :: G.GF 2 1    } deriving (Eq,Show) 
newtype GF_2_2  = GF_2_2  { fromGF_2_2  :: G.GF 2 2    } deriving (Eq,Show) 
newtype GF_2_3  = GF_2_3  { fromGF_2_3  :: G.GF 2 3    } deriving (Eq,Show) 
newtype GF_2_4  = GF_2_4  { fromGF_2_4  :: G.GF 2 4    } deriving (Eq,Show) 
newtype GF_2_5  = GF_2_5  { fromGF_2_5  :: G.GF 2 5    } deriving (Eq,Show) 
newtype GF_2_6  = GF_2_6  { fromGF_2_6  :: G.GF 2 6    } deriving (Eq,Show) 

newtype GF_3_1  = GF_3_1  { fromGF_3_1  :: G.GF 3 1    } deriving (Eq,Show) 
newtype GF_3_2  = GF_3_2  { fromGF_3_2  :: G.GF 3 2    } deriving (Eq,Show) 
newtype GF_3_3  = GF_3_3  { fromGF_3_3  :: G.GF 3 3    } deriving (Eq,Show) 
newtype GF_3_4  = GF_3_4  { fromGF_3_4  :: G.GF 3 4    } deriving (Eq,Show) 
newtype GF_3_5  = GF_3_5  { fromGF_3_5  :: G.GF 3 5    } deriving (Eq,Show) 
newtype GF_3_6  = GF_3_6  { fromGF_3_6  :: G.GF 3 6    } deriving (Eq,Show) 

newtype GF_5_1  = GF_5_1  { fromGF_5_1  :: G.GF 5 1    } deriving (Eq,Show) 
newtype GF_5_2  = GF_5_2  { fromGF_5_2  :: G.GF 5 2    } deriving (Eq,Show) 
newtype GF_5_3  = GF_5_3  { fromGF_5_3  :: G.GF 5 3    } deriving (Eq,Show) 
newtype GF_5_4  = GF_5_4  { fromGF_5_4  :: G.GF 5 4    } deriving (Eq,Show) 
newtype GF_5_5  = GF_5_5  { fromGF_5_5  :: G.GF 5 5    } deriving (Eq,Show) 
newtype GF_5_6  = GF_5_6  { fromGF_5_6  :: G.GF 5 6    } deriving (Eq,Show) 

newtype Zech_2_1  = Zech_2_1  { fromZech_2_1  :: Zech.Zech 2 1 } deriving (Eq,Show) 
newtype Zech_2_2  = Zech_2_2  { fromZech_2_2  :: Zech.Zech 2 2 } deriving (Eq,Show) 
newtype Zech_2_3  = Zech_2_3  { fromZech_2_3  :: Zech.Zech 2 3 } deriving (Eq,Show) 
newtype Zech_2_4  = Zech_2_4  { fromZech_2_4  :: Zech.Zech 2 4 } deriving (Eq,Show) 
newtype Zech_2_5  = Zech_2_5  { fromZech_2_5  :: Zech.Zech 2 5 } deriving (Eq,Show) 
newtype Zech_2_6  = Zech_2_6  { fromZech_2_6  :: Zech.Zech 2 6 } deriving (Eq,Show) 

newtype Zech_3_1  = Zech_3_1  { fromZech_3_1  :: Zech.Zech 3 1 } deriving (Eq,Show) 
newtype Zech_3_2  = Zech_3_2  { fromZech_3_2  :: Zech.Zech 3 2 } deriving (Eq,Show) 
newtype Zech_3_3  = Zech_3_3  { fromZech_3_3  :: Zech.Zech 3 3 } deriving (Eq,Show) 
newtype Zech_3_4  = Zech_3_4  { fromZech_3_4  :: Zech.Zech 3 4 } deriving (Eq,Show) 
newtype Zech_3_5  = Zech_3_5  { fromZech_3_5  :: Zech.Zech 3 5 } deriving (Eq,Show) 
newtype Zech_3_6  = Zech_3_6  { fromZech_3_6  :: Zech.Zech 3 6 } deriving (Eq,Show) 

newtype Zech_13_1  = Zech_13_1  { fromZech_13_1  :: Zech.Zech 13 1 } deriving (Eq,Show) 
newtype Zech_13_2  = Zech_13_2  { fromZech_13_2  :: Zech.Zech 13 2 } deriving (Eq,Show) 
newtype Zech_13_3  = Zech_13_3  { fromZech_13_3  :: Zech.Zech 13 3 } deriving (Eq,Show) 
newtype Zech_13_4  = Zech_13_4  { fromZech_13_4  :: Zech.Zech 13 4 } deriving (Eq,Show) 
newtype Zech_13_5  = Zech_13_5  { fromZech_13_5  :: Zech.Zech 13 5 } deriving (Eq,Show) 
newtype Zech_13_6  = Zech_13_6  { fromZech_13_6  :: Zech.Zech 13 6 } deriving (Eq,Show) 

--------------------------------------------------------------------------------

mkGen :: (forall g. RandomGen g => (g -> (a,g))) -> Gen a
mkGen f = MkGen (\r _ -> let (x,_) = f r in x)

instance Arbitrary ZZ2     where arbitrary = ZZ2     <$> mkGen (randomFieldElem zz2    )
instance Arbitrary ZZ3     where arbitrary = ZZ3     <$> mkGen (randomFieldElem zz3    )
instance Arbitrary ZZ5     where arbitrary = ZZ5     <$> mkGen (randomFieldElem zz5    )
instance Arbitrary ZZ7     where arbitrary = ZZ7     <$> mkGen (randomFieldElem zz7    )
instance Arbitrary ZZ11    where arbitrary = ZZ11    <$> mkGen (randomFieldElem zz11   )
instance Arbitrary ZZ13    where arbitrary = ZZ13    <$> mkGen (randomFieldElem zz13   )
instance Arbitrary ZZ17    where arbitrary = ZZ17    <$> mkGen (randomFieldElem zz17   )
instance Arbitrary ZZ229   where arbitrary = ZZ229   <$> mkGen (randomFieldElem zz229  )
instance Arbitrary ZZ257   where arbitrary = ZZ257   <$> mkGen (randomFieldElem zz257  )
instance Arbitrary ZZ7919  where arbitrary = ZZ7919  <$> mkGen (randomFieldElem zz7919 )
instance Arbitrary ZZ32003 where arbitrary = ZZ32003 <$> mkGen (randomFieldElem zz32003)

instance Arbitrary S2      where arbitrary = S2      <$> mkGen (randomFieldElem s2     )
instance Arbitrary S3      where arbitrary = S3      <$> mkGen (randomFieldElem s3     )
instance Arbitrary S5      where arbitrary = S5      <$> mkGen (randomFieldElem s5     )
instance Arbitrary S7      where arbitrary = S7      <$> mkGen (randomFieldElem s7     )
instance Arbitrary S11     where arbitrary = S11     <$> mkGen (randomFieldElem s11    )
instance Arbitrary S13     where arbitrary = S13     <$> mkGen (randomFieldElem s13    )
instance Arbitrary S17     where arbitrary = S17     <$> mkGen (randomFieldElem s17    )
instance Arbitrary S229    where arbitrary = S229    <$> mkGen (randomFieldElem s229   )
instance Arbitrary S257    where arbitrary = S257    <$> mkGen (randomFieldElem s257   )
instance Arbitrary S7919   where arbitrary = S7919   <$> mkGen (randomFieldElem s7919  )
instance Arbitrary S32003  where arbitrary = S32003  <$> mkGen (randomFieldElem s32003 )

instance Arbitrary GF_2    where arbitrary = GF_2    <$> mkGen (randomFieldElem gf_2   )
instance Arbitrary GF_3    where arbitrary = GF_3    <$> mkGen (randomFieldElem gf_3   )
instance Arbitrary GF_5    where arbitrary = GF_5    <$> mkGen (randomFieldElem gf_5   )
instance Arbitrary GF_7    where arbitrary = GF_7    <$> mkGen (randomFieldElem gf_7   )
instance Arbitrary GF_17   where arbitrary = GF_17   <$> mkGen (randomFieldElem gf_17  )
instance Arbitrary GF_257  where arbitrary = GF_257  <$> mkGen (randomFieldElem gf_257 )

instance Arbitrary GF_2_1  where arbitrary = GF_2_1  <$> mkGen (randomFieldElem gf_2_1 )
instance Arbitrary GF_2_2  where arbitrary = GF_2_2  <$> mkGen (randomFieldElem gf_2_2 )
instance Arbitrary GF_2_3  where arbitrary = GF_2_3  <$> mkGen (randomFieldElem gf_2_3 )
instance Arbitrary GF_2_4  where arbitrary = GF_2_4  <$> mkGen (randomFieldElem gf_2_4 )
instance Arbitrary GF_2_5  where arbitrary = GF_2_5  <$> mkGen (randomFieldElem gf_2_5 )
instance Arbitrary GF_2_6  where arbitrary = GF_2_6  <$> mkGen (randomFieldElem gf_2_6 )

instance Arbitrary GF_3_1  where arbitrary = GF_3_1  <$> mkGen (randomFieldElem gf_3_1 )
instance Arbitrary GF_3_2  where arbitrary = GF_3_2  <$> mkGen (randomFieldElem gf_3_2 )
instance Arbitrary GF_3_3  where arbitrary = GF_3_3  <$> mkGen (randomFieldElem gf_3_3 )
instance Arbitrary GF_3_4  where arbitrary = GF_3_4  <$> mkGen (randomFieldElem gf_3_4 )
instance Arbitrary GF_3_5  where arbitrary = GF_3_5  <$> mkGen (randomFieldElem gf_3_5 )
instance Arbitrary GF_3_6  where arbitrary = GF_3_6  <$> mkGen (randomFieldElem gf_3_6 )

instance Arbitrary GF_5_1  where arbitrary = GF_5_1  <$> mkGen (randomFieldElem gf_5_1 )
instance Arbitrary GF_5_2  where arbitrary = GF_5_2  <$> mkGen (randomFieldElem gf_5_2 )
instance Arbitrary GF_5_3  where arbitrary = GF_5_3  <$> mkGen (randomFieldElem gf_5_3 )
instance Arbitrary GF_5_4  where arbitrary = GF_5_4  <$> mkGen (randomFieldElem gf_5_4 )
instance Arbitrary GF_5_5  where arbitrary = GF_5_5  <$> mkGen (randomFieldElem gf_5_5 )
instance Arbitrary GF_5_6  where arbitrary = GF_5_6  <$> mkGen (randomFieldElem gf_5_6 )

instance Arbitrary Zech_2_1  where arbitrary = Zech_2_1  <$> mkGen (randomFieldElem zech_2_1 )
instance Arbitrary Zech_2_2  where arbitrary = Zech_2_2  <$> mkGen (randomFieldElem zech_2_2 )
instance Arbitrary Zech_2_3  where arbitrary = Zech_2_3  <$> mkGen (randomFieldElem zech_2_3 )
instance Arbitrary Zech_2_4  where arbitrary = Zech_2_4  <$> mkGen (randomFieldElem zech_2_4 )
instance Arbitrary Zech_2_5  where arbitrary = Zech_2_5  <$> mkGen (randomFieldElem zech_2_5 )
instance Arbitrary Zech_2_6  where arbitrary = Zech_2_6  <$> mkGen (randomFieldElem zech_2_6 )

instance Arbitrary Zech_3_1  where arbitrary = Zech_3_1  <$> mkGen (randomFieldElem zech_3_1 )
instance Arbitrary Zech_3_2  where arbitrary = Zech_3_2  <$> mkGen (randomFieldElem zech_3_2 )
instance Arbitrary Zech_3_3  where arbitrary = Zech_3_3  <$> mkGen (randomFieldElem zech_3_3 )
instance Arbitrary Zech_3_4  where arbitrary = Zech_3_4  <$> mkGen (randomFieldElem zech_3_4 )
instance Arbitrary Zech_3_5  where arbitrary = Zech_3_5  <$> mkGen (randomFieldElem zech_3_5 )
instance Arbitrary Zech_3_6  where arbitrary = Zech_3_6  <$> mkGen (randomFieldElem zech_3_6 )

instance Arbitrary Zech_13_1  where arbitrary = Zech_13_1  <$> mkGen (randomFieldElem zech_13_1 )
instance Arbitrary Zech_13_2  where arbitrary = Zech_13_2  <$> mkGen (randomFieldElem zech_13_2 )
instance Arbitrary Zech_13_3  where arbitrary = Zech_13_3  <$> mkGen (randomFieldElem zech_13_3 )
instance Arbitrary Zech_13_4  where arbitrary = Zech_13_4  <$> mkGen (randomFieldElem zech_13_4 )
instance Arbitrary Zech_13_5  where arbitrary = Zech_13_5  <$> mkGen (randomFieldElem zech_13_5 )
instance Arbitrary Zech_13_6  where arbitrary = Zech_13_6  <$> mkGen (randomFieldElem zech_13_6 )

--------------------------------------------------------------------------------

instance Arbitrary (NonZero ZZ2    ) where arbitrary = (NonZero . ZZ2    ) <$> mkGen (randomInvertible zz2    )
instance Arbitrary (NonZero ZZ3    ) where arbitrary = (NonZero . ZZ3    ) <$> mkGen (randomInvertible zz3    )
instance Arbitrary (NonZero ZZ5    ) where arbitrary = (NonZero . ZZ5    ) <$> mkGen (randomInvertible zz5    )
instance Arbitrary (NonZero ZZ7    ) where arbitrary = (NonZero . ZZ7    ) <$> mkGen (randomInvertible zz7    )
instance Arbitrary (NonZero ZZ11   ) where arbitrary = (NonZero . ZZ11   ) <$> mkGen (randomInvertible zz11   )
instance Arbitrary (NonZero ZZ13   ) where arbitrary = (NonZero . ZZ13   ) <$> mkGen (randomInvertible zz13   )
instance Arbitrary (NonZero ZZ17   ) where arbitrary = (NonZero . ZZ17   ) <$> mkGen (randomInvertible zz17   )
instance Arbitrary (NonZero ZZ229  ) where arbitrary = (NonZero . ZZ229  ) <$> mkGen (randomInvertible zz229  )
instance Arbitrary (NonZero ZZ257  ) where arbitrary = (NonZero . ZZ257  ) <$> mkGen (randomInvertible zz257  )
instance Arbitrary (NonZero ZZ7919 ) where arbitrary = (NonZero . ZZ7919 ) <$> mkGen (randomInvertible zz7919 )
instance Arbitrary (NonZero ZZ32003) where arbitrary = (NonZero . ZZ32003) <$> mkGen (randomInvertible zz32003)

instance Arbitrary (NonZero S2     ) where arbitrary = (NonZero . S2     ) <$> mkGen (randomInvertible s2     )
instance Arbitrary (NonZero S3     ) where arbitrary = (NonZero . S3     ) <$> mkGen (randomInvertible s3     )
instance Arbitrary (NonZero S5     ) where arbitrary = (NonZero . S5     ) <$> mkGen (randomInvertible s5     )
instance Arbitrary (NonZero S7     ) where arbitrary = (NonZero . S7     ) <$> mkGen (randomInvertible s7     )
instance Arbitrary (NonZero S11    ) where arbitrary = (NonZero . S11    ) <$> mkGen (randomInvertible s11    )
instance Arbitrary (NonZero S13    ) where arbitrary = (NonZero . S13    ) <$> mkGen (randomInvertible s13    )
instance Arbitrary (NonZero S17    ) where arbitrary = (NonZero . S17    ) <$> mkGen (randomInvertible s17    )
instance Arbitrary (NonZero S229   ) where arbitrary = (NonZero . S229   ) <$> mkGen (randomInvertible s229   )
instance Arbitrary (NonZero S257   ) where arbitrary = (NonZero . S257   ) <$> mkGen (randomInvertible s257   )
instance Arbitrary (NonZero S7919  ) where arbitrary = (NonZero . S7919  ) <$> mkGen (randomInvertible s7919  )
instance Arbitrary (NonZero S32003 ) where arbitrary = (NonZero . S32003 ) <$> mkGen (randomInvertible s32003 )

instance Arbitrary (NonZero GF_2   ) where arbitrary = (NonZero . GF_2   ) <$> mkGen (randomInvertible gf_2   )
instance Arbitrary (NonZero GF_3   ) where arbitrary = (NonZero . GF_3   ) <$> mkGen (randomInvertible gf_3   )
instance Arbitrary (NonZero GF_5   ) where arbitrary = (NonZero . GF_5   ) <$> mkGen (randomInvertible gf_5   )
instance Arbitrary (NonZero GF_7   ) where arbitrary = (NonZero . GF_7   ) <$> mkGen (randomInvertible gf_7   )
instance Arbitrary (NonZero GF_17  ) where arbitrary = (NonZero . GF_17  ) <$> mkGen (randomInvertible gf_17  )
instance Arbitrary (NonZero GF_257 ) where arbitrary = (NonZero . GF_257 ) <$> mkGen (randomInvertible gf_257 )

instance Arbitrary (NonZero GF_2_1 ) where arbitrary = (NonZero . GF_2_1 ) <$> mkGen (randomInvertible gf_2_1 )
instance Arbitrary (NonZero GF_2_2 ) where arbitrary = (NonZero . GF_2_2 ) <$> mkGen (randomInvertible gf_2_2 )
instance Arbitrary (NonZero GF_2_3 ) where arbitrary = (NonZero . GF_2_3 ) <$> mkGen (randomInvertible gf_2_3 )
instance Arbitrary (NonZero GF_2_4 ) where arbitrary = (NonZero . GF_2_4 ) <$> mkGen (randomInvertible gf_2_4 )
instance Arbitrary (NonZero GF_2_5 ) where arbitrary = (NonZero . GF_2_5 ) <$> mkGen (randomInvertible gf_2_5 )
instance Arbitrary (NonZero GF_2_6 ) where arbitrary = (NonZero . GF_2_6 ) <$> mkGen (randomInvertible gf_2_6 )

instance Arbitrary (NonZero GF_3_1 ) where arbitrary = (NonZero . GF_3_1 ) <$> mkGen (randomInvertible gf_3_1 )
instance Arbitrary (NonZero GF_3_2 ) where arbitrary = (NonZero . GF_3_2 ) <$> mkGen (randomInvertible gf_3_2 )
instance Arbitrary (NonZero GF_3_3 ) where arbitrary = (NonZero . GF_3_3 ) <$> mkGen (randomInvertible gf_3_3 )
instance Arbitrary (NonZero GF_3_4 ) where arbitrary = (NonZero . GF_3_4 ) <$> mkGen (randomInvertible gf_3_4 )
instance Arbitrary (NonZero GF_3_5 ) where arbitrary = (NonZero . GF_3_5 ) <$> mkGen (randomInvertible gf_3_5 )
instance Arbitrary (NonZero GF_3_6 ) where arbitrary = (NonZero . GF_3_6 ) <$> mkGen (randomInvertible gf_3_6 )

instance Arbitrary (NonZero GF_5_1 ) where arbitrary = (NonZero . GF_5_1 ) <$> mkGen (randomInvertible gf_5_1 )
instance Arbitrary (NonZero GF_5_2 ) where arbitrary = (NonZero . GF_5_2 ) <$> mkGen (randomInvertible gf_5_2 )
instance Arbitrary (NonZero GF_5_3 ) where arbitrary = (NonZero . GF_5_3 ) <$> mkGen (randomInvertible gf_5_3 )
instance Arbitrary (NonZero GF_5_4 ) where arbitrary = (NonZero . GF_5_4 ) <$> mkGen (randomInvertible gf_5_4 )
instance Arbitrary (NonZero GF_5_5 ) where arbitrary = (NonZero . GF_5_5 ) <$> mkGen (randomInvertible gf_5_5 )
instance Arbitrary (NonZero GF_5_6 ) where arbitrary = (NonZero . GF_5_6 ) <$> mkGen (randomInvertible gf_5_6 )

instance Arbitrary (NonZero Zech_2_1 ) where arbitrary = (NonZero . Zech_2_1 ) <$> mkGen (randomInvertible zech_2_1 )
instance Arbitrary (NonZero Zech_2_2 ) where arbitrary = (NonZero . Zech_2_2 ) <$> mkGen (randomInvertible zech_2_2 )
instance Arbitrary (NonZero Zech_2_3 ) where arbitrary = (NonZero . Zech_2_3 ) <$> mkGen (randomInvertible zech_2_3 )
instance Arbitrary (NonZero Zech_2_4 ) where arbitrary = (NonZero . Zech_2_4 ) <$> mkGen (randomInvertible zech_2_4 )
instance Arbitrary (NonZero Zech_2_5 ) where arbitrary = (NonZero . Zech_2_5 ) <$> mkGen (randomInvertible zech_2_5 )
instance Arbitrary (NonZero Zech_2_6 ) where arbitrary = (NonZero . Zech_2_6 ) <$> mkGen (randomInvertible zech_2_6 )

instance Arbitrary (NonZero Zech_3_1 ) where arbitrary = (NonZero . Zech_3_1 ) <$> mkGen (randomInvertible zech_3_1 )
instance Arbitrary (NonZero Zech_3_2 ) where arbitrary = (NonZero . Zech_3_2 ) <$> mkGen (randomInvertible zech_3_2 )
instance Arbitrary (NonZero Zech_3_3 ) where arbitrary = (NonZero . Zech_3_3 ) <$> mkGen (randomInvertible zech_3_3 )
instance Arbitrary (NonZero Zech_3_4 ) where arbitrary = (NonZero . Zech_3_4 ) <$> mkGen (randomInvertible zech_3_4 )
instance Arbitrary (NonZero Zech_3_5 ) where arbitrary = (NonZero . Zech_3_5 ) <$> mkGen (randomInvertible zech_3_5 )
instance Arbitrary (NonZero Zech_3_6 ) where arbitrary = (NonZero . Zech_3_6 ) <$> mkGen (randomInvertible zech_3_6 )

instance Arbitrary (NonZero Zech_13_1 ) where arbitrary = (NonZero . Zech_13_1 ) <$> mkGen (randomInvertible zech_13_1 )
instance Arbitrary (NonZero Zech_13_2 ) where arbitrary = (NonZero . Zech_13_2 ) <$> mkGen (randomInvertible zech_13_2 )
instance Arbitrary (NonZero Zech_13_3 ) where arbitrary = (NonZero . Zech_13_3 ) <$> mkGen (randomInvertible zech_13_3 )
instance Arbitrary (NonZero Zech_13_4 ) where arbitrary = (NonZero . Zech_13_4 ) <$> mkGen (randomInvertible zech_13_4 )
instance Arbitrary (NonZero Zech_13_5 ) where arbitrary = (NonZero . Zech_13_5 ) <$> mkGen (randomInvertible zech_13_5 )
instance Arbitrary (NonZero Zech_13_6 ) where arbitrary = (NonZero . Zech_13_6 ) <$> mkGen (randomInvertible zech_13_6 )

--------------------------------------------------------------------------------

zz2     = Z.WitnessFp (believeMeItsPrime (unsafeSNat 2    )) :: Z.WitnessFp 2    
zz3     = Z.WitnessFp (believeMeItsPrime (unsafeSNat 3    )) :: Z.WitnessFp 3    
zz5     = Z.WitnessFp (believeMeItsPrime (unsafeSNat 5    )) :: Z.WitnessFp 5    
zz7     = Z.WitnessFp (believeMeItsPrime (unsafeSNat 7    )) :: Z.WitnessFp 7    
zz11    = Z.WitnessFp (believeMeItsPrime (unsafeSNat 11   )) :: Z.WitnessFp 11   
zz13    = Z.WitnessFp (believeMeItsPrime (unsafeSNat 13   )) :: Z.WitnessFp 13   
zz17    = Z.WitnessFp (believeMeItsPrime (unsafeSNat 17   )) :: Z.WitnessFp 17   
zz229   = Z.WitnessFp (believeMeItsPrime (unsafeSNat 229  )) :: Z.WitnessFp 229  
zz257   = Z.WitnessFp (believeMeItsPrime (unsafeSNat 257  )) :: Z.WitnessFp 257  
zz7919  = Z.WitnessFp (believeMeItsPrime (unsafeSNat 7919 )) :: Z.WitnessFp 7919 
zz32003 = Z.WitnessFp (believeMeItsPrime (unsafeSNat 32003)) :: Z.WitnessFp 32003

s2     = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 2    )) :: S.WitnessFp 2    
s3     = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 3    )) :: S.WitnessFp 3    
s5     = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 5    )) :: S.WitnessFp 5    
s7     = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 7    )) :: S.WitnessFp 7    
s11    = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 11   )) :: S.WitnessFp 11   
s13    = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 13   )) :: S.WitnessFp 13   
s17    = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 17   )) :: S.WitnessFp 17   
s229   = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 229  )) :: S.WitnessFp 229  
s257   = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 257  )) :: S.WitnessFp 257  
s7919  = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 7919 )) :: S.WitnessFp 7919 
s32003 = S.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 32003)) :: S.WitnessFp 32003

gf_2   = G.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 2  )) :: G.WitnessGF 2   1
gf_3   = G.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 3  )) :: G.WitnessGF 3   1
gf_5   = G.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 5  )) :: G.WitnessGF 5   1
gf_7   = G.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 7  )) :: G.WitnessGF 7   1
gf_13  = G.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 13 )) :: G.WitnessGF 13  1
gf_17  = G.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 17 )) :: G.WitnessGF 17  1
gf_257 = G.WitnessFp (believeMeItsASmallPrime (unsafeSNat64 257)) :: G.WitnessGF 257 1

gf_2_1 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 2) (unsafeSNat64 1)) :: G.WitnessGF 2 1
gf_2_2 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 2) (unsafeSNat64 2)) :: G.WitnessGF 2 2
gf_2_3 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 2) (unsafeSNat64 3)) :: G.WitnessGF 2 3
gf_2_4 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 2) (unsafeSNat64 4)) :: G.WitnessGF 2 4
gf_2_5 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 2) (unsafeSNat64 5)) :: G.WitnessGF 2 5
gf_2_6 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 2) (unsafeSNat64 6)) :: G.WitnessGF 2 6

gf_3_1 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 3) (unsafeSNat64 1)) :: G.WitnessGF 3 1
gf_3_2 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 3) (unsafeSNat64 2)) :: G.WitnessGF 3 2
gf_3_3 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 3) (unsafeSNat64 3)) :: G.WitnessGF 3 3
gf_3_4 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 3) (unsafeSNat64 4)) :: G.WitnessGF 3 4
gf_3_5 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 3) (unsafeSNat64 5)) :: G.WitnessGF 3 5
gf_3_6 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 3) (unsafeSNat64 6)) :: G.WitnessGF 3 6

gf_5_1 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 5) (unsafeSNat64 1)) :: G.WitnessGF 5 1
gf_5_2 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 5) (unsafeSNat64 2)) :: G.WitnessGF 5 2
gf_5_3 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 5) (unsafeSNat64 3)) :: G.WitnessGF 5 3
gf_5_4 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 5) (unsafeSNat64 4)) :: G.WitnessGF 5 4
gf_5_5 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 5) (unsafeSNat64 5)) :: G.WitnessGF 5 5
gf_5_6 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 5) (unsafeSNat64 6)) :: G.WitnessGF 5 6

gf_13_1 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 13) (unsafeSNat64 1)) :: G.WitnessGF 13 1
gf_13_2 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 13) (unsafeSNat64 2)) :: G.WitnessGF 13 2
gf_13_3 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 13) (unsafeSNat64 3)) :: G.WitnessGF 13 3
gf_13_4 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 13) (unsafeSNat64 4)) :: G.WitnessGF 13 4
gf_13_5 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 13) (unsafeSNat64 5)) :: G.WitnessGF 13 5
gf_13_6 = G.WitnessFq (unsafeLookupConwayPoly (unsafeSNat64 13) (unsafeSNat64 6)) :: G.WitnessGF 13 6

zech_2_1 = Zech.WitnessZech (makeZechTable gf_2  ) :: Zech.WitnessZech 2 1
zech_2_2 = Zech.WitnessZech (makeZechTable gf_2_2) :: Zech.WitnessZech 2 2
zech_2_3 = Zech.WitnessZech (makeZechTable gf_2_3) :: Zech.WitnessZech 2 3
zech_2_4 = Zech.WitnessZech (makeZechTable gf_2_4) :: Zech.WitnessZech 2 4
zech_2_5 = Zech.WitnessZech (makeZechTable gf_2_5) :: Zech.WitnessZech 2 5
zech_2_6 = Zech.WitnessZech (makeZechTable gf_2_6) :: Zech.WitnessZech 2 6

zech_3_1 = Zech.WitnessZech (makeZechTable gf_3  ) :: Zech.WitnessZech 3 1
zech_3_2 = Zech.WitnessZech (makeZechTable gf_3_2) :: Zech.WitnessZech 3 2
zech_3_3 = Zech.WitnessZech (makeZechTable gf_3_3) :: Zech.WitnessZech 3 3
zech_3_4 = Zech.WitnessZech (makeZechTable gf_3_4) :: Zech.WitnessZech 3 4
zech_3_5 = Zech.WitnessZech (makeZechTable gf_3_5) :: Zech.WitnessZech 3 5
zech_3_6 = Zech.WitnessZech (makeZechTable gf_3_6) :: Zech.WitnessZech 3 6

zech_13_1 = Zech.WitnessZech (makeZechTable gf_13  ) :: Zech.WitnessZech 13 1
zech_13_2 = Zech.WitnessZech (makeZechTable gf_13_2) :: Zech.WitnessZech 13 2
zech_13_3 = Zech.WitnessZech (makeZechTable gf_13_3) :: Zech.WitnessZech 13 3
zech_13_4 = Zech.WitnessZech (makeZechTable gf_13_4) :: Zech.WitnessZech 13 4
zech_13_5 = Zech.WitnessZech (makeZechTable gf_13_5) :: Zech.WitnessZech 13 5
zech_13_6 = Zech.WitnessZech (makeZechTable gf_13_6) :: Zech.WitnessZech 13 6

--------------------------------------------------------------------------------
