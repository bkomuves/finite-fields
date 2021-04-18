
-- | Some concrete fields
--
-- It's problematic to have an Arbitrary instance to depend on some
-- external data, so to start with something I'll just have some concrete fields 
-- to test in...
--

{-# LANGUAGE DataKinds, Rank2Types, TypeApplications #-}
module Test.ConcreteFields where

--------------------------------------------------------------------------------

import System.Random

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Math.FiniteField.Class
import Math.FiniteField.Conway
import Math.FiniteField.TypeLevel

import Math.FiniteField.PrimeField.Generic as Z
import Math.FiniteField.PrimeField.Small   as S
import Math.FiniteField.GaloisField.Small  as G

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

-- gf_2_1 =
-- gf_2_2 =
-- gf_2_3 =

--------------------------------------------------------------------------------
