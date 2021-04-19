
-- | Small Galois fields via a precomputed table of Conway polynomials.
--
-- This covers:
--
-- * all fields with order <= 2^30
--
-- * all fields with characteristic < 2^16 and order < 2^64 (?)
--
-- * higher powers for very small prime characteristic
--
-- * some more
--
-- To look up Conway polynomials, see the module "Math.FiniteField.Conway".
--

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, GADTs, TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Math.FiniteField.GaloisField.Small where

--------------------------------------------------------------------------------

import Prelude hiding (div)

import Data.Bits
import Data.Int
import Data.Word
import Data.List

import GHC.TypeNats (Nat)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec

import System.Random ( RandomGen , randomR )

import Math.FiniteField.Class 
import Math.FiniteField.TypeLevel
import Math.FiniteField.Conway
import Math.FiniteField.Primes
import Math.FiniteField.Misc

import qualified Math.FiniteField.PrimeField.Small.Raw       as Raw
import qualified Math.FiniteField.GaloisField.Small.Internal as Quo

--------------------------------------------------------------------------------  
-- * Witness for the existence of GF(q^m)

-- | We need either a Conway polynomial, or in the @m=1@ case, a proof that @p@ is prime
data WitnessGF (p :: Nat) (m :: Nat) where
  WitnessFp :: IsSmallPrime  p   -> WitnessGF p 1
  WitnessFq :: HasConwayPoly p m -> WitnessGF p m

deriving instance Show (WitnessGF p m)

gfParams :: WitnessGF p m -> (Word64,Int)
gfParams w = case w of
  WitnessFp p -> (fromSmallPrime p, 1)
  WitnessFq c -> conwayParams c

data SomeWitnessGF 
  = forall p m. SomeWitnessGF (WitnessGF p m)

deriving instance Show SomeWitnessGF

mkGaloisField :: Int -> Int -> Maybe SomeWitnessGF
mkGaloisField p m = case m of
  1  -> case someSNat64 (fromIntegral p) of 
          SomeSNat64 sp -> (SomeWitnessGF . WitnessFp) <$> isSmallPrime sp 
  _  -> case lookupSomeConwayPoly p m of 
          Nothing -> Nothing
          Just (SomeConwayPoly cw) -> Just (SomeWitnessGF (WitnessFq cw))

--------------------------------------------------------------------------------  

-- | An element of the Galois field of order @q = p^m@
data GF (p :: Nat) (m :: Nat) where
  Fp :: {-# UNPACK #-} !(IsSmallPrime  p  ) -> {-# UNPACK #-} !Word64          -> GF p 1
  Fq :: {-# UNPACK #-} !(HasConwayPoly p m) ->                !(Vector Word32) -> GF p m

-- | An alias for @GF p m@, that is, the elements of the Galois field of order @q = p^m@
type Fq p m = GF p m

fqWitness :: GF p m -> WitnessGF p m
fqWitness element = case element of
  Fp p _ -> WitnessFp p
  Fq c _ -> WitnessFq c

-- | An element of the prime field
fp :: WitnessGF p m -> Word64 -> GF p m
fp witness x = 
  case witness of
    WitnessFp p -> fp1 p x 
    WitnessFq c -> fpM c x

  where
    fpM :: HasConwayPoly p m -> Word64 -> GF p m
    fpM conway x = Fq conway (Vec.fromListN m (y : replicate (m-1) 0)) where
      (p,m) = conwayParams conway
      y = if x >= 0 && x < p then fromIntegral x else fromIntegral (mod x p) :: Word32
    
    fp1 :: IsSmallPrime p -> Word64 -> GF p 1
    fp1 prime x = Fp prime y where
      p = fromSmallPrime prime 
      y = if x >= 0 && x < p then x else mod x p

fpIsZero :: GF p m -> Bool
fpIsZero (Fp _ x) = x == 0
fpIsZero (Fq _ v) = all (==0) (Vec.toList v)

fpIsOne :: GF p m -> Bool
fpIsOne (Fp _ x) = x == 1
fpIsOne (Fq _ v) = case Vec.toList v of { (x:xs) -> x==1 && all (==0) xs }

randomFq :: RandomGen gen => WitnessGF p m -> gen -> (GF p m, gen) 
randomFq witness gen = case witness of
  WitnessFp p -> 
    let !q = fromSmallPrime p 
    in  case randomR (0,q-1) gen of { (x, gen') -> (Fp p x, gen') } 
  WitnessFq c -> 
    let !(p,m) = conwayParams c
    in  case mapAccumL (\ !g _ -> swap (randomR (0,p-1) g) ) gen [1..m] of
          (gen' , xs) -> ( Fq c (Vec.fromList (map fromIntegral xs)) , gen' )

-- | The field element corresponding to the polynomial @X@ (which is a primitive generator)
gen :: HasConwayPoly p m -> GF p m
gen conway = gen' conway 1

-- | The field element corresponding to the polynomial @c*X@
gen' :: HasConwayPoly p m -> Word64 -> GF p m
gen' conway x = Fq conway (Vec.fromListN m (0 : y : replicate (m-2) 0)) where
  (p,m) = conwayParams conway
  y = fromIntegral (mod x p) :: Word32

primGenFq :: WitnessGF p m -> GF p m 
primGenFq !w = case w of
  WitnessFq cw -> gen cw
  WitnessFp pw -> prim where
    !p   = fromSmallPrime pw
    prim = case lookupConwayPrimRoot_ (fromIntegral p) of
      Just g       -> embedSmall w (fromIntegral g)
      Nothing      -> error "GaloisField/Fp/primGen: primitive root not found in the Conway polynomial table"

--------------------------------------------------------------------------------  

instance Eq (GF p m) where
  (==) (Fp _ x ) (Fp _ y ) = x == y
  (==) (Fq _ xs) (Fq _ ys) = xs == ys

-- | Note: the @Ord@ instance is present only so that you can use 'GF' as key
-- in @Maps@ - the ordering is kind of arbitrary! 
instance Ord (GF p m) where
  compare (Fp _ x ) (Fp _ y ) = compare x  y
  compare (Fq _ xs) (Fq _ ys) = compare xs ys

instance Show (GF p m) where
  show (Fp prime   x  ) = "<" ++ show x ++ " mod " ++ show (fromSmallPrime prime) ++ ">"
  show (Fq witness vec) = "<" ++ intercalate "+" list ++ " mod " ++ show p ++ ">" where
    (p,m) = conwayParams witness
    list = zipWith f [0..] (Vec.toList vec) 
    f  0 !v = show v
    f  1 !v = show v ++ "*g"
    f !e !v = show v ++ "*g^" ++ show e

instance Num (GF p m) where
  fromInteger = error "GF/fromInteger: cannot be defined; use `embed` instead" 
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  signum = kst1

instance Fractional (GF p m) where
  fromRational = error "GF/fromRational: cannot be defined; use `embed` instead" 
  recip = inv 
  (/)   = div 

instance Field (GF p m) where
  type Witness (GF p m) = WitnessGF p m
  characteristic   !w = fromIntegral (fst (gfParams w))
  dimension        !w = fromIntegral (snd (gfParams w))
  fieldSize        !w = case gfParams w of (p,m) -> (fromIntegral p :: Integer) ^ m
  enumerate        !w = enumerateFq w
  witnessOf        !x = fqWitness x
  embed         !w !x = fp w (fromInteger  x)
  embedSmall    !w !x = fp w (fromIntegral x)
  randomFieldElem  !w = randomFq w
  primGen          !w = primGenFq w

  zero w = fp w 0
  one  w = fp w 1
  isZero = fpIsZero
  isOne  = fpIsOne

--------------------------------------------------------------------------------  
-- * Enumerations

-- | Enumerate all field elements (in a lexicographic order)
enumerateFq :: WitnessGF p m -> [GF p m]
enumerateFq witness = 
  case witness of
    WitnessFp p -> enumerateFp1 p
    WitnessFq c -> enumerateFpM c

  where
    enumerateFpM :: HasConwayPoly p m -> [GF p m]
    enumerateFpM conway = [ Fq conway vec | vec <- vecs ] where
      (p,m) = conwayParams conway
      shape = replicate m (fromIntegral (p-1) :: Word32)
      vecs = map (Vec.fromListN m) (word32Tuples' shape)
    
    enumerateFp1 :: IsSmallPrime p -> [GF p 1]
    enumerateFp1 prime = [ Fp prime k | k <- [0..fromSmallPrime prime-1] ]

--------------------------------------------------------------------------------  
-- * Field operations

kst0 :: GF p m -> GF p m
kst0 what = case what of
  Fp p _ -> fp (WitnessFp p) 0
  Fq c _ -> fp (WitnessFq c) 0

kst1 :: GF p m -> GF p m
kst1 what = case what of
  Fp p _ -> fp (WitnessFp p) 1
  Fq c _ -> fp (WitnessFq c) 1

neg :: GF p m -> GF p m 
neg (Fp p x ) = Fp p (Raw.neg (fromSmallPrime p) x ) 
neg (Fq c xs) = Fq c (Quo.neg (conwayPrime_   c) xs)

add :: GF p m -> GF p m -> GF p m
add (Fp p x ) (Fp _ y ) = Fp p (Raw.add (fromSmallPrime p) x  y )
add (Fq c xs) (Fq _ ys) = Fq c (Quo.add (conwayPrime_   c) xs ys)

sub :: GF p m -> GF p m -> GF p m
sub (Fp p x ) (Fp _ y ) = Fp p (Raw.sub (fromSmallPrime p) x  y ) 
sub (Fq c xs) (Fq _ ys) = Fq c (Quo.sub (conwayPrime_   c) xs ys)

mul :: GF p m -> GF p m -> GF p m
mul (Fp p x ) (Fp _ y ) = Fp p (Raw.mul (fromSmallPrime p) x  y ) 
mul (Fq c xs) (Fq _ ys) = Fq c (Quo.mul (fromConwayPoly c) xs ys)

inv :: GF p m -> GF p m 
inv (Fp p x ) = Fp p (Raw.inv                  (fromSmallPrime p) x ) 
inv (Fq c xs) = Fq c (Quo.inv (conwayPrime_ c) (fromConwayPoly c) xs)

div :: GF p m -> GF p m -> GF p m
div (Fp p x ) (Fp _ y ) = Fp p (Raw.div                  (fromSmallPrime p) x  y ) 
div (Fq c xs) (Fq _ ys) = Fq c (Quo.div (conwayPrime_ c) (fromConwayPoly c) xs ys)

--------------------------------------------------------------------------------
