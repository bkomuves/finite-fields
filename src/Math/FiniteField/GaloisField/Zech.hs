
-- | Small Galois fields via precomputed tables of Zech's logarithms.
--
-- <https://en.wikipedia.org/wiki/Zech%27s_logarithm>
--
-- When \"creating\" a field, we precompute the Zech logarithm table. 
-- After that, computations should be fast.
--
-- This is practical up to fields of size @10^5 -- 10^6@ 
--
-- TODO: also export the tables to C, and write C implementation of
-- of the field operations.
--

{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, ExistentialQuantification #-}

module Math.FiniteField.GaloisField.Zech where

--------------------------------------------------------------------------------

import Data.Int

import GHC.TypeNats (Nat)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Vector.Unboxed ( Vector , MVector )
import qualified Data.Vector.Unboxed as Vec

import System.Random ( RandomGen , randomR )

import Math.FiniteField.Class

import Math.FiniteField.GaloisField.Small ( GF , WitnessGF )
import qualified Math.FiniteField.GaloisField.Small as GF

--------------------------------------------------------------------------------

data ZechTable = ZechTable
  { _zechParams :: !(Int32,Int32)     -- ^ @(p,m)@
  , _qMinus1    :: !Int32             -- ^ @p^m-1 = q-1@ 
  , _logMinus1  :: !Int32             -- ^ an integer @e@ such that @g^e = -1@
  , _embedding  :: !(Vector Int32)    -- ^ embedding of F_p into F_q
  , _zechLogs   :: !(Vector Int32)    -- ^ Zech's logarithms
  }
  deriving Show

newtype WitnessZech (p :: Nat) (m :: Nat) 
  = WitnessZech { fromWitnessZech :: ZechTable }
  deriving Show

data SomeWitnessZech 
  = forall p m. SomeWitnessZech (WitnessZech p m)

deriving instance Show SomeWitnessZech

mkZechField :: Int -> Int -> Maybe SomeWitnessZech
mkZechField p m = case GF.mkGaloisField p m of 
  Nothing   -> Nothing
  Just some -> case some of
    GF.SomeWitnessGF wgf -> Just (SomeWitnessZech (WitnessZech (makeZechTable wgf)))

--------------------------------------------------------------------------------

data Zech (p :: Nat) (m :: Nat) = Zech !ZechTable {-# UNPACK #-} !Int32

instance Eq (Zech p m) where
  (==) (Zech _ k1) (Zech _ k2) = k1 == k2 

instance Ord (Zech p m) where
  compare (Zech _ k1) (Zech _ k2) = compare k1 k2

instance Show (Zech p m) where
  show (Zech _ n)
    | n == -1    = "0"
    | n ==  0    = "1"
    | n ==  1    = "g"
    | otherwise  = "g^" ++ show n

instance Num (Zech p m) where
  fromInteger = error "GaloisField/Zech/fromInteger: cannot be defined; use `embed` instead!"
  negate = zechNeg
  (+)    = zechAdd
  (-)    = zechSub
  (*)    = zechMul 
  abs    = id
  signum = error "GaloisField/Zech/signum: not implemented"

instance Fractional (Zech p m) where
  fromRational = error "GaloisField/Zech/fromRational: cannot be defined; use `embed` instead!"
  recip = zechInv
  (/)   = zechDiv

instance Field (Zech p m) where
  type Witness (Zech p m) = WitnessZech p m

  characteristic (WitnessZech !w) = fromIntegral (fst (_zechParams w))
  dimension      (WitnessZech !w) = fromIntegral (snd (_zechParams w))
  fieldSize      (WitnessZech !w) = case _zechParams w of (p,m) -> (fromIntegral p :: Integer) ^ m
  enumerate         w = enumerateZech w
  witnessOf        !x = case x of { Zech table _ -> WitnessZech table }
  embed         !w !x = embedZech w (fromInteger  x)
  embedSmall    !w !x = embedZech w x
  randomFieldElem   w = randomZech    w
  randomInvertible  w = randomInvZech w
  power               = zechPow

  zero    (WitnessZech w) = Zech w (-1)
  one     (WitnessZech w) = Zech w   0
  primGen (WitnessZech w) = Zech w   1
  isZero (Zech _ a) = a == -1
  isOne  (Zech _ a) = a == 0

--------------------------------------------------------------------------------

makeZechTable :: forall p m. WitnessGF p m -> ZechTable
makeZechTable witness = ZechTable (p,m) qm1 e embeds zechlogs where
  g   = primGen witness
  o   = one     witness
  p   = fromInteger (characteristic witness) :: Int32
  m   = fromInteger (dimension      witness) :: Int32
  q   = fromInteger (fieldSize      witness) :: Int32
  qm1 = q - 1
  e   = if p == 2 then 0 else Prelude.div qm1 2
  list = worker 0 (one witness) :: [(GF p m, Int32)]
  dlog = Map.fromList list
  worker !e !acc
    | e < qm1    = (acc,e) : worker (e+1) (acc*g)
    | otherwise  = []
  embeds   = Vec.fromList $ (-1) : [ (Map.!) dlog (embedSmall witness (fromIntegral i)) | i<-[1..p-1] ]
  zechlogs = Vec.fromList $ {- 0 : -} [ Map.findWithDefault (-1) (x + o) dlog | x <- map fst list ]

-- debugging 
printzech p m = case GF.mkGaloisField p m of 
  Just (GF.SomeWitnessGF field) -> print (makeZechTable field)

--------------------------------------------------------------------------------

embedZech :: WitnessZech p m -> Int -> Zech p m
embedZech (WitnessZech table) k
  | k >= 0 && k < p  = Zech table (Vec.unsafeIndex embeds      k   )
  | otherwise        = Zech table (Vec.unsafeIndex embeds (mod k p)) 
  where
    p = fromIntegral (fst (_zechParams table)) :: Int
    embeds = _embedding table

randomZech :: RandomGen gen => WitnessZech p m -> gen -> (Zech p m, gen)
randomZech (WitnessZech table) g = case randomR (-1,_qMinus1 table-1) g of
  (k,g') -> (Zech table k, g')

randomInvZech :: RandomGen gen => WitnessZech p m -> gen -> (Zech p m, gen)
randomInvZech (WitnessZech table) g = case randomR (0,_qMinus1 table-1) g of
  (k,g') -> (Zech table k, g')

enumerateZech :: WitnessZech p m -> [Zech p m]
enumerateZech (WitnessZech table) = [ Zech table i | i<-[-1..n-1] ] where
  n = _qMinus1 table

--------------------------------------------------------------------------------

zechNeg :: Zech p m -> Zech p m
zechNeg (Zech table a)
  | a == -1    = Zech table a
  | otherwise  = let n = _qMinus1  table
                     c = a + (_logMinus1 table)
                 in  if c < n then Zech table c else Zech table (c - n)

zechAdd :: Zech p m -> Zech p m -> Zech p m 
zechAdd (Zech table a) (Zech _ b) 
  | a == -1    = Zech table b     -- 0 + y
  | b == -1    = Zech table a     -- x + 0
  | otherwise  = let n    = _qMinus1  table
                     zech = _zechLogs table
                     plusMod k = if k < n then k else k - n
                 in  if a >= b 
                       then let d = Vec.unsafeIndex zech (fromIntegral (a - b))
                            in  if d < 0 then Zech table d 
                                         else Zech table (plusMod (b + d))
                       else let d = Vec.unsafeIndex zech (fromIntegral (b - a))
                            in  if d < 0 then Zech table d 
                                         else Zech table (plusMod (a + d))

zechSub :: Zech p m -> Zech p m -> Zech p m 
zechSub x y = zechAdd x (zechNeg y)

--------------------------------------------------------------------------------

zechMul :: Zech p m -> Zech p m -> Zech p m 
zechMul (Zech table a) (Zech _ b) 
  | a == -1    = Zech table a     -- 0 * y
  | b == -1    = Zech table b     -- y * 0
  | otherwise  = let n = _qMinus1 table
                     c = a + b
                 in  if c < n then Zech table c else Zech table (c - n)

zechPow :: Zech p m -> Integer -> Zech p m 
zechPow z@(Zech table a) e
  | e == 0     = Zech table 0     -- x^0 = 1
  | a == -1    = Zech table a     -- 0^e = 0
  | a == 0     = Zech table a     -- 1^e = 1
  | otherwise  = let n = fromIntegral (_qMinus1 table) :: Integer
                     c = fromIntegral a * e            :: Integer
                 in  Zech table (fromInteger (mod c n))
  
zechInv :: Zech p m -> Zech p m 
zechInv (Zech table a)
  | a == -1    = Zech table a     -- 1 / 0 = undefined = 0 
  | a ==  0    = Zech table a     -- 1 / 1 = 1 
  | otherwise  = let n = _qMinus1 table in  Zech table (n - a)    -- we assume here that a > 0 !

zechDiv :: Zech p m -> Zech p m -> Zech p m 
zechDiv (Zech table a) (Zech _ b) 
  | a == -1    = Zech table a     -- 0 / x
  | b == -1    = Zech table b     -- x / 0
  | otherwise  = let n = _qMinus1 table
                     c = a - b
                 in  if c >= 0 then Zech table c else Zech table (c + n)

--------------------------------------------------------------------------------
