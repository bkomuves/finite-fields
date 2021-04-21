
-- | C implementation of GF(p^m) via precomputed tables of Zech's logarithm.
--
-- This way I can test the C implementation using the Haskell test framework.
--

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving, ExistentialQuantification #-}

module Math.FiniteField.GaloisField.Zech.C where

--------------------------------------------------------------------------------

import Data.Int

import GHC.TypeNats (Nat)

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal

import System.Random ( RandomGen , randomR )

import System.IO
import System.IO.Unsafe as Unsafe

import qualified Data.Vector.Unboxed as Vec

import Math.FiniteField.Class
import Math.FiniteField.TypeLevel.Singleton

import qualified Math.FiniteField.GaloisField.Zech as Z

--------------------------------------------------------------------------------

data WitnessC (p :: Nat) (m :: Nat) 
  = WitnessC (ForeignPtr Int32)
  deriving Show

fromWitnessC :: WitnessC p m -> ForeignPtr Int32
fromWitnessC (WitnessC fptr) = fptr

data SomeWitnessC 
  = forall p m. SomeWitnessC (WitnessC p m)

deriving instance Show SomeWitnessC

mkCField :: Int -> Int -> Maybe SomeWitnessC
mkCField p m = case Z.mkZechField p m of 
  Nothing   -> Nothing
  Just some -> case some of
    Z.SomeWitnessZech wzech -> Just (SomeWitnessC (makeCZechTable wzech))

unsafeCField :: Int -> Int -> SomeWitnessC
unsafeCField p m = case mkCField p m of 
  Nothing   -> error $ "unsafeCField: cannot find Conway polynomial for GF(" ++ show p ++ "^" ++ show m ++ ")"
  Just some -> some

instance FieldWitness (WitnessC p m) where
  type FieldElem    (WitnessC p m) = CFq p m
  type WitnessPrime (WitnessC p m) = p
  type WitnessDim   (WitnessC p m) = m

--------------------------------------------------------------------------------

makeCZechTable :: Z.WitnessZech p m -> WitnessC p m
makeCZechTable (Z.WitnessZech zechtable) = unsafePerformIO (WitnessC <$> marshalZechTable zechtable)

marshalZechTable :: Z.ZechTable -> IO (ForeignPtr Int32)
marshalZechTable ztable = do

  let (p,m) = Z._zechParams ztable
  let q = p ^ m 
  let e = if p == 2 then 0 else div (q-1) 2
  let len = 4 * fromIntegral (4 + p + q-1)
  fptr <- mallocForeignPtrBytes len :: IO (ForeignPtr Int32)

  withForeignPtr fptr $ \ptr -> do
    pokeElemOff ptr 0 p
    pokeElemOff ptr 1 m
    pokeElemOff ptr 2 (q-1)
    pokeElemOff ptr 3 e
    let ofs = 4 * (4 + fromIntegral p)
    pokeArray (plusPtr ptr 16 ) (Vec.toList (Z._embedding ztable))
    pokeArray (plusPtr ptr ofs) (Vec.toList (Z._zechLogs  ztable))

  return fptr

-- | Save the data necessary to do computations to a file
saveCZechTable :: FilePath -> WitnessC p q -> IO ()
saveCZechTable fname w@(WitnessC fptr) = do
  let p = rawPrime w
  let m = rawDim   w
  let q = p^m
  let len = 4 * fromIntegral (4 + p + q-1)
  withForeignPtr fptr $ \ptr -> do
    h <- openBinaryFile fname WriteMode 
    hPutBuf h ptr len
    hClose h

-- | Load the data necessary to do computations from a file
loadCZechTable :: FilePath -> IO (Maybe SomeWitnessC)
loadCZechTable fname = do
  h  <- openBinaryFile fname ReadMode 
  mb <- allocaBytes 16 $ \(header :: Ptr Int32) -> do
    hGetBuf h header 16
    p   <- peekElemOff header 0 
    m   <- peekElemOff header 1
    qm1 <- peekElemOff header 2
    e   <- peekElemOff header 3
    let ok1 = qm1 + 1 == p^m
        ok2 = if p == 2 then e == 0 else e == div qm1 2 
    if not (ok1 && ok2) 
      then return Nothing
      else do
        hSeek h AbsoluteSeek 0
        let len = 16 + 4 * (fromIntegral qm1 + fromIntegral p) 
        fptr <- mallocForeignPtrBytes len
        withForeignPtr fptr $ \ptr -> hGetBuf h ptr len  
        return $ case (someSNat64 (fromIntegral p), someSNat64 (fromIntegral m)) of
          (SomeSNat64 sp, SomeSNat64 sm) -> Just (SomeWitnessC (constructWitnessC sp sm fptr))
  hClose h
  return mb

constructWitnessC :: SNat64 p -> SNat64 m -> ForeignPtr Int32 -> WitnessC p m
constructWitnessC _ _ fptr = WitnessC fptr

--------------------------------------------------------------------------------

-- | An element of the field
data CFq (p :: Nat) (m :: Nat) 
  = CFq {-# UNPACK #-} !(ForeignPtr Int32) {-# UNPACK #-} !Int32 

instance Eq (CFq p m) where
  (==) (CFq _ x) (CFq _ y) = x == y

instance Ord (CFq p m) where
  compare (CFq _ x) (CFq _ y) = compare x y

instance Show (CFq p m) where
  show (CFq _ k)
    | k == -1    = "0"
    | k ==  0    = "1"
    | k ==  1    = "g"
    | otherwise  = "g^" ++ show k

instance Num (CFq p m) where
  fromInteger = error "GaloisField/Zech/C/fromInteger: cannot be implemented; use `embed` instead"
  negate (CFq fptr x)           = CFq fptr (fromRaw (rawNeg (WitnessC fptr) (Raw x)        ))
  (+)    (CFq fptr x) (CFq _ y) = CFq fptr (fromRaw (rawAdd (WitnessC fptr) (Raw x) (Raw y)))
  (-)    (CFq fptr x) (CFq _ y) = CFq fptr (fromRaw (rawSub (WitnessC fptr) (Raw x) (Raw y)))
  (*)    (CFq fptr x) (CFq _ y) = CFq fptr (fromRaw (rawMul (WitnessC fptr) (Raw x) (Raw y)))
  abs    (CFq fptr x)           = CFq fptr x
  signum (CFq fptr x)           = CFq fptr 0

instance Fractional (CFq p m) where
  fromRational = error "GaloisField/Zech/C/fromRational: cannot be implemented; use `embed` instead"
  recip  (CFq fptr x)           = CFq fptr (fromRaw (rawInv (WitnessC fptr) (Raw x)        ))
  (/)    (CFq fptr x) (CFq _ y) = CFq fptr (fromRaw (rawDiv (WitnessC fptr) (Raw x) (Raw y)))

instance Field (CFq p m) where
  type Witness (CFq p m) = WitnessC p m
  type Prime   (CFq p m) = p
  type Dim     (CFq p m) = m

  characteristic    w = fromIntegral (rawPrime     w)
  dimension         w = fromIntegral (rawDim       w)
  fieldSize         w = fromIntegral (rawFieldSize w)
  witnessOf        !x = case x of { CFq fptr _ -> WitnessC fptr }

  enumerate  (WitnessC fptr)    = map (\r -> CFq fptr (fromRaw r)) (rawEnumerate (WitnessC fptr))
  embed      (WitnessC fptr) !k = CFq fptr (fromRaw (rawEmbed (WitnessC fptr) (fromInteger k)))
  embedSmall (WitnessC fptr) !k = CFq fptr (fromRaw (rawEmbed (WitnessC fptr)              k ))

  randomFieldElem   w = randomCFq    w
  randomInvertible  w = randomInvCFq w

  power      (CFq fptr x) e = CFq fptr (fromRaw (rawPow (WitnessC fptr) (Raw x) (fromIntegral e)))
  powerSmall (CFq fptr x) e = CFq fptr (fromRaw (rawPow (WitnessC fptr) (Raw x)               e ))

  zero    (WitnessC fptr) = CFq fptr (-1)
  one     (WitnessC fptr) = CFq fptr   0
  primGen (WitnessC fptr) = CFq fptr   1
  isZero  (CFq _ a)       = a == -1
  isOne   (CFq _ a)       = a == 0

--------------------------------------------------------------------------------

randomCFq :: RandomGen gen => WitnessC p m -> gen -> (CFq p m, gen)
randomCFq w@(WitnessC fptr) g = 
  let q = rawFieldSize w 
  in  case randomR (-1,q-2) g of (k,g') -> (CFq fptr (fromIntegral k), g')

randomInvCFq :: RandomGen gen => WitnessC p m -> gen -> (CFq p m, gen)
randomInvCFq w@(WitnessC fptr) g = 
  let q = rawFieldSize w 
  in  case randomR (0,q-2) g of (k,g') -> (CFq fptr (fromIntegral k), g')

--------------------------------------------------------------------------------
-- * The \"raw\" interface, where you have to manually supply the tables

newtype Raw (p :: Nat) (m :: Nat) 
  = Raw Int32 
  deriving (Eq,Ord)

fromRaw :: Raw p m -> Int32
fromRaw (Raw k) = k

instance Show (Raw p m) where
  show (Raw k)
    | k == -1    = "0"
    | k ==  0    = "1"
    | k ==  1    = "g"
    | otherwise  = "g^" ++ show k

rawNeg :: WitnessC p m -> Raw p m -> Raw p m
rawNeg (WitnessC fptr) (Raw x) = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_neg ptr x))

rawAdd :: WitnessC p m -> Raw p m -> Raw p m -> Raw p m
rawAdd (WitnessC fptr) (Raw x) (Raw y) = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_add ptr x y))

rawSub :: WitnessC p m -> Raw p m -> Raw p m -> Raw p m
rawSub (WitnessC fptr) (Raw x) (Raw y) = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_sub ptr x y))

rawInv :: WitnessC p m -> Raw p m -> Raw p m
rawInv (WitnessC fptr) (Raw x) = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_inv ptr x))

rawMul :: WitnessC p m -> Raw p m -> Raw p m -> Raw p m
rawMul (WitnessC fptr) (Raw x) (Raw y) = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_mul ptr x y))

rawDiv :: WitnessC p m -> Raw p m -> Raw p m -> Raw p m
rawDiv (WitnessC fptr) (Raw x) (Raw y) = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_div ptr x y))

rawPow :: WitnessC p m -> Raw p m -> Int -> Raw p m
rawPow (WitnessC fptr) (Raw x) e = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_pow ptr x (fromIntegral e)))

rawIsZero :: Raw p m -> Bool
rawIsZero (Raw x) = (cboolToBool $ zech_is_zero x)

rawIsOne :: Raw p m -> Bool
rawIsOne (Raw x) = (cboolToBool $ zech_is_one x)

rawZero :: Raw p m
rawZero = Raw (-1)

rawOne :: Raw p m
rawOne = Raw 0

rawPrim :: Raw p m
rawPrim = Raw 1

rawEmbed :: WitnessC p m -> Int -> Raw p m
rawEmbed (WitnessC fptr) k = unsafePerformIO (withForeignPtr fptr (\ptr -> Raw <$> zech_embed ptr (fromIntegral k)))

rawEnumerate :: WitnessC p m -> [Raw p m]
rawEnumerate (WitnessC fptr) = unsafePerformIO $ do
  withForeignPtr fptr $ \ptr -> do
    qminus1 <- peekElemOff ptr 2     :: IO Int32
    let q = fromIntegral qminus1 + 1 :: Int
    allocaBytes (4*q) $ \tgt -> do
      _ <- zech_enumerate ptr tgt
      map Raw <$> peekArray q tgt

rawPrime :: WitnessC p m -> Int
rawPrime (WitnessC fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> fromIntegral <$> peekElemOff ptr 0 

rawDim :: WitnessC p m -> Int
rawDim (WitnessC fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> fromIntegral <$> peekElemOff ptr 1 

rawFieldSize :: WitnessC p m -> Int
rawFieldSize (WitnessC fptr) = unsafePerformIO $ do
  withForeignPtr fptr $ \ptr -> do
    qminus1 <- peekElemOff ptr 2 :: IO Int32
    return (fromIntegral qminus1 + 1)

--------------------------------------------------------------------------------
-- * foreign imports

cboolToBool :: CBool -> Bool
cboolToBool b = (b /= 0)

foreign import ccall unsafe "zech_neg" zech_neg :: Ptr Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "zech_add" zech_add :: Ptr Int32 -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "zech_sub" zech_sub :: Ptr Int32 -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "zech_inv" zech_inv :: Ptr Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "zech_mul" zech_mul :: Ptr Int32 -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "zech_div" zech_div :: Ptr Int32 -> Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "zech_pow" zech_pow :: Ptr Int32 -> Int32 -> CInt  -> IO Int32

foreign import ccall unsafe "zech_zero" zech_zero :: Int32
foreign import ccall unsafe "zech_one"  zech_one  :: Int32
foreign import ccall unsafe "zech_prim" zech_prim :: Int32

foreign import ccall unsafe "zech_is_zero" zech_is_zero :: Int32 -> CBool
foreign import ccall unsafe "zech_is_one"  zech_is_one  :: Int32 -> CBool

foreign import ccall unsafe "zech_embed"     zech_embed     :: Ptr Int32 -> CInt      -> IO Int32
foreign import ccall unsafe "zech_enumerate" zech_enumerate :: Ptr Int32 -> Ptr Int32 -> IO CInt

--------------------------------------------------------------------------------
