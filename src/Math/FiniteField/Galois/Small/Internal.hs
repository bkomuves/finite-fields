
-- | Low level stuff

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, GADTs #-}
module Math.FiniteField.Galois.Small.Internal where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word
import Data.List

import GHC.TypeNats (Nat)

import Control.Monad
import Control.Monad.ST

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Unboxed         as Vec
import qualified Data.Vector.Generic.Mutable as MVec

import System.IO.Unsafe as Unsafe

import Math.FiniteField.Primes
import Math.FiniteField.TypeLevel
import Math.FiniteField.Conway
import Math.FiniteField.Conway.Internal
import Math.FiniteField.Misc
 
import qualified Math.FiniteField.PrimeField.Small.Raw as Raw

--------------------------------------------------------------------------------  

type P = Word64
type M = Int
type C = Ptr Word32
type F = Vector Word32

--------------------------------------------------------------------------------  

neg :: P -> F -> F
neg !p !xs = Vec.map neg1 xs where
  neg1 !x = w64toW32 (Raw.neg p (w32toW64 x))

add :: P -> F -> F -> F
add !p !xs !ys = Vec.zipWith add1 xs ys where 
  add1 !x !y = w64toW32 (Raw.add p (w32toW64 x) (w32toW64 y))

sub :: P -> F -> F -> F
sub !p !xs !ys = Vec.zipWith sub1 xs ys where
  sub1 !x !y = w64toW32 (Raw.sub p (w32toW64 x) (w32toW64 y))

mul :: C -> F -> F -> F
mul !ptr !xs !ys = Unsafe.unsafePerformIO (mulIO ptr xs ys)

--------------------------------------------------------------------------------  
-- * multiplication

mulIO :: C -> F -> F -> IO F
mulIO !ptr !xs !ys = 
  do
    !p0 <- peek ptr             :: IO Word32
    !m0 <- peek (plusPtr ptr 4) :: IO Word32
    let !p  = fromIntegral p0   :: Word64
        !m  = fromIntegral m0   :: Int
        !m1 = m - 1             :: Int
    cvec <- Vec.fromListN (m+1) <$> peekArray (m+1) (plusPtr ptr 8)
    mvec <- Vec.unsafeThaw (mulVecPoly p m xs ys)
    forM_ (downIndices m1) $ \ !k -> do
      c0 <- MVec.unsafeRead mvec (m+k-1)
      -- print (k,c0)
      let !c = fromIntegral c0 :: Word64
      when (c /= 0) $ do
        forM_ [0..m] $ \ !j -> do     -- NOTE: this could be [0..m1], a bit faster, but while debugging i leave it as is
          let !y = w32toW64 (Vec.unsafeIndex cvec j)
          MVec.unsafeModify mvec (\ !z -> w64toW32 (Raw.sub p (w32toW64 z) (Raw.mul p c y))) (j+k-1)
    -- print =<< Vec.freeze mvec
    xs <- forM [0..m-1] $ \ !i -> MVec.unsafeRead mvec i
    return $ Vec.fromListN m xs

  where
    downIndices :: Int -> [Int]
    downIndices !k = if k <= 0 then [] else k : downIndices (k-1)

-- | Multiplication of polynomials
mulVecPoly  :: P -> M -> Vector Word32 -> Vector Word32 -> Vector Word32
mulVecPoly !p !m !xs !ys = 
  Vec.create $ do
    mvec <- MVec.replicate mbig 0
    forM_ [0..m-1] $ \ !i -> do
      let !x = w32toW64 (Vec.unsafeIndex xs i)
      forM_ [0..m-1] $ \ !j -> do
        let !y = w32toW64 (Vec.unsafeIndex ys j)
        MVec.unsafeModify mvec (\ !z -> w64toW32 (Raw.add p (w32toW64 z) (Raw.mul p x y))) (i+j)
    return mvec
  where
    !mbig = m+m-1

--------------------------------------------------------------------------------  
