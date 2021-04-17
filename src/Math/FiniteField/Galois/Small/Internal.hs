
-- | Low level stuff

{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

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
import Control.Monad.ST.Unsafe (unsafeIOToST)      -- debugging only

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

inv :: P -> C -> F -> F
inv !p !ptr !xs = invEuclid p ptr xs

div :: P -> C -> F -> F -> F
div !p !ptr !xs !ys = mul ptr xs (inv p ptr ys)

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
    mvec <- Vec.unsafeThaw (mulPoly p xs ys)
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

--------------------------------------------------------------------------------  e
-- * polynomials

-- -- | Multiplication of polynomials. Note: we assume both have length @m@
-- mulVecPolyFix  :: P -> M -> Vector Word32 -> Vector Word32 -> Vector Word32
-- mulVecPolyFIx !p !m !xs !ys = 
--   Vec.create $ do
--     mvec <- MVec.replicate mbig 0
--     forM_ [0..m-1] $ \ !i -> do
--       let !x = w32toW64 (Vec.unsafeIndex xs i)
--       forM_ [0..m-1] $ \ !j -> do
--         let !y = w32toW64 (Vec.unsafeIndex ys j)
--         MVec.unsafeModify mvec (\ !z -> w64toW32 (Raw.add p (w32toW64 z) (Raw.mul p x y))) (i+j)
--     return mvec
--   where
--     !mbig = m+m-1

-- | The vectors can have different lengths
addPoly :: P -> Vector Word32 -> Vector Word32 -> Vector Word32
addPoly !p !xs !ys 
  | Vec.length xs == Vec.length ys = Vec.zipWith add1 xs ys 
  | otherwise = Vec.fromList (longZipWith 0 0 add1 (Vec.toList xs) (Vec.toList ys))
  where
    add1 !x !y = w64toW32 (Raw.add p (w32toW64 x) (w32toW64 y))

-- | The vectors can have different lengths
subPoly :: P -> Vector Word32 -> Vector Word32 -> Vector Word32
subPoly !p !xs !ys 
  | Vec.length xs == Vec.length ys = Vec.zipWith sub1 xs ys 
  | otherwise = Vec.fromList (longZipWith 0 0 sub1 (Vec.toList xs) (Vec.toList ys))
  where
    sub1 !x !y = w64toW32 (Raw.sub p (w32toW64 x) (w32toW64 y))

scalePoly :: P -> Word32 -> Vector Word32 -> Vector Word32
scalePoly !p !s !ys = Vec.map mul1 ys where
  mul1 !y = w64toW32 (Raw.mul p s64 (w32toW64 y))
  s64 = (w32toW64 s)

mulPoly  :: P -> Vector Word32 -> Vector Word32 -> Vector Word32
mulPoly !p !xs !ys =
  Vec.create $ do
    mvec <- MVec.replicate mbig 0
    forM_ [0..m1-1] $ \ !i -> do
      let !x = w32toW64 (Vec.unsafeIndex xs i)
      forM_ [0..m2-1] $ \ !j -> do
        let !y = w32toW64 (Vec.unsafeIndex ys j)
        MVec.unsafeModify mvec (\ !z -> w64toW32 (Raw.add p (w32toW64 z) (Raw.mul p x y))) (i+j)
    return mvec
  where
    !m1 = Vec.length xs
    !m2 = Vec.length ys
    !mbig = m1+m2-1

zeroPoly :: Int -> Vector Word32 
zeroPoly n = Vec.replicate n 0

onePoly :: Int -> Vector Word32
onePoly n = Vec.fromListN n (1 : replicate (n-1) 0)

isZeroPoly :: Vector Word32 -> Bool
isZeroPoly v = all (==0) (Vec.toList v)

polyDegree :: Vector Word32 -> Int
polyDegree v = go (n-1) where
  !n = Vec.length v
  go !i = if i < 0 
    then (-1) 
    else if Vec.unsafeIndex v i /= 0 
      then i
      else go (i-1)

polyLongDivision :: P -> Vector Word32 -> Vector Word32 -> (Vector Word32, Vector Word32)
polyLongDivision !p !as !bs = runST action where
  action :: forall s. ST s (Vector Word32, Vector Word32) 
  action = do
    let !m = Vec.length as
    let !d = polyDegree bs
        !lcf0 = Vec.unsafeIndex bs d   -- leading coefficient
        !lcf  = w32toW64 lcf0
    rem  <- Vec.thaw as
    quot <- MVec.replicate @(ST s) @MVector m (0 :: Word32) 
   
    forM_ (downIndices (m-d)) $ \ !k -> do
      c0 <- MVec.unsafeRead rem (d+k-1)
      -- print (k,c0)
      when (c0 /= 0) $ do
        let !s = Raw.div p (w32toW64 c0) lcf
        -- Vec.freeze rem >>= \r -> unsafeIOToST $ print (k,c0,s,r)
        MVec.unsafeWrite quot (k-1) (w64toW32 s)
        forM_ [0..d] $ \ !j -> do     -- NOTE: this could be [0..d-1], a bit faster, but while debugging i leave it as is
          let !y = w32toW64 (Vec.unsafeIndex bs j)
          MVec.unsafeModify rem (\ !z -> w64toW32 (Raw.sub p (w32toW64 z) (Raw.mul p s y))) (j+k-1)
  
    fquot <- forM [0..m-1] $ \ !i -> MVec.unsafeRead quot i
    frem  <- forM [0..d-1] $ \ !i -> MVec.unsafeRead rem  i
    return (Vec.fromListN m fquot, Vec.fromListN d frem)

downIndices :: Int -> [Int]
downIndices !k = if k <= 0 then [] else k : downIndices (k-1)

polyQuotient :: P -> Vector Word32 -> Vector Word32 -> Vector Word32
polyQuotient p xs ys = fst (polyLongDivision p xs ys)

--------------------------------------------------------------------------------  

getCPoly :: Ptr Word32 -> Vector Word32
getCPoly ptr = Unsafe.unsafePerformIO (getCPolyIO ptr)

getCPolyIO :: Ptr Word32 -> IO (Vector Word32)
getCPolyIO ptr = do
  !m0 <- peek (plusPtr ptr 4) :: IO Word32
  let !m  = fromIntegral m0   :: Int
  Vec.fromListN (m+1) <$> peekArray (m+1) (plusPtr ptr 8)

-- | Based on <https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers>
invEuclid :: P -> C -> Vector Word32 -> Vector Word32
invEuclid !p !ptr !a = worker (zeroPoly n) (onePoly n) (getCPoly ptr) a where

  n = Vec.length a

  worker :: Vector Word32 -> Vector Word32 -> Vector Word32 -> Vector Word32 -> Vector Word32
  worker !t !newt !r !newr = case isZeroPoly newr of

    False -> let quot = polyQuotient p r newr 
                 r'   = subPoly p r (mulPoly p quot newr)
                 t'   = subPoly p t (mulPoly p quot newt)
             in  worker newt t' newr r'

    True  -> if (polyDegree r > 0)
      then zeroPoly (Vec.length a)
      else let r0 = w32toW64 (Vec.unsafeIndex r 0)
           in  scalePoly p (w64toW32 (Raw.inv p r0)) t

--------------------------------------------------------------------------------
