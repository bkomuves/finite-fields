
-- | Table of Conway polynomials
--
-- The data is from <http://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html>
--
-- The text file from the above link is converted to a C source file during the 
-- build process, and the resulting object file is then linked.
-- This should be much faster (in compilation time) than encoding it as a Haskell 
-- data structure, because GHC does not like big constants. Also the in-memory representation
-- can be controlled more precisely.
--

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Math.FiniteField.Conway.Internal where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Array

import qualified System.IO.Unsafe as Unsafe

--------------------------------------------------------------------------------
-- * The C table

foreign import ccall "get_conway_table_size" c_conway_table_size :: Word32
foreign import ccall "get_conway_table_ptr"  c_conway_table_ptr  :: Ptr Word32

getConwayEntryParams :: Ptr Word32 -> IO (Word64,Int)
getConwayEntryParams !ptr = do
  p <- peek ptr             :: IO Word32
  m <- peek (plusPtr ptr 4) :: IO Word32
  return (fromIntegral p, fromIntegral m)

marshalConwayEntry :: Ptr Word32 -> IO (Word64,Int,[Word64])
marshalConwayEntry !ptr = do
  p <- peek ptr             :: IO Word32
  m <- peek (plusPtr ptr 4) :: IO Word32
  coeffs <- peekArray (fromIntegral m + 1) (plusPtr ptr 8) :: IO [Word32]
  return (fromIntegral p , fromIntegral m , map fromIntegral coeffs)

--------------------------------------------------------------------------------
-- * The global table

newtype ConwayTable 
  = ConwayTable { fromConwayTable :: IntMap (Ptr Word32) }

encodePrimeExpo :: Int -> Int -> Int
encodePrimeExpo !prime !expo  = prime .|. (shiftL expo 20)

decodePrimeExpo :: Int -> (Int,Int)
decodePrimeExpo !code = (code .&. 0xfffff , shiftR code 20)

{-# NOINLINE theConwayTable #-}
theConwayTable :: ConwayTable
theConwayTable = Unsafe.unsafePerformIO readConwayTableIO

{-# NOINLINE lookupConwayEntry #-}
lookupConwayEntry :: Int -> Int -> Maybe (Word64,Int,[Word64])
lookupConwayEntry p m = case IntMap.lookup (encodePrimeExpo p m) (fromConwayTable theConwayTable) of
  Nothing  -> Nothing
  Just ptr -> Just (Unsafe.unsafePerformIO (marshalConwayEntry ptr))

--------------------------------------------------------------------------------
-- * Parse the C table into a lookup table

readConwayTableIO :: IO ConwayTable
readConwayTableIO = 
  do
    list <- go c_conway_table_size c_conway_table_ptr 
    let f (p,m,ptr) = (encodePrimeExpo (fromIntegral p) (fromIntegral m) , ptr)
    return $ ConwayTable $ IntMap.fromList (map f list)
  where
    go :: Word32 -> Ptr Word32 -> IO [(Word32,Word32,Ptr Word32)]
    go 0  _    = return []
    go !k !ptr = do
      p <- peek ptr             :: IO Word32
      m <- peek (plusPtr ptr 4) :: IO Word32
      let ptr' = plusPtr ptr (8 + 4*(fromIntegral m + 1))
      let this = (p,m,ptr)
      rest <- go (k-1) ptr'
      return (this:rest)

--------------------------------------------------------------------------------