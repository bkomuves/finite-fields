
-- | Miscelleanous auxilary functions

{-# LANGUAGE BangPatterns #-}
module Math.FiniteField.Misc where

--------------------------------------------------------------------------------

import Data.List
import Data.Word

--------------------------------------------------------------------------------
-- * pairs

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pairs :: [a] -> [(a,a)]
pairs = go where
  go (x:xs@(y:_)) = (x,y) : go xs
  go _            = []

pairsWith :: (a -> a -> b) -> [a] -> [b]
pairsWith f = go where
  go (x:xs@(y:_)) = f x y : go xs
  go _            = []

--------------------------------------------------------------------------------
-- * lists

longZipWith :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
longZipWith !x0 !y0 !f = go where
  go (x:xs) (y:ys) = f x  y  : go xs ys
  go []     (y:ys) = f x0 y  : go [] ys
  go (x:xs) []     = f x  y0 : go xs []
  go _      _      = []

{-# SPECIALIZE sum' :: [Int]     -> Int     #-}
{-# SPECIALIZE sum' :: [Integer] -> Integer #-}
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave [x]    []     = x : []
interleave []     []     = []
interleave _      _      = error "interleave: shouldn't happen"

evens, odds :: [a] -> [a] 
evens (x:xs) = x : odds xs
evens [] = []
odds (x:xs) = evens xs
odds [] = []

--------------------------------------------------------------------------------
-- * tuples

-- | \"Tuples\" fitting into a give shape. The order is lexicographic, that is,
--
-- > sort ts == ts where ts = tuples' shape
--
--   Example: 
--
-- > tuples' [2,3] = 
-- >   [[0,0],[0,1],[0,2],[0,3],[1,0],[1,1],[1,2],[1,3],[2,0],[2,1],[2,2],[2,3]]
--
tuples' :: [Int] -> [[Int]]
tuples' [] = [[]]
tuples' (s:ss) = [ x:xs | x <- [0..s] , xs <- tuples' ss ] 

word32Tuples' :: [Word32] -> [[Word32]]
word32Tuples' [] = [[]]
word32Tuples' (s:ss) = [ x:xs | x <- [0..s] , xs <- word32Tuples' ss ] 

-- | positive \"tuples\" fitting into a give shape.
tuples1' :: [Int] -> [[Int]]
tuples1' [] = [[]]
tuples1' (s:ss) = [ x:xs | x <- [1..s] , xs <- tuples1' ss ]

tuples 
  :: Int    -- ^ length (width)
  -> Int    -- ^ maximum (height)
  -> [[Int]]
tuples len k = tuples' (replicate len k)

tuples1 
  :: Int    -- ^ length (width)
  -> Int    -- ^ maximum (height)
  -> [[Int]]
tuples1 len k = tuples1' (replicate len k)

--------------------------------------------------------------------------------
-- * products

-- | Product of list of integers, but in interleaved order (for a list of big numbers,
-- it should be faster than the linear order)
productInterleaved :: [Integer] -> Integer
productInterleaved = go where
  go []    = 1
  go [x]   = x
  go [x,y] = x*y
  go list  = go (evens list) * go (odds list)

--------------------------------------------------------------------------------
-- * words

w32toW64 :: Word32 -> Word64
w32toW64 = fromIntegral

w64toW32 :: Word64 -> Word32
w64toW32 = fromIntegral

--------------------------------------------------------------------------------
