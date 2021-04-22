
-- | Example application: brute force counting of points on the elliptic
-- curve @y^2 = x^3 + 2@ over the finite field @F_q@ where @q=p^m@.
--
-- For example, over GF(7), the zeta function of (the projective) curve is 
--
-- >    1 + T + 7*T^2 
-- > -------------------
-- >   (1-T) * (1-7*T) 
--
-- which, after taking the logarithmic derivative and then a Taylor series 
-- expansion, predicts the (projective) counts for GF(7^m) to be:
--
-- > 9, 63, 324, 2331, 17019 ...
--
-- This program counts the affine points, so the result it outputs is one less. 
--

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Main where

--------------------------------------------------------------------------------

import Text.Read (readMaybe)

import System.Environment

import Math.FiniteField.Class
import Math.FiniteField.Conway
-- import Math.FiniteField.GaloisField.Small
import Math.FiniteField.GaloisField.Zech
import Math.FiniteField.TypeLevel

--------------------------------------------------------------------------------

-- | The affine elliptic curve @y^2 = x^3 + a*x + b@
curveEquation :: Num a => (a,a) -> a -> a -> a
curveEquation (!a,!b) !x !y = - y*y + x*x*x + a*x + b

countCurvePointsIn :: (Eq a, Num a) => a -> (a,a) -> [(a,a)] -> Int
countCurvePointsIn !zero !ab = go 0 where
  go !acc []         = acc
  go !acc ((x,y):ps) = if curveEquation ab x y == zero then go (acc+1) ps else go acc ps 

fieldWorker :: forall f. Field f => Witness f -> IO ()
fieldWorker field = do
  let zero  =  embed field 0                  
      ab    = (embed field 0, embed field 2) 
  let elems = enumerate field 
      plane = [ (x,y) | x<-elems, y<-elems ] 
  let cnt   = countCurvePointsIn zero ab plane
  let p     = characteristic field
      m     = dimension      field
  putStrLn $ "the number of points of the affine elliptic curve y^2=x^3+2 is:"
  putStrLn $ "#E( F_{" ++ show p ++ "^" ++ show m ++ "} ) = " ++ show cnt

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [prime,expo] -> case (readMaybe prime, readMaybe expo) of
      (Just p, Just m) -> do
        putStrLn $ "Galois field of order " ++ show p ++ "^" ++ show m
        case mkZechField p m of
          Nothing   -> putStrLn $ "error: cannot construct finite field GF(" ++ show p ++ "^" ++ show m ++ ")"
          Just some -> case some of { SomeWitnessZech witness -> fieldWorker witness }
      _ -> printUsage
    _ -> printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "usage: ./curve_count <prime> <exponent>"

--------------------------------------------------------------------------------
