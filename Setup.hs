
-- | Parse the conway polynomial table, and create a C source code out of it
--
-- It's C instead of Haskell because GHC does not like big tables. 
-- I can also control the in-memory representation better this way.
--

module Main where

--------------------------------------------------------------------------------

import Data.List

import Distribution.Simple
import Distribution.PackageDescription

import System.Process
import System.Directory
import System.FilePath

main = defaultMainWithHooks myUserHooks

--------------------------------------------------------------------------------

conway_input :: FilePath
conway_input = "./conway/ConwayPolynomials.txt"

cbits_dir :: FilePath
cbits_dir = "./cbits/"

conway_output :: FilePath
conway_output = "./cbits/conway_table.c"

myPreBuildHook args buildflags = do
  createDirectoryIfMissing False cbits_dir
  makeCTable conway_input conway_output
  return $ emptyHookedBuildInfo

myPostCleanHook args cleanflags pdep mlocalbuildinfo = do
  removeFile conway_output

myUserHooks = simpleUserHooks
  { preBuild  = myPreBuildHook
  , postClean = myPostCleanHook
  }

--------------------------------------------------------------------------------

parseLine :: String -> (Int,Int,[Int])
parseLine ln0 = read ln1 where
  ln1 ="(" ++ (reverse $ drop 2 $ reverse $ tail ln0) ++ ")" 
  
entryToC :: Bool -> (Int,Int,[Int]) -> String
entryToC isfirst (p,m,cfs) = prefix ++ content where
  prefix = if isfirst then "  { " else "  , "
  content = if length cfs == m+1 then intercalate "," (map show (p:m:cfs)) else error "entryToC: invalid entry in the Conway polynomial database"

isFirst :: [Bool]
isFirst = True : repeat False

makeCTable :: FilePath -> FilePath -> IO ()
makeCTable in_file out_file = do
  ls <- (init . tail . lines) <$> readFile in_file
  let entries = map parseLine ls
  let n = length entries
  let text = unlines (zipWith entryToC isFirst entries)
  writeFile out_file (prefix n ++ text ++ postfix)

prefix :: Int -> String
prefix no_entries = unlines
  [ "// conway polynomials"
  , "// generated from: <http://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html>"
  , ""
  , "#include <stdint.h>"
  , ""
  , "uint32_t conway_table[];"
  , ""
  , "uint32_t  get_conway_table_size() { return " ++ show no_entries ++ " ; }"
  , "uint32_t *get_conway_table_ptr()  { return conway_table ; }"
  , ""
  , "uint32_t conway_table[] = "
  ]

postfix :: String
postfix = "  };\n"

