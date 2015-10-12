module Main where

import Control.Arrow
import Control.Monad
import Data.List

import System.Environment
import System.Process
import System.Exit

import System.FilePath
import System.FilePath.Wrapper (FileT(..))
import Development.Cake3.Utils.Find

filterCakes :: FilePath -> Bool
filterCakes x = nodots && isPrefixOf "Cake" bn &&  isSuffixOf ".hs" bn where
  bn = takeFileName x
  nodots = all (not . isPrefixOf ".") $ skipRelativeDot $ splitDirectories x
  skipRelativeDot (".":ds) = ds
  skipRelativeDot x = x

pathOrder :: FilePath -> FilePath -> Ordering
pathOrder a b =
  let
    na = normalise a
    nb = normalise b
    la = length (splitDirectories na)
    lb = length (splitDirectories nb)
    x = compare la lb
  in
    case x of
      EQ -> compare na nb
      _ -> x


nameOrder :: FilePath -> FilePath -> Ordering
nameOrder a b = takeFileName a `compare` takeFileName b

main :: IO ()
main = do
  fs <- map (sortBy pathOrder) <$> groupBy (\a b -> EQ == nameOrder a b) <$> sortBy nameOrder <$> filter filterCakes <$> getDirectoryContentsRecursive' "."

  forM_ fs $ \x -> do
    putStrLn (show x)

  return ()
