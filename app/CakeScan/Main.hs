module Main where

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

toFilePath (FileT () x) = x

main :: IO ()
main = do
  fs <- filter filterCakes <$> map toFilePath <$> getDirectoryContentsRecursive (FileT () ".")

  forM_ fs $ \x -> do
    putStrLn x

  return ()
