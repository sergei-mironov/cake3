module Main where

import Control.Monad

import System.Environment
import System.Process
import System.Exit

import System.FilePath.Wrapper
import Development.Cake3.Utils.Find


main :: IO ()
main = do
  fs <- getDirectoryContentsRecursive (FileT () ".")

  forM_ fs $ \(FileT () x) -> do
    putStrLn x

  return ()
