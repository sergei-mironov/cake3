module Main where

import Control.Arrow
import Control.Monad
import Control.Exception
import Data.List

import System.Environment
import System.Process
import System.Exit
import System.IO
import System.Directory
import System.Process

import System.FilePath
import System.FilePath.Wrapper (FileT(..))
import Development.Cake3.Utils.Find

import Text.Printf

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

checkCakeGroupAgainstGit :: [FilePath] -> IO (Either String (String,FilePath))
checkCakeGroupAgainstGit cakes = do
  cwd <- getCurrentDirectory
  revs <- forM cakes $ \c -> do
    bracket (setCurrentDirectory (takeDirectory c)) (const (setCurrentDirectory cwd)) $ \ () -> do
      return ("ACAACACACACACACACACAACACACA", c)
  return $
    foldl' (\err (r',c') ->
      case err of
        Left x -> Left x
        Right (r,c) ->
          case r == r' of
            True -> Right (r,c)
            False -> Left (printf "Git revision for %s (%s) doesn't match the one for %s (%s)" c' r' c r)
    ) (Left "empty list") revs

main :: IO ()
main = do
  fs <- map (sortBy pathOrder) <$> groupBy (curry ((EQ==) . uncurry nameOrder)) <$> sortBy nameOrder <$> filter filterCakes <$> getDirectoryContentsRecursive' "."

  forM_ fs $ \x -> do
    putStrLn (show x)

  return ()

