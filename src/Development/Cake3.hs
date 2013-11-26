{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Development.Cake3 (

    Variable
  , Recipe
  , RefInput(..)
  , RefOutput(..)
  , RefMerge(..)
  -- , Placable(..)
  , Reference
  , ReferenceLike(..)

  -- Monads
  , A
  , Make
  , buildMake
  , runMake
  , runMake_
  , includeMakefile
  , MonadMake(..)

  -- Rules
  , rule
  , rule'
  , phony
  , depend
  , before
  , produce
  , unsafe
  , merge
  , selfUpdateRule
  , prebuild
  , postbuild
  
  -- Files
  , FileLike(..)
  , File
  , file'
  , (.=)
  , (</>)
  , toFilePath
  , readFileForMake

  -- Make parts
  , prerequisites
  , shell
  , cmd
  , makevar
  , extvar
  , makefile
  , CommandGen'(..)
  , make
  , ProjectLocation(..)
  , currentDirLocation

  -- More
  , module Control.Monad
  , module Control.Applicative
  ) where

-- import Prelude (id, Char(..), Bool(..), Maybe(..), Either(..), flip, ($), (+), (.), (/=), undefined, error,not)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Loc
import qualified Data.List as L
import Data.List (concat,map, (++), reverse,elem,intercalate,delete)
import Data.Foldable (Foldable(..), foldr)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set,member,insert)
import Data.String as S
import Data.Tuple
import System.IO
import System.Directory
import qualified System.FilePath as F
import Text.Printf

import Development.Cake3.Types
import Development.Cake3.Writer
import Development.Cake3.Monad
import System.FilePath.Wrapper as W

data ProjectLocation = ProjectLocation {
    root :: FilePath
  , off :: FilePath
  } deriving (Show, Eq, Ord)

currentDirLocation :: (MonadIO m) => m ProjectLocation
currentDirLocation = do
  cwd <- liftIO $ getCurrentDirectory
  return $ ProjectLocation cwd cwd

file' :: ProjectLocation -> String -> File
file' pl f' = fromFilePath ((".") </> makeRelative (root pl) ((off pl) </> f)) where
  f = F.dropTrailingPathSeparator f'

selfUpdateRule :: Make Recipe
selfUpdateRule = do
  rule $ do
    shell (CommandGen' (
      concat <$> sequence [
        refInput $ (fromFilePath ".") </> "Cakegen"
      , refMerge $ string " > "
      , refOutput makefile]))

runMake' :: Make a -> (Either String String -> IO b) -> IO b
runMake' mk output = do
  -- ms <- evalMake (selfUpdateRule >> mk)
  ms <- evalMake mk
  when (not $ L.null (warnings ms)) $ do
    hPutStr stderr (warnings ms)
  when (not $ L.null (errors ms)) $ do
    fail (errors ms)
  output (buildMake ms)

runMake_ :: Make a -> IO ()
runMake_ mk = runMake' mk output where
  output (Left err) = hPutStrLn stderr err
  output (Right str) = hPutStrLn stdout str

runMake :: Make a -> IO String
runMake mk = runMake' mk output where
  output (Left err) = fail err
  output (Right str) = return str

withPlacement :: Make (Recipe,a) -> Make (Recipe,a)
withPlacement mk = do
  (r,a) <- mk
  --p <- getPlacementPos
  addPlacement 0 (S.findMin (rtgt r))
  return (r,a)

rule' :: A a -> Make (Recipe,a)
rule' act = do
  loc <- getLoc
  (r,a) <- runA loc act
  addRecipe r
  return (r,a)

phony :: String -> A ()
phony name = do
  produce (W.fromFilePath name :: File)
  markPhony

rule :: A a -> Make Recipe
rule act = fst <$> withPlacement (rule' act)

-- FIXME: depend can be used under unsafe but it doesn't work
unsafe :: A () -> A ()
unsafe action = do
  r <- get
  action
  modify $ \r' -> r' { rsrc = rsrc r, rvars = rvars r }

before :: Make Recipe -> A ()
before mx =  liftMake mx >>= refInput >> return ()


