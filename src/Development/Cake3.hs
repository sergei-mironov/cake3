{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Development.Cake3 (

    Variable
  , Recipe
  , RefInput(..)
  , RefOutput(..)
  , CakeString
  , string

  -- Monads
  , A
  , Make
  , buildMake
  , runMake
  , writeMake
  , includeMakefile
  , MonadMake(..)

  -- Rules
  , rule
  , rule2
  , rule'
  , phony
  , depend
  , produce
  , ignoreDepends
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
  , genFile
  , genTmpFile

  -- Make parts
  , prerequisites
  , shell
  , unsafeShell
  , cmd
  , makevar
  , extvar
  , CommandGen'(..)
  , make
  , ProjectLocation(..)
  , currentDirLocation

  -- Import several essential modules
  , module Data.String
  , module Control.Monad
  , module Control.Applicative
  ) where

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
import Data.String
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
file' pl f' = fromFilePath (addpoint (F.normalise rel)) where
  rel = makeRelative (root pl) ((off pl) </> f)
  f = F.dropTrailingPathSeparator f'
  addpoint "." = "."
  addpoint p = "."</>p

runMake'
  :: File -- ^ Output file
  -> Make a  -- ^ Make builder
  -> (String -> IO b) -- ^ Handler to output the file
  -> IO b
runMake' makefile mk output = do
  ms <- evalMake makefile mk
  when (not $ L.null (warnings ms)) $ do
    hPutStr stderr (warnings ms)
  when (not $ L.null (errors ms)) $ do
    fail (errors ms)
  case buildMake ms of
    Left e -> fail e
    Right s -> output s

-- | Execute the Make monad, build the Makefile, write it to the output file. Also
-- note, that errors (if any) go to the stderr. fail will be executed in such
-- cases
writeMake
  :: File -- ^ Output file
  -> Make a -- ^ Makefile builder
  -> IO ()
writeMake f mk = runMake' f mk (writeFile (toFilePath f))

-- | A General Make runner. Executes the monad, returns the Makefile as a
-- String. Errors go to stdout. fail is possible.
runMake :: Make a -> IO String
runMake mk = runMake' defaultMakefile mk return

-- | Raise the recipe's priority (it will appear higher in the final Makefile)
withPlacement :: (MonadMake m) => m (Recipe,a) -> m (Recipe,a)
withPlacement mk = do
  (r,a) <- mk
  liftMake $ do
    addPlacement 0 (S.findMin (rtgt r))
    return (r,a)

-- | Build a Recipe using the builder provided and record it to the MakeState.
-- Return the copy of Recipe (which should not be changed in future) and the
-- result of recipe builder. The typical recipe builder result is the list of
-- it's targets.
--
-- /Example/
-- Lets declare a rule which builds "main.o" out of "main.c" and "CFLAGS"
-- variable
--
-- > let c = file "main.c"
-- > rule $ shell [cmd| gcc -c $(extvar "CFLAGS") -o @(c.="o") $c |]
--
rule2 :: (MonadMake m)
  => A a -- ^ Recipe builder
  -> m (Recipe,a) -- ^ The recipe itself and the recipe builder's result
rule2 act = liftMake $ do
  loc <- getLoc
  (r,a) <- runA loc act
  addRecipe r
  return (r,a)

-- | A version of rule2. Rule places it's recipe above all recipies defined so
-- far.
rule
  :: A a    -- ^ Recipe builder
  -> Make a
rule act = snd <$> withPlacement (rule2 act)

-- | A version of rule, without monad set explicitly
rule' :: (MonadMake m) => A a -> m a
rule' act = liftMake $ snd <$> withPlacement (rule2 act)


genFile :: (MonadMake m) => File -> String -> m File
genFile tgt cnt = rule' $do
  shell [cmd|-rm -rf @tgt |]
  forM_ (lines cnt) $ \l -> do
    shell [cmd|echo '$(string (quote_dollar l))' >> @tgt |]
  return tgt
  where
    quote_dollar [] = []
    quote_dollar ('$':cs) = '$':'$':(quote_dollar cs)
    quote_dollar (c:cs) = c : (quote_dollar cs)

genTmpFile :: (MonadMake m) => String -> m File
genTmpFile cnt = tmpFile >>= \f -> genFile f cnt


