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

-- | Converts string representation of Path into type-safe File. Internally,
-- files are stored as a relative offsets from the project root directory
file' :: ProjectLocation -> String -> File
file' pl f' = fromFilePath (addpoint (F.normalise rel)) where
  rel = makeRelative (root pl) ((off pl) </> f)
  f = F.dropTrailingPathSeparator f'
  addpoint "." = "."
  addpoint p = "."</>p

-- | A Generic Make monad runner. Execute the monad @mk@, provide the @output@
-- handler with Makefile encoded as a string. Note that Makefile may contain
-- rules which references the file itself by the name @makefile@.  In case of
-- errors, print report to stderr and abort the execution with @fail@ call
runMakeH
  :: File -- ^ Output file
  -> Make a  -- ^ Make builder
  -> (String -> IO b) -- ^ Handler to output the file
  -> IO (MakeState,b)
runMakeH makefile mk output = do
  ms <- evalMake makefile mk
  when (not $ L.null (warnings ms)) $ do
    hPutStr stderr (warnings ms)
  when (not $ L.null (errors ms)) $ do
    fail (errors ms)
  case buildMake ms of
    Left e -> fail e
    Right s -> do
      o <- output s
      return (ms,o)

-- | A Version of @runMakeH@ returning no state
runMakeH_
  :: File -- ^ Output file
  -> Make a  -- ^ Make builder
  -> (String -> IO b) -- ^ Handler to output the file
  -> IO b
runMakeH_ f m h = snd `liftM` (runMakeH f m h)

-- | Execute the @mk@ monad, return the Makefile as a String.  In case of
-- errors, print report to stderr and abort the execution with @fail@ call
runMake :: Make a -> IO String
runMake mk = runMakeH_ defaultMakefile mk return

-- | Execute the @mk@ monad, build the Makefile, write it to the output file.
-- In case of errors, print report to stderr and abort the execution with @fail@
-- call
writeMake
  :: File -- ^ Output file
  -> Make a -- ^ Makefile builder
  -> IO ()
writeMake f mk = runMakeH_ f mk (writeFile (toFilePath f))

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

-- FIXME: buggy function, breaks commutativity
-- genTmpFile :: (MonadMake m) => String -> m File
-- genTmpFile cnt = tmpFile [] >>= \f -> genFile f cnt

-- FIXME: buggy function, breaks commutativity
-- genTmpFileWithPrefix :: (MonadMake m) => String -> String -> m File
-- genTmpFileWithPrefix pfx cnt = tmpFile pfx >>= \f -> genFile f cnt

