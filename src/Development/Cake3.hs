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
  , runMakeH
  , runMakeH_
  , writeMake
  , includeMakefile
  , MonadMake(..)

  -- Rules
  , rule'
  , rule
  , phony
  , depend
  , produce
  , ignoreDepends
  , prebuild
  , postbuild

  -- Files
  , FileLike(..)
  , FileT(..)
  , File
  , ModuleLocation(..)
  , file'
  , (.=)
  , (</>)
  , topRel
  , readFileForMake
  , genFile'
  , genFile

  -- Make parts
  , prerequisites
  , shell
  , unsafeShell
  , cmd
  , makevar
  , extvar
  , tool
  , CommandGen'(..)
  , make
  , currentDirLocation

  -- Git
  , gitSubmoduleFile

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
import Data.Char
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

currentDirLocation :: (MonadIO m) => m ModuleLocation
currentDirLocation = return toplevelModule

-- | A Generic Make monad runner. Execute the monad @mk@, provide the @output@
-- handler with Makefile encoded as a string. Note that Makefile may contain
-- rules which references the file itself by the name @makefile@.  In case of
-- errors, print report to stderr and abort the execution with @fail@ call
runMakeH
  :: MakeState -- ^ Result of evalMake
  -> (String -> IO b) -- ^ Handler to output the file
  -> IO (MakeState,b)
runMakeH ms output = do
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
  :: MakeState -- ^ Result of evalMake
  -> (String -> IO b) -- ^ Handler to output the file
  -> IO b
runMakeH_ ms h = snd `liftM` (runMakeH ms h)

-- | Execute the @mk@ monad, return the Makefile as a String.  In case of
-- errors, print report to stderr and abort the execution with @fail@ call
runMake :: Make a -> IO String
runMake mk = do
  ms <- evalMake defaultMakefile mk
  runMakeH_ ms return

-- | Execute the @mk@ monad, build the Makefile, write it to the output file.
-- In case of errors, print report to stderr and abort the execution with @fail@
-- call
writeMake
  :: File -- ^ Output file
  -> Make a -- ^ Makefile builder
  -> IO ()
writeMake f mk = do
  ms <- evalMake defaultMakefile mk
  runMakeH_ ms (writeFile (topRel f))

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
rule' :: (MonadMake m)
  => A a -- ^ Recipe builder
  -> m (Recipe,a) -- ^ The recipe itself and the recipe builder's result
rule' act = liftMake $ do
  loc <- getLoc
  (r,a) <- runA loc act
  addRecipe r
  return (r,a)

-- | Create the rule, place it's recipe above all recipies defined so far. See
-- rule' for other details
rule
  :: A a    -- ^ Recipe builder
  -> Make a
rule act = snd `liftM` withPlacement (rule' act)

-- | A version of rule, without monad set explicitly
-- rule' :: (MonadMake m) => A a -> m a
-- rule' act = liftMake $ snd <$> withPlacement (rule2 act)

-- | Build a rule for creating file @tgt@ with a fixed content @cnt@, use
-- additional actions @act@ for the recipe
genFile' :: File -> String -> A () -> Make File
genFile' tgt cnt act =
  rule $ do
    case null cnt of
      True -> do
        shell [cmd| echo -n > @tgt|]
      False -> do
        shell [cmd|( \|]
        forM_ (lines cnt) $ \l -> do
          shell [cmd|echo $(string (quote l))  ;\|]
        shell [cmd|) > @tgt|]
    act
    return tgt
  where
    quote [] = []
    quote ('$':cs) = '$':'$':(quote cs)
    quote (x:cs)
      | not (isAlphaNum x) = '\\':x:(quote cs)
      | otherwise = x : (quote cs)

-- | Similar to @genFile' with empty additional action
genFile :: File -> String -> Make File
genFile f c = genFile' f c (return ())

-- FIXME: buggy function, breaks commutativity
-- genTmpFile :: (MonadMake m) => String -> m File
-- genTmpFile cnt = tmpFile [] >>= \f -> genFile f cnt

-- FIXME: buggy function, breaks commutativity
-- genTmpFileWithPrefix :: (MonadMake m) => String -> String -> m File
-- genTmpFileWithPrefix pfx cnt = tmpFile pfx >>= \f -> genFile f cnt

-- checkoutGitSubmoduleFile :: File -> Make File
-- checkoutGitSubmoduleFile f = rule $ do
--   shell [cmd| $(tool "git") -C $(fileModule f) submodule update --init |]
--   shell [cmd| $(tool "git") -C $(fileModule f) checkout -f |]
--   shell [cmd| $(tool "touch") -c @f|]
--   return f

gitSubmoduleFile :: File -> Make File
gitSubmoduleFile file = rule $
  let
    fm = fileModule file
  in do
  when ((splitDirectories (topRel fm)) /= ["."]) $ do
    shell [cmd| $(tool "git") -C $fm submodule update --init |]
    shell [cmd| $(tool "git") -C $fm checkout -f |]
    return ()
  shell [cmd| $(tool "touch") -c @(file)|]
  return file

--
-- TESTS
--

-- t1 :: Make ()
-- t1 = do
--   rule $ do
--     a <- rule' $ shell1 [cmd|echo a > @(file "a")|]
--     shell [cmd|cp $(snd a) @(file "b")|]
--   return ()
--   where
--     file = file' (ProjectLocation "." ".")




