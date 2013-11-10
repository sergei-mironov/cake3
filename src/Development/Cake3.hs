{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , include

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
  
  -- Files
  , FileLike(..)
  , File
  , file'
  , (.=)
  , (</>)
  , toFilePath

  -- Make parts
  , prerequisites
  , shell
  , cmd
  , makevar
  , extvar
  , makefile
  , CommandGen(..)

  -- More
  , module Control.Monad
  , module Control.Applicative
  ) where

import Prelude (id, Char(..), Bool(..), Maybe(..), Either(..), flip, ($), (+), (.), (/=), undefined, error,not)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Loc
import qualified Data.Text as T
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
import qualified System.FilePath as F
import Text.QuasiMake
import Text.Printf

import Language.Haskell.TH.Quote
import Language.Haskell.TH hiding(unsafe)
import Language.Haskell.Meta (parseExp)

import Development.Cake3.Types
import Development.Cake3.Writer
import Development.Cake3.Monad
import System.FilePath.Wrapper as W

file' :: String -> String -> String -> File
file' root cwd f' =
  let f = F.dropTrailingPathSeparator f' in
  (fromFilePath ".") </> makeRelative (fromFilePath root)
                                      ((fromFilePath cwd) </> (fromFilePath f))

selfUpdateRule :: Make Recipe
selfUpdateRule = do
  rule $ do
    shell (CommandGen (
      concat <$> sequence [
        refInput $ (fromFilePath ".") </> (fromFilePath "Cakegen" :: File)
      , refMerge $ string " > "
      , refOutput makefile]))

runMake' :: Make a -> (Either String String -> IO b) -> IO b
runMake' mk output = do
  ms <- evalMake (mk >> selfUpdateRule)
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

-- | CommandGen is a recipe packed in the newtype to prevent partial expantion
newtype CommandGen = CommandGen { unCommand :: A Command }

withPlacement :: Make (Recipe,a) -> Make (Recipe,a)
withPlacement mk = do
  p <- getPlacementPos
  (r,a) <- mk
  addPlacement p (S.findMin (rtgt r))
  return (r,a)

rule' :: A a -> Make (Recipe,a)
rule' act = do
  (r,a) <- runA act
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

shell :: CommandGen -> A ()
shell cmd = do
  line <- unCommand cmd
  modify (\r -> r { rcmd = (rcmd r) ++ [line] })

depend :: (RefInput x) => x -> A ()
depend x = refInput x >> return ()

before :: Make Recipe -> A ()
before mx =  liftMake mx >>= refInput >> return ()

produce :: (RefOutput x) => x -> A ()
produce x = refOutput x >> return ()

merge :: (RefMerge x) => x -> A ()
merge x = refMerge x >> return ()

var :: String -> Maybe String -> Variable
var n v = Variable n v

makevar :: String -> String -> Variable
makevar n v = var n (Just v)

extvar :: String -> Variable
extvar n = var n Nothing

newtype Reference = Reference String

class ReferenceLike a where
  string :: a -> Reference

instance ReferenceLike String where
  string s = Reference s

instance ReferenceLike File where
  string (FileT x) = string x

class RefMerge x where
  refMerge :: x -> A Command

instance RefMerge Variable where
  refMerge v@(Variable n _) = do
    addVariable v
    return_text $ printf "$(%s)" n

instance RefMerge CommandGen where
  refMerge cg = unCommand cg

instance RefMerge Reference where
  refMerge v@(Reference s) = do
    return_text s

instance RefMerge Command where
  refMerge = return


class RefOutput x where
  refOutput :: x -> A Command

instance RefOutput File where
  refOutput f = do
    modify $ \r -> r { rtgt = f `insert` (rtgt r)}
    return_file f

-- | Data structure x may be referenced from within the command. Referal
-- class specifies side effects of such referencing. For example, referencig the
-- file leads to adding it to the prerequisites list.
class RefInput x where
  refInput :: x -> A Command

instance RefInput File where
  refInput f = do
    modify $ \r -> r { rsrc = f `insert` (rsrc r)}
    return_file f

instance RefInput Recipe where
  refInput r = refInput (rtgt r)

instance RefInput x => RefInput [x] where
  refInput xs = concat <$> mapM refInput xs

instance RefInput x => RefInput (Set x) where
  refInput xs = refInput (S.toList xs)

instance RefInput x => RefInput (IO x) where
  refInput mx = liftIO mx >>= refInput

-- | Has effect of a function :: QQ -> CommandGen where QQ is a string supporting
-- $VARs. Each $VAR will be dereferenced using Ref typeclass. Result will
-- be equivalent to
--
-- return Command $ do
--   s1 <- refInput "gcc "
--   s2 <- refInput (flags :: Variable)
--   s3 <- refInput " "
--   s4 <- refInput (file :: File)
--   return (s1 ++ s2 ++ s3)
--
-- Later, this command may be examined or passed to the shell function to apply
-- it to the recepi
--
cmd :: QuasiQuoter
cmd = QuasiQuoter
  { quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  , quoteExp = \s -> appE [| \x -> CommandGen x |] (qqact s)
  } where
    qqact s = 
      let chunks = flip map (getChunks (S.fromString s)) $ \c ->
                     case c of
                       T t -> [| return_text t |]
                       E c t -> case parseExp (T.unpack t) of
                                  Left  e -> error e
                                  Right e -> case c of
                                    '@' -> appE [| refInput |] (return e)
                                    '%' -> appE [| refOutput |] (return e)
                                    _   -> appE [| refMerge |] (return e)
      in appE [| \l -> L.concat <$> (sequence l) |] (listE chunks)


