{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Development.Cake3 (

    Alias
  , Variable
  , Recipe
  , Referal(..)
  , Placable(..)
  , Reference

  -- Monads
  , A
  , Make
  , toMake
  , runMake

  -- Rules
  , rule
  , ruleM
  , phony
  , phonyM
  , depend
  , unsafe
  , defaultSelfUpdate
  
  -- Files
  , FileLike(..)
  , File
  , file'
  , (.=)
  , (</>)

  -- Make parts
  , string
  , prerequisites
  , shell
  , cmd
  , makevar
  , extvar
  , dst
  , makefile
  , CommandGen(..)
  , unCommand

  -- More
  , module Control.Monad
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
import Text.QuasiText
import Text.Printf

import Language.Haskell.TH.Quote
import Language.Haskell.TH hiding(unsafe)
import Language.Haskell.Meta (parseExp)

import Development.Cake3.Types
import Development.Cake3.Writer
import Development.Cake3.Monad
import System.FilePath.Wrapper

makefile :: File
makefile = makefileT

file' :: String -> String -> String -> File
file' root cwd f = (fromFilePath ".") </> makeRelative (fromFilePath root) ((fromFilePath cwd) </> (fromFilePath f))

defaultSelfUpdate = rule makefile $ do
  -- shell [cmd|./Cakegen > Makefile |]
  shell (CommandGen (concat <$> sequence [
      ref $ (fromFilePath ".") </> (fromFilePath "Cakegen" :: File)
    , ref $ string " > "
    , ref makefile]))

runMake :: Make () -> IO ()
runMake mk = evalMake mk >>= output where
  output (Left err) = hPutStrLn stderr err
  output (Right a) = hPutStrLn stdout (toMake a)

-- | CommandGen is a recipe packed in the newtype to prevent partial expantion
newtype CommandGen = CommandGen (A Command)
unCommand (CommandGen a) = a

type Rule = Alias
type Rules = [Alias]

-- | Means that data structure f (containing Files) may be used to create data
-- structure a (containing Aliases).
class Rulable f a | f -> a where
  rule :: f -> A () -> a

ruleM :: (Monad m, Rulable f a) => f -> A () -> m a
ruleM a b = return (rule a b)

list1 a = [a]
fmap1 f a = f a

list2 (a1,a2) = [a1,a2]
fmap2 f (a1,a2) = (f a1,f a2)

list3 (a1,a2,a3) = [a1,a2,a3]
fmap3 f (a1,a2,a3) = (f a1,f a2,f a3)

list4 (a1,a2,a3,a4) = [a1,a2,a3,a4]
fmap4 f (a1,a2,a3,a4) = (f a1,f a2,f a3,f a4)

phony name = rule' fmap1 list1 True (fromFilePath name)

phonyM :: (Monad m) => String -> A () -> m Alias
phonyM n a = return $ phony n a

rule' fmapX listX isPhony dst act = flip fmapX dst $ \x -> Alias (x, listX dst, do
  loc <- getLoc
  runA (Recipe (S.fromList $ listX dst) mempty [] M.empty loc isPhony) act)

instance Rulable File Alias where
  rule = rule' fmap1 list1 False

instance Rulable (File,File) (Alias,Alias) where
  rule = rule' fmap2 list2 False

instance Rulable (File,File,File) (Alias,Alias,Alias) where
  rule = rule' fmap3 list3 False

instance Rulable (File,File,File,File) (Alias,Alias,Alias,Alias) where
  rule = rule' fmap4 list4 False

instance Rulable [File] [Alias] where
  rule = rule' map id False

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

depend :: (Referal x) => x -> A ()
depend x = ref x >> return ()

var :: String -> Maybe String -> Variable
var n v = Variable n v

makevar :: String -> String -> Variable
makevar n v = var n (Just v)

extvar :: String -> Variable
extvar n = var n Nothing

string :: String -> Reference
string s = Reference s

newtype Reference = Reference String

dst :: A (Set File)
dst = rtgt <$> get

-- | Data structure x may be referenced from within the command. Referal
-- class specifies side effects of such referencing. For example, referencig the
-- file leads to adding it to the prerequisites list.
class Referal x where
  ref :: x -> A Command

instance Referal Reference where
  ref v@(Reference s) = do
    return_text s

instance Referal Variable where
  ref v@(Variable n _) = do
    addVariable v
    return_text $ printf "$(%s)" n

-- Alias may be referenced from the recipe of itself, so we have to prevent
-- the recursion
not_myself :: File -> A a -> A ()
not_myself f act = targets >>= \ts -> do
  when (not (f `member` ts)) (act >> return ())

instance Referal File where
  ref f = do
    not_myself f $ do
      modify $ \r -> r { rsrc = f `insert` (rsrc r)}
    return_file f

instance Referal Alias where
  ref (Alias (f,_,mr)) = do
    not_myself f (A (lift mr))
    ref f

-- instance Referal [Char] where
--   ref s = return_text s

instance Referal [Alias] where
  ref as = concat <$> (mapM ref as)

instance Referal (Set File) where
  ref as = ref (S.toList as)

instance Referal [File] where
  ref fs = concat <$> mapM ref fs

instance Referal x => Referal (A x) where
  ref mx = mx >>= ref

instance Referal x => Referal (Make x) where
  ref mx = (A $ lift mx) >>= ref

instance Referal x => Referal (IO x) where
  ref mx = liftIO mx >>= ref

-- | Has effect of a function :: QQ -> CommandGen where QQ is a string supporting
-- $VARs. Each $VAR will be dereferenced using Ref typeclass. Result will
-- be equivalent to
--
-- return Command $ do
--   s1 <- ref "gcc "
--   s2 <- ref (flags :: Variable)
--   s3 <- ref " "
--   s4 <- ref (file :: File)
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
                       E t -> case parseExp (T.unpack t) of
                                Left  e -> error e
                                Right e -> appE [| ref |] (return e)
                       V t -> appE [| ref |] (global (mkName (T.unpack t)))
      in appE [| \l -> L.concat <$> (sequence l) |] (listE chunks)


