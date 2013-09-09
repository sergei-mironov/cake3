{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Development.Cake3 (
    Recipe
  , Alias
  , Rule
  , Rules
  , Variable
  , rule
  , file'
  , Make
  , string
  , Referrable
  , A
  , targets
  , prerequisites
  , shell
  , cmd
  , makevar
  , extvar
  , dst
  , phony
  , depend
  , Development.Cake3.unsafe
  , CommandGen(..)
  , unCommand
  , makefile
  , Rulable
  , runMake
  , toMake
  , module Control.Monad
  , module System.FilePath.Wrapper
  ) where

import Prelude (Char(..), Bool(..), Maybe(..), Either(..), flip, ($), (+), (.), (/=), undefined, error,not)

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
import Data.Set (Set)
import Data.String as S
import Data.Tuple
import System.IO
import Text.QuasiText
import Text.Printf

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

import Development.Cake3.Types
import Development.Cake3.Writer
import Development.Cake3.Monad
import System.FilePath.Wrapper

-- FIXME: Oh, too ugly
makefile = File "Makefile"

file' root cwd f = (File ".") </> makeRelative (File root) ((File cwd) </> (File f))

-- | CommandGen is a recipe packed in the newtype to prevent partial expantion
newtype CommandGen = CommandGen (A Command)
unCommand (CommandGen a) = a

type Rule = Alias
type Rules = [Alias]

-- | Means that data structure f (containing Files) may be used to create data
-- structure a (containing Aliases).
class Rulable f a | f -> a where
  rule :: f -> A () -> a

phony name = rule' (alias_unit) (\x->[x]) True (File name)

collect :: (Foldable f) => f a -> [a]
collect = foldr (\a b -> a:b) []

alias_functor :: (Functor f, Foldable f) => f File -> Make () -> f Alias
alias_functor fs m = fmap (\f -> Alias (f, collect fs ,m)) fs

alias_unit :: File -> Make () -> Alias
alias_unit f m = Alias (f,[f],m)

rule' :: (x -> Make () -> y) -> (x -> [File]) -> Bool -> x -> A () -> y
rule' alias unfiles isPhony dst act = alias dst $ do
  loc <- getLoc
  runA (Recipe (unfiles dst) [] [] M.empty loc isPhony) act

instance Rulable File Alias where
  rule = rule' alias_unit (\x->[x]) False

instance (Functor f, Foldable f) => Rulable (f File) (f Alias) where
  rule = rule' alias_functor collect False

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

depend :: (Ref x) => x -> A ()
depend x = ref x >> return ()

var :: String -> Maybe String -> Variable
var n v = Variable n v

makevar :: String -> String -> Variable
makevar n v = var n (Just v)

extvar :: String -> Variable
extvar n = var n Nothing

string :: String -> Referrable
string s = Referrable s

newtype Referrable = Referrable String

dst :: A [File]
dst = rtgt <$> get

-- | Data structure x may be referenced from within the command. Ref class
-- specifies side effects of such referencing. For example, referencig the file
-- leads to adding it to the prerequisites list.
class Ref x where
  ref :: x -> A Command

instance Ref Referrable where
  ref v@(Referrable s) = do
    return_text s

instance Ref Variable where
  ref v@(Variable n _) = do
    modify $ \r -> r { rvars = M.insertWith mappend n (S.singleton v) (rvars r) }
    return_text $ printf "$(%s)" n

-- Alias may be referenced from the recipe of itself, so we have to prevent
-- the recursion
not_myself :: File -> A () -> A ()
not_myself f act = targets >>= \fs -> when (not (f `elem` fs)) act

instance Ref File where
  ref f = do
    not_myself f $ do
      modify $ \r -> r { rsrc = f : (rsrc r)}
    return_file f

instance Ref Alias where
  ref (Alias (f,_,mr)) = do
    not_myself f (A (lift mr))
    ref f

instance Ref [Char] where
  ref s = return_text s

instance Ref [Alias] where
  ref as = concat <$> (mapM ref as)

instance Ref [File] where
  ref as = concat <$> (mapM ref as)

-- instance Ref x => Ref [x] where
--   ref l = L.concat <$> mapM ref l

instance Ref x => Ref (A x) where
  ref mx = mx >>= ref

instance Ref x => Ref (Make x) where
  ref mx = (A $ lift mx) >>= ref

instance Ref x => Ref (IO x) where
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

