{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Development.Cake3 (

    Alias
  , Variable
  , Recipe
  , Referrable

  -- Monads
  , A
  , Make
  , toMake
  , runMake

  -- Rules
  , rule
  , phony
  , depend
  , unsafe
  , defautlSelfUpdate
  
  -- Files
  , File
  , file'
  , (.=)
  , (</>)
  , takeBaseName

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
import Language.Haskell.TH hiding(unsafe)
import Language.Haskell.Meta (parseExp)

import Development.Cake3.Types
import Development.Cake3.Writer
import Development.Cake3.Monad
import System.FilePath.Wrapper

makefile :: File1
makefile = makefileT

file' :: String -> String -> String -> File1
file' root cwd f = (fromFilePath ".") </> makeRelative (fromFilePath root) ((fromFilePath cwd) </> (fromFilePath f))

defautlSelfUpdate = rule makefile $ do
  -- shell [cmd|./Cakegen > Makefile |]
  shell (CommandGen (concat <$> sequence [
      ref $ (fromFilePath ".") </> (fromFilePath "Cakegen" :: File1)
    , ref " > "
    , ref makefile]))

runMake :: [Alias] -> IO ()
runMake a = do
  (e,a) <- evalMake a
  mapM (hPutStrLn stderr) e
  hPutStrLn stdout (toMake a)

-- | CommandGen is a recipe packed in the newtype to prevent partial expantion
newtype CommandGen = CommandGen (A (Command File1))
unCommand (CommandGen a) = a

type Rule = Alias
type Rules = [Alias]

-- | Means that data structure f (containing Files) may be used to create data
-- structure a (containing Aliases).
class Rulable f a | f -> a where
  rule :: f -> A () -> a

phony name = rule' (alias_unit) (\x->[x]) True (fromFilePath name)

collect :: (Foldable f) => f a -> [a]
collect = foldr (\a b -> a:b) []

alias_functor :: (Functor f, Foldable f) => f File1 -> Make () -> f Alias
alias_functor fs m = fmap (\f -> Alias (f, collect fs ,m)) fs

alias_unit :: File1 -> Make () -> Alias
alias_unit f m = Alias (f,[f],m)

-- FIXME: Ah, boilerplate, boilerplate
alias_p2 :: (File1,File1) -> Make () -> (Alias,Alias)
alias_p2 (f1,f2) m = (Alias (f1,[f1,f2],m), Alias (f2,[f1,f2],m))
alias_p3 :: (File1,File1,File1) -> Make () -> (Alias,Alias,Alias)
alias_p3 (f1,f2,f3) m = (Alias (f1,l,m), Alias (f2,l,m), Alias (f3,l,m)) where
  l = [f1,f2,f3]

rule' :: (x -> Make () -> y) -> (x -> [File1]) -> Bool -> x -> A () -> y
rule' alias unfiles isPhony dst act = alias dst $ do
  loc <- getLoc
  runA (Recipe (unfiles dst) [] [] M.empty loc isPhony) act

instance Rulable File1 Alias where
  rule = rule' alias_unit (\x->[x]) False

instance Rulable (File1,File1) (Alias,Alias) where
  rule = rule' alias_p2 (\(a,b)->[a,b]) False

instance Rulable (File1,File1,File1) (Alias,Alias,Alias) where
  rule = rule' alias_p3 (\(a,b,c)->[a,b,c]) False

instance Rulable [File1] [Alias] where
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

dst :: A [File1]
dst = rtgt <$> get

-- | Data structure x may be referenced from within the command. Ref class
-- specifies side effects of such referencing. For example, referencig the file
-- leads to adding it to the prerequisites list.
class Ref x where
  ref :: x -> A (Command File1)

instance Ref Referrable where
  ref v@(Referrable s) = do
    return_text s

instance Ref Variable where
  ref v@(Variable n _) = do
    modify $ \r -> r { rvars = M.insertWith mappend n (S.singleton v) (rvars r) }
    return_text $ printf "$(%s)" n

-- Alias may be referenced from the recipe of itself, so we have to prevent
-- the recursion
not_myself :: File1 -> A () -> A ()
not_myself f act = targets >>= \fs -> when (not (f `elem` fs)) act

instance Ref File1 where
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

instance Ref [File1] where
  ref as = concat <$> (mapM ref as)

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

