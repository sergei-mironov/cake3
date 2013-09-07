{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE IncoherentInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}
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
  , MakeState
  , A
  , shell
  , cmd
  , makevar
  , extvar
  , dst
  , runMake
  , phony
  , depend
  , Development.Cake3.unsafe
  , CommandGen(..)
  , unCommand
  , makefile
  , Rulable
  , module Control.Monad
  , module Development.Cake3.Writer
  , module System.FilePath.Wrapper
  ) where

import Prelude (Char(..), Bool(..), Maybe(..), Either(..), flip, ($), (+), undefined, error,not)

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
import System.FilePath.Wrapper
import Text.QuasiText
import Text.Printf

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

import Development.Cake3.Types
import Development.Cake3.Writer

file' root cwd f = makeRelative (File root) ((File cwd) </> (File f))

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO)

runMake :: [Alias] -> IO MakeState
runMake mks = execStateT (unMake (unalias $ reverse $ mks)) defMS

addRecipe r = getPos >>= \p -> modifyRecipes $ M.insertWith mappend (rtgt r) (Positioned p $ S.singleton r)

addVar v =  modifyVars $ M.insertWith mappend (vname v) (S.singleton v)

modifyRecipes f = modify $ \ms -> ms { srecipes = f (srecipes ms) }
modifyVars f = modify $ \ms -> ms { svars = f (svars ms) }
modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

getLoc :: Make String
getLoc = sloc <$> get

getPos = do
  p <- spos <$> get
  modify $ \ms -> ms { spos = p + 1 }
  return p

instance MonadLoc Make where
  withLoc l' (Make um) = Make $ do
    modifyLoc (\l -> l') >> um

newtype A a = A { unA :: StateT Recipe Make a }
  deriving(Monad, Functor, Applicative, MonadState Recipe, MonadIO)

-- | CommandGen is a recipe packed in the newtype to prevent partial expantion
newtype CommandGen = CommandGen (A Command)
unCommand (CommandGen a) = a

runA :: Recipe -> A a -> Make (a,Recipe)
runA r a = runStateT (unA a) r

execA :: Recipe -> A () -> Make Recipe
execA r a = snd <$> runA r a

newtype Alias = Alias (File, [File], Make ())

unalias :: [Alias] -> Make ()
unalias as = sequence_ $ map (\(Alias (_,_,x)) -> x) as

instance AsFile Alias where
  toFile (Alias (f,_,_) ) = f

type Rule = Alias
type Rules = [Alias]

-- instance AsFile (Rule Unit) where
--   toFile (a) = toFile a

class Rulable x l | x -> l where
  rule :: x -> A () -> l

phony name = rule' (alias_unit) (\x->[x]) True (File name)

collect :: (Foldable f) => f a -> [a]
collect = foldr (\a b -> a:b) []

alias_functor :: (Functor f, Foldable f) => f File -> Make () -> f Alias
alias_functor fs m = fmap (\f -> Alias (f, collect fs ,m)) fs

alias_unit :: File -> Make () -> Alias
alias_unit f m = Alias (f,[f],m)

-- FIXME: Oh, too ugly
makefile = File "Makefile"

rule' :: (x -> Make () -> y) -> (x -> [File]) -> Bool -> x -> A () -> y
rule' alias unfiles isPhony dst act = alias dst $ do
  loc <- getLoc
  r' <- execA (Recipe (unfiles dst) [] [] M.empty loc isPhony) act
  addRecipe r'
  return ()

instance Rulable File Alias where
  rule = rule' alias_unit (\x->[x]) False

instance (Functor f, Foldable f) => Rulable (f File) (f Alias) where
  rule = rule' alias_functor collect False

-- FIXME: depend can be used under unsafe but it doesn't work
unsafe :: A () -> A ()
unsafe action = do
  r <- get
  action
  modifyRecipe $ \r' -> r' { rsrc = rsrc r, rvars = rvars r }

modifyRecipe = modify

shell :: CommandGen -> A ()
shell cmd = do
  line <- unCommand cmd
  modifyRecipe (\r -> r { rcmd = (rcmd r) ++ [line] })

depend :: (Ref x) => x -> A ()
depend x = ref x >> return ()

var :: String -> Maybe String -> Make Variable
var n v = do
  let var = Variable n v
  addVar var
  return var

makevar :: String -> String -> Make Variable
makevar n v = var n (Just v)

extvar :: String -> Make Variable
extvar n = var n Nothing

dst :: A [File]
dst = rtgt <$> get

class Ref x where
  ref :: x -> A Command

instance Ref Variable where
  ref v@(Variable n _) = do
    modifyRecipe $ \r -> r { rvars = M.insertWith mappend n (S.singleton v) (rvars r) }
    return_text $ printf "$(%s)" n

instance Ref File where
  ref f = do
    modifyRecipe $ \r -> r { rsrc = f : (rsrc r)}
    return_file $ f

instance Ref Alias where
  ref (Alias (f,all,mr)) = do
    me <- get
    -- Prevents the recursion
    when (not $ f`elem`(rtgt me)) $ do
      A (lift mr) >> modifyRecipe (\r' -> r' { rsrc =  all ++ (rsrc r')})
    return_file f

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
      in appE [| (\l -> L.concat <$> (sequence l)) |] (listE chunks)

