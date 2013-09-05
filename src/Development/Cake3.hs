{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE IncoherentInstances #-}

module Development.Cake3 (
    Recipe
  , Alias
  , Rule
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
  , (.=)
  , module Filesystem.Path.CurrentOS
  , module Control.Monad
  , module Development.Cake3.Writer
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Loc
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import qualified Data.List as L
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.String as S
import Prelude as P hiding (FilePath)
import System.IO hiding (FilePath)
import Text.QuasiText
import Text.Printf
import Filesystem.Path.CurrentOS
import Filesystem.Path.CurrentOS as P hiding (concat)

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

import Development.Cake3.Types
import Development.Cake3.Writer

(.=) f ext = P.replaceExtension f (S.fromString ext)

file' :: String -> String -> String -> FilePath
file' proot cwd pf = case P.stripPrefix (endslash proot) ((endslash cwd) </> (s2p pf)) of
  Just f -> f
  Nothing -> error $ printf "Failed to strip file prefix: cwd %s path %s </> %s" proot cwd pf
  where
    s2p = P.decodeString
    endslash [] = error "file': null arg "
    endslash x | last x == '/' = s2p x | otherwise = s2p $ x ++ "/"

-- FIXME: print to command line abstraction, not to the text
fileToText :: FilePath -> Text
fileToText = T.fromStrict . either id id . P.toText

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO)

runMake :: [[Alias]] -> IO MakeState
runMake mks = execStateT (unMake (unalias $ reverse $ P.concat mks)) defMS

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

execA r a = snd <$> runA r a

newtype Alias = Alias (FilePath, Make Recipe)

instance AsFile Alias where
  toFile (Alias (f,_) ) = f

type Rule = [Alias]

instance AsFile Rule where
  toFile [a] = toFile a
  toFile _ = error "Error: attempt to convert several aliases into one file"

alias :: (Functor f) => f FilePath -> Make Recipe -> f Alias
alias fs m = fmap (\f -> Alias (f,m)) fs

unalias :: [Alias] -> Make [Recipe]
unalias as = sequence $ map (\(Alias (_,x)) -> x) as

-- FIXME: too ugly
makefile = P.fromText "Makefile"

rule' :: Bool -> [FilePath] -> A () -> Rule
rule' isPhony dst act = alias dst $ do
  loc <- getLoc
  -- Don't include Makefile in the list of prerequisits if it is already a
  -- target
  -- FIXME: check for all possible ./Makefile ././Makefile Makefile and so on
  let s = if makefile `elem` dst then [] else [makefile]
      r = Recipe dst s [] M.empty loc isPhony
  r' <- execA r $ act
  addRecipe r'
  return r'

rule = rule' False

phony name = rule' True [P.decodeString name]

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

dst :: A [FilePath]
dst = rtgt <$> get

class Ref x where
  ref :: x -> A Command

instance Ref Variable where
  ref v@(Variable n _) = do
    modifyRecipe $ \r -> r { rvars = M.insertWith mappend n (S.singleton v) (rvars r) }
    return_text $ printf "$(%s)" n

instance Ref FilePath where
  ref f = do
    modifyRecipe $ \r -> r { rsrc = f : (rsrc r)}
    return_file $ f

instance Ref String where
  ref s = do return_text s

instance Ref Alias where
  ref (Alias (f,mr)) = A (lift mr) >> do
    modifyRecipe $ \r' -> r' { rsrc = f : (rsrc r')}
    return_file f

instance Ref x => Ref [x] where
  ref l = L.concat <$> mapM ref l

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
                       E t -> case parseExp (T.unpack $ T.fromStrict t) of
                                Left  e -> error e
                                Right e -> appE [| ref |] (return e)
                       V t -> appE [| ref |] (global (mkName (T.unpack $ T.fromStrict t)))
      in appE [| (\l -> L.concat <$> (sequence l)) |] (listE chunks)

