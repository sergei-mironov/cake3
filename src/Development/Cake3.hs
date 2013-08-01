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
  , File
  , file'
  , Make
  , MakeState
  , A
  , make
  , makevar
  , extvar
  , dst
  , (.=)
  , runMake
  , phony
  , depend
  , Development.Cake3.unsafe
  , module System.FilePath
  , module Control.Monad
  , module Development.Cake3.Writer
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Loc
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.List as L
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Prelude as P
import System.FilePath
import System.IO
import Text.QuasiText
import Text.Printf

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

import Development.Cake3.Types
import Development.Cake3.Writer

file' x = File (escape x)

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO)

runMake :: [[Alias]] -> IO MakeState
runMake mks = execStateT (unMake (unalias $ reverse $ concat mks)) defMS

addRecipe r = getPos >>= \p -> modifyRecipes $ M.insertWith mappend (rtgt' r) (Positioned p $ S.singleton r)

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

runA :: Recipe -> A a -> Make (a,Recipe)
runA r a = runStateT (unA a) r

execA r a = snd <$> runA r a

newtype Alias = Alias (File, Make Recipe)

type Rule = [Alias]

alias :: (Functor f) => f File -> Make Recipe -> f Alias
alias fs m = fmap (\f -> Alias (f,m)) fs

unalias :: [Alias] -> Make [Recipe]
unalias as = sequence $ map (\(Alias (_,x)) -> x) as

rule :: [File] -> A () -> Rule
rule dst act = alias dst $ do
  loc <- getLoc
  let r = Recipe dst [] [] M.empty loc False
  r' <- execA r $ act
  addRecipe r'
  return r'

phony :: String -> A () -> Rule
phony dst' act = let dst = [file' dst'] in alias dst $ do
    loc <- getLoc
    let r = Recipe dst [] [] M.empty loc True
    r' <- execA r $ act
    addRecipe r'
    return r'

-- FIXME: depend can be used under unsafe but it doesn't work
unsafe :: A () -> A ()
unsafe action = do
  r <- get
  action
  modifyRecipe $ \r' -> r' { rsrc' = rsrc' r, rvars = rvars r }

modifyRecipe = modify

sys :: Command -> A ()
sys s = modifyRecipe (\r -> r { rcmd = (rcmd r) ++ [s] })

cmd :: A String -> A ()
cmd ma = do
  str <- ma
  modifyRecipe (\r -> r { rcmd = (str : rcmd r) })

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
  ref :: x -> A Text

instance Ref Variable where
  ref v@(Variable n _) = do
    modifyRecipe $ \r -> r { rvars = M.insertWith mappend n (S.singleton v) (rvars r) }
    return $ T.pack $ printf "$(%s)" n

instance Ref File where
  ref f = do
    modifyRecipe $ \r -> r { rsrc' = f : (rsrc' r)}
    return $ T.pack $ unfile f

instance Ref String where
  ref s = do
    return $ T.pack s

instance Ref Alias where
  ref (Alias (f,mr)) = A (lift mr) >> do
    modifyRecipe $ \r' -> r' { rsrc' = f : (rsrc' r')}
    return $ T.pack $ unfile f

instance Ref x => Ref [x] where
  ref l = sequence (L.map ref l) >>= return . T.intercalate " "

instance Ref x => Ref (A x) where
  ref mx = mx >>= ref

instance Ref x => Ref (Make x) where
  ref mx = (A $ lift mx) >>= ref

make :: QuasiQuoter
make = QuasiQuoter
  { quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  , quoteExp = \s -> 
      let chunks = flip P.map (getChunks (T.pack s)) $ \c ->
                     case c of
                       T t -> [| return t |]
                       E t -> case parseExp (T.unpack t) of
                                Left  e -> error e
                                Right e -> appE [| ref |] (return e)
                       V t -> appE [| ref |] (global (mkName (T.unpack t)))
      in appE [| (\l -> T.unpack <$> T.concat <$> (sequence l) >>= sys) |] (listE chunks)
  }

(.=) :: File -> String -> File
(.=) (File (Escaped src)) newext = (File (Escaped $ src ++ newext))

