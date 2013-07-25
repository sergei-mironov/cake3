{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE IncoherentInstances #-}

module Development.Cake3 (
    Rule
  , rule
  , File
  , file'
  , Make
  , A
  , make
  , var
  , dst
  , (.=)
  , runMake
  , phony
  , depend
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

type Location = String

type MakeState = (Rules,Location)

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO)

runMake :: [Alias] -> IO Rules
runMake mks = fst <$> snd <$> runStateT (unMake (unalias mks)) mempty

addRule r = modifyRules $ M.insertWith mappend (rtgt' r) (S.singleton r)

modifyRules f = modify $ \(r,l) -> (f r,l)

modifyLoc f = modify $ \(r,l) -> (r,f l)

getLoc :: Make String
getLoc = snd <$> get

instance MonadLoc Make where
  withLoc l' (Make um) = Make $ do
    modifyLoc (\l -> l') >> um

newtype A a = A { unA :: StateT Rule Make a }
  deriving(Monad, Functor, Applicative, MonadState Rule, MonadIO)

runA :: Rule -> A a -> Make (a,Rule)
runA r a = runStateT (unA a) r

execA r a = snd <$> runA r a

newtype Alias = Alias (File, Make Rule)

alias :: [File] -> Make Rule -> [Alias]
alias fs m = map (\f -> Alias (f,m)) fs

unalias :: [Alias] -> Make [Rule]
unalias as = sequence $ map (\(Alias (_,x)) -> x) as

rule :: [File] -> A () -> [Alias]
rule dst act = alias dst $ do
  loc <- getLoc
  let r = Rule dst [] [] M.empty loc False
  r' <- execA r $ act
  addRule r'
  return r'

phony :: String -> A () -> [Alias]
phony dst' act = let dst = [file' dst'] in alias dst $ do
    loc <- getLoc
    let r = Rule dst [] [] M.empty loc True
    r' <- execA r $ act
    addRule r'
    return r'

modifyRule = modify

sys :: Command -> A ()
sys s = modifyRule (\r -> r { rcmd = (s : rcmd r) })

cmd :: A String -> A ()
cmd ma = do
  str <- ma
  modifyRule (\r -> r { rcmd = (str : rcmd r) })

depend :: Alias -> A ()
depend mr = ref mr >> return ()

var :: String -> String -> A Variable
var n v = do
  let var = Variable n v
  modifyRule (\r -> r { rvars = M.insertWith mappend n (S.singleton var) (rvars r) })
  return var

dst :: A [FilePath]
dst = rtgt <$> get

class Ref x where
  ref :: x -> A Text

instance Ref Variable where
  ref (Variable n _) = return $ T.pack $ printf "$(%s)" n

instance Ref File where
  ref f = do
    modifyRule $ \r -> r { rsrc' = f : (rsrc' r)}
    return $ T.pack $ unfile f

instance Ref String where
  ref s = do
    return $ T.pack s

instance Ref Alias where
  ref (Alias (f,mr)) = A (lift mr) >> do
    modifyRule $ \r' -> r' { rsrc' = f : (rsrc' r')}
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

