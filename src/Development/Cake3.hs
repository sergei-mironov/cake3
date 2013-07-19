{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification, TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module Development.Cake3 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Data.Text
import Data.List as L
import Prelude as P
import System.FilePath
import System.IO
import Text.Printf
import Data.Text as T
import Text.QuasiText

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

type Command = String

data Variable = Variable { vname :: String, vval :: String }
  deriving(Show)

data Rule = Rule
  { rtgt :: FilePath
  , rsrc :: [FilePath]
  , rcmd :: [Command]
  , rvars :: [Variable]
  } deriving(Show)

newtype Make a = Make { unMake :: (WriterT [Rule] IO) a }
  deriving(Monad, Functor, Applicative, MonadWriter [Rule], MonadIO)

newtype A a = A { unA :: StateT Rule Make a }
  deriving(Monad, Functor, Applicative, MonadState Rule, MonadIO)

runA :: Rule -> A a -> Make (a,Rule)
runA r a = runStateT (unA a) r

execA r a = snd <$> runA r a

rule :: FilePath -> A () -> Make Rule
rule dst act = do
  let r = Rule dst [] [] []
  r' <- execA r $ act
  tell [r']
  return r'

sys :: Command -> A ()
sys s = modify (\r -> r { rcmd = s : rcmd r })

var :: String -> String -> A Variable
var n v = do
  let var = Variable n v
  modify (\r -> r { rvars = var:(rvars r) })
  return var

dst :: A FilePath
dst = rtgt <$> get






class Ref x where
  ref :: x -> A Text

instance Ref Variable where
  ref (Variable n _) = return $ T.pack n

instance Ref FilePath where
  ref f = return $ T.pack f

instance Ref Rule where
  ref r = return $ T.pack $ rtgt r

instance Ref x => Ref [x] where
  ref l = sequence (L.map ref l) >>= return . T.intercalate " "

instance Ref x => Ref (A x) where
  ref mx = mx >>= ref

instance Ref x => Ref (Make x) where
  ref mx = (A $ lift mx) >>= ref

instance Ref (A Variable) where
  ref mv = mv >>= \(Variable n _) -> T.pack <$> (pure n)

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
      in appE [| (\l -> T.unpack <$> T.concat <$> (sequence l) >>= sys ) |] (listE chunks)
  }
