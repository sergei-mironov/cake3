{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE IncoherentInstances #-}

module Development.Cake3 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Data.Text as T
import Data.List as L
import Data.Map as M
import Data.Set as S
import Prelude as P
import System.FilePath
import System.IO
import Text.Printf
import Text.QuasiText
import Text.Printf

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

type Command = String

class ToMake x where
  toMake :: x -> [String]

data Variable = Variable { vname :: String, vval :: String }
  deriving(Show, Eq, Ord)

instance ToMake Variable where
  toMake (Variable n v) = [printf "%s = %s" n v]

data Rule = Rule
  { rtgt :: FilePath
  , rsrc :: [FilePath]
  , rcmd :: [Command]
  , rvars :: Map String (Set Variable)
  } deriving(Show, Eq, Ord)

instance ToMake Rule where
  toMake r = hdr : cmds where
    hdr = printf "%s : %s" (rtgt r) (L.intercalate " " (rsrc r))
    cmds = L.map ("\t" ++) (rcmd r)

type Rules = Map FilePath (Set Rule)

rules2vars :: Rules -> Map String (Set Variable)
rules2vars rules = M.unionsWith mappend (L.map rvars $ L.concat $ L.map S.toList (L.map snd $ M.toList rules))

rulesToMake :: Rules -> String
rulesToMake rules = L.unlines $ (L.concat $ L.map toMake vs) ++  (L.concat $ L.map toMake rs) where 
  vs = L.concat $ L.map S.toList $ L.map snd $ M.toList $ rules2vars rules
  rs = L.concat $ L.map S.toList $ L.map snd $ M.toList rules

newtype Make a = Make { unMake :: (StateT Rules IO) a }
  deriving(Monad, Functor, Applicative, MonadState Rules, MonadIO)

runMake :: Make a -> IO Rules
runMake mk = snd <$> runStateT (unMake mk) M.empty

addRule r = modify $ M.insertWith mappend (rtgt r) (S.singleton r)

newtype A a = A { unA :: StateT Rule Make a }
  deriving(Monad, Functor, Applicative, MonadState Rule, MonadIO)

runA :: Rule -> A a -> Make (a,Rule)
runA r a = runStateT (unA a) r

execA r a = snd <$> runA r a

rule :: FilePath -> A () -> Make Rule
rule dst act = do
  let r = Rule dst [] [] M.empty
  r' <- execA r $ act
  addRule r'
  return r'

sys :: Command -> A ()
sys s = modify (\r -> r { rcmd = s : rcmd r })

var :: String -> String -> A Variable
var n v = do
  let var = Variable n v
  modify (\r -> r { rvars = M.insertWith mappend n (S.singleton var) (rvars r) })
  return var

dst :: A FilePath
dst = rtgt <$> get

class Ref x where
  ref :: x -> A Text

instance Ref Variable where
  ref (Variable n _) = return $ T.pack $ printf "$(%s)" n

instance Ref FilePath where
  ref f = do
    modify $ \r -> r { rsrc = f : (rsrc r)}
    return $ T.pack f

instance Ref Rule where
  ref r = do
    modify $ \r' -> r' { rsrc = (rtgt r) : (rsrc r')}
    return $ T.pack $ rtgt r

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
      in appE [| (\l -> T.unpack <$> T.concat <$> (sequence l) >>= sys ) |] (listE chunks)
  }

