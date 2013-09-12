{-# LANGUAGE FlexibleContexts #-}
module Development.Cake3.Rules.UrWeb where

import Data.Monoid
import Data.List
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State

import System.FilePath.Wrapper
import Development.Cake3
import Development.Cake3.Types
import Development.Cake3.Monad

splitWhen c l = (h,if null t then [] else tail t) where
  (h,t) = span (/=c) l

-- | URP file parser. Takes file name and 2 callbacks: one for header lines and
-- one for source lines
urpparse :: (MonadIO m) => FilePath -> ((String,String) -> m ()) -> (FilePath -> m ()) -> m ()
urpparse fp hact sact = do
  inp <- lines `liftM` (liftIO $ readFile fp)
  parseline False inp where
    parseline _ [] = return ()
    parseline False (l:ls)
      | (unwords (words l)) == [] = parseline True ls
      | otherwise = hact (splitWhen ' ' l) >> parseline False ls
    parseline True (l:ls) = sact l >> parseline True ls

-- | Builds dependencies of the URP file
deps :: File -> A ()
deps f = do
  depend' f
  withFile_ f $ \fp -> do
    urpparse fp lib src where
      depend' f = prerequisites >>= \ps -> when (not (f`elem`ps)) (depend f)
      lib (h,x) = when (h=="library") $ deps (fromFilePath x)
      src d@(c:_) = when (c/='$') $ do
        depend' ((fromFilePath d) .= "ur")
        depend' ((fromFilePath d) .= "urs")

urpline :: String -> File -> File
urpline sect f = fileTransform sect f $ \fp -> do
  flip execStateT [] $ urpparse fp lib (const $ return ()) where
    lib (n,x) | n == sect = put x
              | otherwise = return ()

urpdb :: String -> File -> File
urpdb dbsect f = fileTransform ("urpdb_"++dbsect) f $ \fp -> do
  flip execStateT [] $ urpparse fp lib (const $ return ()) where
    lib (n,x) | n == "database" = put (snd (splitWhen '=' x))
              | otherwise = return ()


urpexe :: File -> File
urpexe f = (takeBaseName f) .= "exe"

urp :: File -> A () -> ((Alias, Alias), File)
urp f act = (rule (urpexe f, urpline "sql" f) (act >> deps f), urpdb "name" f)

