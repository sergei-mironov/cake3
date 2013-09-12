{-# LANGUAGE FlexibleContexts #-}
module Development.Cake3.Rules.UrWeb(urp) where

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
urpparse :: (Monad m) => String -> ((String,String) -> m ()) -> (FilePath -> m ()) -> m ()
urpparse inp hact sact = do
  parseline False (lines inp) where
    parseline _ [] = return ()
    parseline False (l:ls)
      | (unwords (words l)) == [] = parseline True ls
      | otherwise = hact (splitWhen ' ' l) >> parseline False ls
    parseline True (l:ls) = sact l >> parseline True ls

-- | Build dependencies of the URP file
moredeps :: File -> A ()
moredeps f = depend' f >> accessContents_ f (\c -> urpparse c lib src) where
  depend' f = prerequisites >>= \ps -> when (not (f`elem`ps)) (depend f)
  lib (h,x) = when (h=="library") $ moredeps (fromFilePath x)
  src d@(c:_) = when (c/='$') $ do
    depend' ((fromFilePath d) .= "ur")
    depend' ((fromFilePath d) .= "urs")

-- | Search for @sect@ in the urp file's header.
urpline :: String -> File -> File
urpline sect f = reactive sect f $ accessContents $ \c -> do
  flip execStateT [] $ urpparse c lib (const $ return ()) where
    lib (n,x) | n == sect = put x
              | otherwise = return ()

-- | Search for section @sect@ in the urp file's database line
-- FIXME: actually supports only 'databse dbname=XXX' format
urpdb :: String -> File -> File
urpdb dbsect f = reactive ("urpdb_"++dbsect) f $ accessContents $ \c -> do
  flip execStateT [] $ urpparse c lib (const $ return ()) where
    lib (n,x) | n == "database" = put (snd (splitWhen '=' x))
              | otherwise = return ()

-- | Get executable name of an URP project
urpexe :: File -> File
urpexe f = (takeBaseName f) .= "exe"

-- | Take the URP file and the build action. Provide three aliases: one for
-- executable, one for SQL-file and one for database file
urp :: File -> A () -> (Alias, Alias, Alias)
urp f act = rule (urpexe f, urpline "sql" f, urpdb "name" f) (act >> moredeps f)

