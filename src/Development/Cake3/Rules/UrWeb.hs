{-# LANGUAGE FlexibleContexts #-}
module Development.Cake3.Rules.UrWeb(urp) where

import Data.Monoid
import Data.List
import Data.Set (member)
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

-- | Build dependencies for the file specified
moredeps :: File -> A ()
moredeps f = do
  ps <- prerequisites
  case f`member`ps of
    True -> return ()
    False -> do
      depend f
      inp <- liftIO (readFile $ unpack f)
      urpparse inp lib src
  where

    lib (h,x) = when (h=="library") $ do
      moredeps ((takeDirectory f) </> (fromFilePath x) .= "urp")

    src d@(c:_) = when (c/='$') $ do
      depend ((fromFilePath d) .= "ur" :: File)
      depend ((fromFilePath d) .= "urs" :: File)
    src [] = return ()

-- | Search for @sect@ in the urp file's header.
urpline :: String -> String -> IO File
urpline sect c = flip execStateT mempty $ urpparse c lib (const $ return ()) where
  lib (n,x) | n == sect = put (fromFilePath x)
            | otherwise = return ()

-- | Search for section @sect@ in the urp file's database line
-- FIXME: actually supports only 'databse dbname=XXX' format
urpdb :: String -> String -> IO File
urpdb dbsect c = flip execStateT mempty $ urpparse c lib (const $ return ()) where
  lib (n,x) | n == "database" = put (fromFilePath $ snd (splitWhen '=' x))
            | otherwise = return ()

-- | Get executable name of an URP project
urpexe :: File -> File
urpexe f = (takeBaseName f) .= "exe"

-- | Take the URP file and the build action. Provide three aliases: one for
-- executable, one for SQL-file and one for database file
urp :: File -> A() -> IO (Alias, Alias, Alias)
urp f act = do
  c <- liftIO (readFile $ unpack f)
  exe <- return (urpexe f)
  sql <- urpline "sql" c
  db <- urpdb "name" c
  ruleM (exe,sql,db) (act >> moredeps f)

