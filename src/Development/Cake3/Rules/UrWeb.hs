{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Development.Cake3.Rules.UrWeb(urweb,urembed) where

import Data.Monoid
import Data.List
import Data.Set (member)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import System.Directory
import System.IO as IO

import System.FilePath.Wrapper
import Development.Cake3
import Development.Cake3.Types
import Development.Cake3.Monad as C3

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
      inp <- C3.readFile f
      urpparse inp lib src
  where
    lib (h,x) = when (h=="library") $ do
      let nested = (takeDirectory f) </> (fromFilePath x)
      isdir <- liftIO $ doesDirectoryExist (unpack nested)
      case isdir of
        True -> moredeps (nested </> (fromFilePath "lib.urp"))
        False -> moredeps (nested .= "urp")

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
--
-- FIXME: Rewrite in urembed style: fill urweb_cmd and pass it back to the user
urweb' :: File -> A() -> IO (Alias, Alias, Alias)
urweb' f act = do
  c <- liftIO (IO.readFile $ unpack f)
  exe <- return (urpexe f)
  sql <- urpline "sql" c
  db <- urpdb "name" c
  ruleM (exe,sql,db) (act >> moredeps f)

urweb :: File -> A() -> IO (Alias, Alias, Alias)
urweb f = urweb' (f .= "urp")

-- | Generate Ur/Web project file @urp@ providing embedded files @files@
-- FIXME: Generate unique variable name instead of URGCC
urembed :: File -> [File] -> (CommandGen -> A ()) -> IO Alias
urembed urp files act = let
  dir = takeDirectory urp
  fn = takeFileName urp
  in ruleM urp $ do
    depend (dir`rule`(shell [cmd| mkdir -pv $dir |]))
    let gcc = makevar "URGCC" "$(shell urweb -print-ccompiler)"
    act [cmd| urembed -d $(string (unpack fn)) -o $dir -c `which $gcc` $files |]

