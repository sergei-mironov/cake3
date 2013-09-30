{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Development.Cake3.Rules.UrWeb(
    urweb
  , urembed
  , Config(..)
  , defaultConfig
  , urdeps
  ) where

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

data Config = Config {
    urObj :: File -> Rule
  , urInclude :: Variable
  }

defaultConfig = Config {
    urObj = \f -> rule f (fail "urobj: not set")
  , urInclude = makevar "UR_INCLUDE_DIR" "/usr/local/include/urweb"
  }

-- | Build dependencies for the file specified
urdeps :: Config -> File -> A ()
urdeps cfg f = do
  ps <- prerequisites
  case f`member`ps of
    True -> return ()
    False -> do
      depend f
      inp <- C3.readFile f
      urpparse inp lib src
  where
    relative x = takeDirectory f </> (fromFilePath x)

    lib (h,x)
      | (h=="library") = do
        let nested = relative x
        isdir <- liftIO $ doesDirectoryExist (unpack nested)
        case isdir of
          True -> urdeps cfg (nested </> (fromFilePath "lib.urp"))
          False -> urdeps cfg (nested .= "urp")
      | (h=="ffi") = do
        depend ((relative x) .= "urs")
      | (h=="include") = do
        depend (relative x)
      | (h=="link") = do
        depend (urObj cfg (relative x))
      | otherwise = return ()

    src d@(c:_)
      | (c/='$') = do
        depend ((relative d) .= "ur" :: File)
        depend ((relative d) .= "urs" :: File)
      | otherwise = return ()
    src _ = return ()

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
urweb' :: Config -> File -> A() -> IO (Alias, Alias, Alias)
urweb' cfg f act = do
  c <- liftIO (IO.readFile $ unpack f)
  exe <- return (urpexe f)
  sql <- urpline "sql" c
  db <- urpdb "name" c
  ruleM (exe,sql,db) (act >> urdeps cfg f)

urweb :: Config -> File -> A() -> IO (Alias, Alias, Alias)
urweb cfg f = urweb' cfg (f .= "urp")

-- | Generate Ur/Web project file @urp@ providing embedded files @files@
-- FIXME: Generate unique variable name instead of URGCC
urembed :: Config -> File -> [File] -> IO Alias
urembed cfg urp files = let
  dir = takeDirectory urp
  dir_ = string (toFilePath dir)
  urcc = makevar "URCC" "$(shell urweb -print-ccompiler)"
  gcc = makevar "CC" "$(shell $(URCC) -print-prog-name=gcc)"
  ld = makevar "LD" "$(shell $(URCC) -print-prog-name=ld)"
  in ruleM urp $ do
    depend $ rule (dir</>(fromFilePath "Makefile")) $ do
      shell [cmd|mkdir -pv $(dir_)|]
      shell [cmd|urembed -o $(dir_) $files|]
    -- FIXME: implement variable dependency
    depend urcc
    shell [cmd|$(extvar "MAKE") -C $(dir_) CC=$gcc LD=$ld UR_INCLUDE_DIR=$(urInclude cfg) urp|]

