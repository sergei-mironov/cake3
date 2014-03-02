{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Main where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Data.Either
import Data.Generics
import Data.Char
import Data.List
import Data.Data
import Data.Typeable
import Data.Maybe
import Data.String
-- import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Language.JavaScript.Parser
import Options.Applicative
import System.Environment
import System.Process
import System.Exit
import System.IO
-- import System.FilePath
import System.Directory
import System.Info
import Text.Printf

import Development.Cake3
import Development.Cake3.Ext.UrWeb


import Paths_cake3

io :: (MonadIO m) => IO a -> m a
io = liftIO

hio :: (MonadIO m) => Handle -> String -> m ()
hio h = io . hPutStrLn h

err,out :: (MonadIO m) => String -> m ()
err = hio stderr
out = hio stdout

data Args = A
  { tgtdir :: FilePath
  , fversion :: Bool
  , dontrunmake :: Bool
  , files :: [FilePath]
  }

pargs :: Parser Args
pargs = A
  <$> strOption
      (  long "output"
      <> short 'o'
      <> metavar "FILE.urp"
      <> help "Name of the Ur/Web project being generated"
      <> value "")
  <*> flag False True ( long "version" <> help "Show version information" )
  <*> flag False True ( long "dont-run-make" <> help "Do not run the makefile generated" )
  <*> arguments str ( metavar "FILE" <> help "File to embed" )

main :: IO ()
main = do
  h <- (getDataFileName >=> readFile) "UrEmbedHelp.txt" 
  main_ =<< execParser (
    info (helper <*> pargs)
      (  fullDesc
      <> progDesc h
      <> header "UrEmebed is the Ur/Web module generator" ))


main_ (A tgturp True drm ins) = do
  hPutStrLn stderr $ "urembed version " ++ (show version)

main_ (A tgturp False drm ins) = do
  let tgtdir = takeDirectory tgturp

  when (null tgtdir) $ do
    fail "An output directory should be specified, use -o"

  when (null ins) $ do
    fail "At least one file should be specified, see --help"

  -- exists <- doesDirectoryExist tgtdir
  -- when (not exists) $ do
  --   fail $ "Couldn't create output file, no such directory " ++ show tgtdir
  liftIO $ createDirectoryIfMissing True tgtdir

  when (null tgtdir) $ do
    fail "An output directory should be specified, use -o"

  when (null ins) $ do
    fail "At least one file should be specified, see --help"

  cntnts <- mapM BS.readFile ins

  setCurrentDirectory tgtdir
  loc <- currentDirLocation
  let file = file' loc
  let bin = bin' (file ".")

  let mk = (("." </> takeFileName tgturp) .= "mk")
  writeMake (file mk) $ do
    let urp = file (takeFileName tgturp)
    u <- uwlib urp $ do
      forM_ (ins`zip`cntnts) $ \(i,c) -> do
        bin i c
    rule $ do
      phony "all"
      depend u
  
  when (drm == False) $ do
    system $ printf "make -f %s" mk
    return ()

