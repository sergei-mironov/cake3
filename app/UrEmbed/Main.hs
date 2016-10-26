module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Data
import Data.Either
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Options.Applicative
import System.IO
import System.FilePath
import Text.Printf

import Types
import C
import CSS
import JS
-- import Paths_cake3

pargs :: Parser Args
pargs = A
  <$> flag False True ( long "version" <> help "Show version information" )
  <*> strOption (short 'c' <> metavar "FILE.c" <> help "Output C-source")
  <*> strOption (short 'H' <> metavar "FILE.h" <> help "Output C-header file")
  <*> strOption (short 's' <> metavar "FILE.urs" <> help "Output Ur/Web signature file")
  <*> strOption (short 'w' <> metavar "FILE.ur" <> help "Output Ur/Web wrapper")
  <*> strOption (short 'j' <> metavar "FILE.urs" <> help "Output Ur/Web JS FFI signatures" <> value "")
  <*> switch (long css_mangle_flag <> help "Convert CSS url references to Ur/Web style")
  <*> argument str (metavar "FILE" <> help "File to embed")


main :: IO ()
main = do
  -- h <- (getDataFileName >=> readFile) "UrEmbedHelp.txt" 
  h <- return "a help"
  main_ =<< execParser (
    info (helper <*> pargs)
      (  fullDesc
      <> progDesc h
      <> header "UrEmebed allows including binary data into your Ur/Web project" ))

main_ a
  | bver a = do
    hPutStrLn stderr ("urembed version X")
    -- hPutStrLn stderr ("urembed version " ++ (show version))
  | otherwise = do

    c <-  ByteString.readFile (inp a)

    (c,u) <- if takeExtension (inp a) == ".css" && (mangle_css_url a) then
               case parse_css c of
                Left e -> do
                  hPutStrLn stderr $ "Warning: CSS parser failed ("++ (show e) ++ "), URL tracking disabled"
                  return (c, [])
                Right pr ->
                  return pr
             else
               return (c,[])

    j <- if (not $ null (out_ffi_js a)) then do
      pr <- parse_js c
      case pr of
        Left e -> do
          hPutStrLn stderr $ "Failed to parse JS: " ++ e
          return False
        Right js -> do
          mk_js_wrap a js
          return True
      else
        return False

    mk_c a c
    mk_h a
    mk_urs a
    mk_wrap a u j




