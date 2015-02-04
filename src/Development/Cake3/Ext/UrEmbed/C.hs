-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Development.Cake3.Ext.UrEmbed.C where

import Control.Monad.State.Strict

import Data.Data
import Data.Either
import Data.Typeable
import Data.String
import Data.ByteString.Char8 (ByteString,pack,unpack)
import qualified Data.ByteString.Char8 as ByteString

import Network.Mime (defaultMimeLookup)

import System.Directory
import System.FilePath
import System.IO

import Text.Printf

import Development.Cake3.Ext.UrEmbed.Types
import Development.Cake3.Ext.UrEmbed.JS

-- toFile :: MonadIO m => FilePath -> Writer ByteString a -> m ()
-- toFile f wr = liftIO $ do
--   createDirectoryIfMissing True (takeDirectory f)
--   ByteString.writeFile f $ execWriter $ wr

toFile :: MonadIO m => FilePath -> StateT Handle IO () -> m ()
toFile f m = liftIO $ do
  createDirectoryIfMissing True (takeDirectory f)
  withFile f WriteMode (evalStateT m)

class LineLike x where
  line :: x -> StateT Handle IO ()

line_ putter s = get >>= \h -> liftIO $ putter h s

instance LineLike ByteString where
  line = line_ ByteString.hPutStrLn

instance LineLike [Char] where
  line = line . pack

instance LineLike [[Char]] where
  line s = forM_ s (\s -> line_ ByteString.hPutStr (pack s)) >> line ""

mk_c :: Args -> ByteString -> IO ()
mk_c a d = toFile (out_c a) $ do
  line $ "/* http://stupefydeveloper.blogspot.ru/2008/08/cc-embed-binary-data-into-elf.html */"
  line $ "#include <urweb.h>"
  line $ "#include <stdio.h>"
  line $ "#include \"" ++ (takeFileName (out_h a)) ++ "\""
  line $ ""
  line $ "#define BLOBSZ " ++ (printf "%d" (ByteString.length d))
  line $ "static char blob[BLOBSZ];"
  line $ ""
  line $ [ "uw_Basis_blob ", cblobfun a , " (uw_context ctx, uw_unit unit)" ]
  line $ "{"
  line $ "  uw_Basis_blob uwblob;"
  line $ "  uwblob.data = &blob[0];"
  line $ "  uwblob.size = BLOBSZ;"
  line $ "  return uwblob;"
  line $ "}"
  line $ ""
  line $ [ "uw_Basis_string " , ctextfun a , " (uw_context ctx, uw_unit unit) {" ]
  line $ "  char* data = &blob[0];"
  line $ "  size_t size = sizeof(blob);"
  line $ "  char * c = uw_malloc(ctx, size+1);"
  line $ "  char * write = c;"
  line $ "  int i;"
  line $ "  for (i = 0; i < size; i++) {"
  line $ "    *write =  data[i];"
  line $ "    if (*write == '\\0')"
  line $ "      *write = '\\n';"
  line $ "    *write++;"
  line $ "  }"
  line $ "  *write=0;"
  line $ "  return c;"
  line $ "  }"
  line $ ""
  line $ "static char blob[BLOBSZ] = {"
  h <- get
  liftIO $ do
    forM_ (unpack d) $ \c -> do
      hPutStr h (printf "0x%02X ," c)
  -- liftIO $ ByteString.foldl' (\act c -> act >> hPutStr h (printf "0x%02X ," c)) (return ()) d
  line $ "};"
  line $ ""

mk_h :: Args -> IO ()
mk_h a = toFile (out_h a) $ do
  line $ "#pragma once"
  line $ "#include <urweb.h>"
  line $ [ "uw_Basis_blob ", cblobfun a , " (uw_context ctx, uw_unit unit);" ]
  line $ [ "uw_Basis_string ", ctextfun a, " (uw_context ctx, uw_unit unit);" ]

mk_urs :: Args -> IO ()
mk_urs a = toFile (out_urs a) $ do
  line $ [ "val ", urblobfun, " : unit -> transaction blob" ]
  line $ [ "val ", urtextfun, " : unit -> transaction string" ]

guessMime inf = fixup $ unpack (defaultMimeLookup (fromString inf)) where
  fixup "application/javascript" = "text/javascript"
  fixup m = m

mk_wrap :: Args -> [Url] -> Bool -> IO ()
mk_wrap a us open_js_ffi = toFile (out_wrapper a) $ do
  let mm = guessMime (inp a)
  line $ "open " ++ (uwModName (out_urs a))
  line $ "fun content {} = b <- "++ urblobfun ++ " () ; returnBlob b (blessMime \"" ++ mm ++ "\")"
  line $ "val propagated_urls : list url = "
  forM_ us $ \u -> do
    line $ "    " ++ u ++ ".url ::"
  line $ "    []"
  when (open_js_ffi) $ do
    line $ "open " ++ (uwModName (out_ffi_js a))
  line $ "val url = url(content {})"

mk_js_wrap :: Args -> ([JSType],[JSFunc]) -> IO ()
mk_js_wrap a (jt,jf) = do
  toFile (out_ffi_js a) $ do
    forM_ jt $ \decl -> line (urtdecl decl)
    forM_ jf $ \decl -> line (urdecl decl)

