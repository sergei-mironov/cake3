{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Cake3.Rules.UrWeb2 where

import Data.Maybe
import Data.Monoid
import Data.List
import Data.Set (Set)
import Data.Set as S
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Network.Mime (guessMime)
import System.Directory
import Text.Printf
import System.IO as IO

import System.FilePath.Wrapper
import Development.Cake3
-- import Development.Cake3.Types
-- import Development.Cake3.Monad


data UrpState = UrpState {
    sqlfile :: Maybe File
  , uhdr :: [String]
  , ubdy :: [String]
  , ulibs :: [File]
  , udeps :: [File]
  , uobjs :: [File]
  } deriving (Show)

defState = UrpState Nothing mempty mempty mempty mempty mempty

newtype Urp a = Urp { unUrp :: StateT UrpState Make a }
  deriving(Functor, Applicative, Monad, MonadState UrpState, MonadMake)


toFile f wr = liftIO $ writeFile (toFilePath f) $ execWriter $ wr

line :: (MonadWriter String m) => String -> m ()
line s = tell (s++"\n")

runUrp :: String -> Urp () -> Make ()
runUrp urpfile_ m = do
  let urpfile = fromFilePath urpfile_
  ((),s) <- runStateT (unUrp m) defState
  let exefile = urpfile .= "exe"

  toFile urpfile $ do
    forM (uhdr s) line
    line ""
    forM (ubdy s) line

  rule $ do
    depend $ rule $ do
      produce urpfile
      forM_ (udeps s) $ \f -> do
        depend f
    produce exefile
    when ((sqlfile s) /= Nothing) $ do
      produce (fromJust $ sqlfile s)
    shell [cmd|urweb $(string $ takeBaseName urpfile)|]

  forM_ (uobjs s) $ \o -> do
    let incl = makevar "URINCL" "$(shell urweb -print-cinclude)"
    let cc = makevar "URCC" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=gcc)"
    rule $ do
      shell [cmd| $cc -c -I $incl -o %o @(o .= "c") |]

  forM_ (ulibs s) $ \l -> do
    prebuild [cmd| $make -C @l |]

  return ()


liftUrp m = m

liftMake m = Urp (lift m)

addHdr h = modify $ \s -> s { uhdr = (uhdr s) ++ [h]} 

dependOnFile f = modify $ \s -> s { udeps = (udeps s) ++ [f]} 

addModule f = do
  modify $ \s -> s { ubdy = (ubdy s) ++ [f]} 
  when ((f !! 0) /= '$') $ do
    dependOnFile (fromFilePath f .= "ur")
    dependOnFile (fromFilePath f .= "urs")

database :: String -> Urp ()
database db = addHdr $ printf "database %s" db
  
allow :: String -> String -> Urp ()
allow t p = addHdr $ printf "allow %s %s" t p

rewrite :: String -> String -> Urp ()
rewrite a b = addHdr $ printf "rewrite %s %s" a b

library :: String -> Urp ()
library l = do
  modify $ \s -> s { ulibs = (ulibs s) ++ [fromFilePath l]} 
  addHdr ("library " ++ l)

ur :: String -> Urp ()
ur s = addModule s

debug :: Urp ()
debug = addHdr "debug"

include :: FilePath -> Urp ()
include f = do
  addHdr $ printf "include %s" f
  dependOnFile (fromFilePath f)

link :: FilePath -> Urp ()
link f = do
  addHdr $ printf "link %s" f
  modify $ \s -> s { uobjs = (uobjs s) ++ [fromFilePath f] }
  dependOnFile (fromFilePath f)

ffi :: String -> Urp ()
ffi s = addHdr $ printf "ffi %s" s

sql :: String -> Urp ()
sql f = do
  addHdr $ printf "sql %s" f
  modify $ \s -> s { sqlfile = Just (fromFilePath f) }
  

binaryFile :: File -> Urp ()
binaryFile inf = 

  liftMake $ addMakeDep inf

  let modname = (mkname inf)
  let modname_c = modname ++ "_c"
  let blobname = modname ++ "_c_blob"
  let modname_js = modname ++ "_js"
  let mime = guessMime inf

  content <- liftIO $ BS.readFile inf

  let csrc = replaceExtension modname_c ".c"
  toFile csrc $ do
    line $ "// Thanks, http://stupefydeveloper.blogspot.ru/2008/08/cc-embed-binary-data-into-elf.html"
    line $ "#include <urweb.h>"
    line $ "#include <stdio.h>"
    -- let start = printf "_binary___%s_start" blobname
    -- let size = printf "_binary___%s_size" blobname
    line $ printf "#define BLOBSZ %d" (BS.length content)
    line $ "static char blob[BLOBSZ];"
    line $ "uw_Basis_blob " ++ binfunc ++ " (uw_context ctx, uw_unit unit)"
    line $ "{"
    line $ "  uw_Basis_blob uwblob;"
    line $ "  uwblob.data = &blob[0];"
    line $ "  uwblob.size = BLOBSZ;"
    line $ "  return uwblob;"
    line $ "}"
    line $ ""
    line $ "uw_Basis_string " ++ textfunc ++ " (uw_context ctx, uw_unit unit) {"
    line $ "  char* data = &blob[0];"
    line $ "  size_t size = sizeof(blob);"
    line $ "  char * c = uw_malloc(ctx, size+1);"
    line $ "  char * write = c;"
    line $ "  int i;"
    line $ "  for (i = 0; i < size; i++) {"
    line $ "    *write =  data[i];"
    line $ "    if (*write == '\\0')"
    line $ "    *write = '\\n';"
    line $ "    *write++;"
    line $ "  }"
    line $ "  *write=0;"
    line $ "  return c;"
    line $ "  }"
    line $ ""

  let append f wr = BS.appendFile (indest f) $ execWriter $ wr
  append (toFilePath csrc) $ do
    let line s = tell ((BS.pack s)`mappend`(BS.pack "\n"))
    line $ ""
    line $ "static char blob[BLOBSZ] = {"
    let buf = reverse $ BS.foldl (\a c -> (BS.pack (printf "0x%02X ," c)) : a) [] content
    tell (BS.concat buf)
    line $ "};"
    line $ ""

  let header = (replaceExtension modname_c ".h")
  write header $ do
    line $ "#include <urweb.h>"
    line $ "uw_Basis_blob " ++ binfunc ++ " (uw_context ctx, uw_unit unit);"
    line $ "uw_Basis_string " ++ textfunc ++ " (uw_context ctx, uw_unit unit);"

  let binobj = replaceExtension modname_c ".o"
  -- let dataobj = replaceExtension modname_c ".data.o"

  write (replaceExtension modname_c ".urp") $ do
    line $ "ffi " ++ modname_c
    line $ "include " ++ header
    line $ "link " ++ binobj
    -- line $ "link " ++ dataobj

  -- Copy the file to the target dir and run linker from there. Thus the names
  -- it places will be correct (see start,size in _c)
  -- copyFile inf (indest blobname)

  -- Module_js.urp
  (jstypes,jsdecls) <- if ((takeExtension inf) == ".js") then do
                          e <- parse_js inf
                          case e of
                            Left e -> do
                              err $ printf "Error while parsing %s" (takeFileName inf)
                              fail e
                            Right decls -> do
                              -- err (show decls)
                              return decls
                       else
                          return ([],[])

  write (replaceExtension modname_js ".urs") $ do
    forM_ jstypes $ \decl -> line (urtdecl decl)
    forM_ jsdecls $ \decl -> line (urdecl decl)

  write (replaceExtension modname_js ".urp") $ do
    line $ "ffi " ++ modname_js
    forM_ jsdecls $ \decl -> do
      line $ printf "jsFunc %s.%s = %s" modname_js (urname decl) (jsname decl)
      line $ printf "benignEffectful %s.%s" modname_js (urname decl)
  
  -- Module.urp
  write (replaceExtension modname ".urs") $ do
    line $ "val binary : unit -> transaction blob"
    line $ "val text : unit -> transaction string"
    line $ "val blobpage : unit -> transaction page"
    line $ "val geturl : url"
    forM_ jstypes $ \decl -> line (urtdecl decl)
    forM_ jsdecls $ \d -> line (urdecl d)

  write (replaceExtension modname ".ur") $ do
    line $ "val binary = " ++ modname_c ++ ".binary"
    line $ "val text = " ++ modname_c ++ ".text"
    forM_ jsdecls $ \d ->
      line $ printf "val %s = %s.%s" (urname d) modname_js (urname d)
    line $ printf "fun blobpage {} = b <- binary () ; returnBlob b (blessMime \"%s\")" mime
    line $ "val geturl = url(blobpage {})"

  write (replaceExtension modname ".urp") $ do
    line $ "library " ++ modname_c
    line $ "library " ++ modname_js
    line $ printf "safeGet %s/blobpage" modname
    line $ printf "safeGet %s/blob" modname
    line $ ""
    line $ modname

  where

    mkname :: File -> File
    mkname f = fromFilePath . upper1 . notnum . map under . takeFileName $ toFilePath f where
      under c | c`elem`"_-. /" = '_'
              | otherwise = c
      upper1 [] = []
      upper1 (x:xs) = (toUpper x) : xs
      notnum n@(x:xs) | isDigit x = "f" ++ n
                      | otherwise = n

