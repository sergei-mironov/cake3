{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Development.Cake3.Ext.UrWeb where

import Data.Data
import Data.Char
import Data.Typeable
import Data.Generics
import Data.Maybe
import Data.Monoid
import Data.List ()
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (Foldable(..), foldl')
import qualified Data.Foldable as F
import Data.ByteString.Char8 (ByteString(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.String
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Language.JavaScript.Parser
import Network.Mime (defaultMimeLookup)
import System.Directory
import Text.Printf
import qualified System.FilePath as F
import System.IO as IO

import System.FilePath.Wrapper
import Development.Cake3.Monad
import Development.Cake3

data UrpAllow = UrpMime | UrpUrl
  deriving(Show,Data,Typeable)

data UrpRewrite = UrpStyle
  deriving(Show,Data,Typeable)

data UrpHdrToken = UrpDatabase String
                 | UrpSql File
                 | UrpAllow UrpAllow String
                 | UrpRewrite UrpRewrite String
                 | UrpLibrary File
                 | UrpDebug
                 | UrpInclude File
                 | UrpLink File
                 | UrpFFI File
                 | UrpJSFunc String String String -- ^ Module name, UrWeb name, JavaScript name
                 | UrpSafeGet String
  deriving(Show,Data,Typeable)

data UrpModToken
  = UrpModule1 File
  | UrpModule2 File File
  | UrpModuleSys String
  deriving(Show,Data,Typeable)

data Urp = Urp {
    urp :: File
  , uexe :: Maybe File
  , uhdr :: [UrpHdrToken]
  , umod :: [UrpModToken]
  } deriving(Show,Data,Typeable)

newtype UWLib = UWLib Urp
  deriving (Show,Data,Typeable)

newtype UWExe = UWExe Urp
  deriving (Show,Data,Typeable)

instance (MonadAction a m) => RefInput a m UWLib where
  refInput (UWLib u) = refInput (urp u)
 
instance (MonadAction a m) => RefInput a m UWExe where
  refInput (UWExe u) = refInput (urpExe u)
 
class UrpLike x where
  toUrp :: x -> Urp
  tempfiles :: x -> [File]
  tempfiles = (\x -> urpObjs x ++ maybeToList (urpSql' x) ++ maybeToList (urpExe' x)) . toUrp

instance UrpLike Urp where
  toUrp = id

instance UrpLike UWLib where
  toUrp (UWLib x) = x
instance UrpLike UWExe where
  toUrp (UWExe x) = x

urpDeps :: Urp -> [File]
urpDeps (Urp _ _ hdr mod) = foldl' scan2 (foldl' scan1 mempty hdr) mod where
  scan1 a (UrpLink f) = f:a
  scan1 a (UrpInclude f) = f:a
  scan1 a _ = a
  scan2 a (UrpModule1 f) = f:a
  scan2 a (UrpModule2 f1 f2) = f1:f2:a
  scan2 a _ = a

urpSql' :: Urp -> Maybe File
urpSql' (Urp _ _ hdr _) = find hdr where
  find [] = Nothing
  find ((UrpSql f):hs) = Just f
  find (h:hs) = find hs

urpSql :: Urp -> File
urpSql u = case urpSql' u of
  Nothing -> error "ur project defines no SQL file"
  Just sql -> sql

urpObjs (Urp _ _ hdr _) = foldl' scan [] hdr where
  scan a (UrpLink f) = f:a
  scan a _ = a

urpLibs (Urp _ _ hdr _) = foldl' scan [] hdr where
  scan a (UrpLibrary f) = f:a
  scan a _ = a

urpExe' = uexe
urpExe u = case uexe u of
  Nothing -> error "ur project defines no EXE file"
  Just exe -> exe

data UrpState = UrpState {
    urpst :: Urp
  } deriving (Show)

defState urp = UrpState (Urp urp Nothing [] [])

class ToUrpWord a where
  toUrpWord :: a -> String

instance ToUrpWord UrpAllow where
  toUrpWord (UrpMime) = "mime"
  toUrpWord (UrpUrl) = "url"

instance ToUrpWord UrpRewrite where
  toUrpWord (UrpStyle) = "style"

class ToUrpLine a where
  toUrpLine :: FilePath -> a -> String

instance ToUrpLine UrpHdrToken where
  toUrpLine up (UrpDatabase dbs) = printf "database %s" dbs
  toUrpLine up (UrpSql f) = printf "sql %s" (up </> toFilePath f)
  toUrpLine up (UrpAllow a s) = printf "allow %s %s" (toUrpWord a) s
  toUrpLine up (UrpRewrite a s) = printf "rewrite %s %s" (toUrpWord a) s
  toUrpLine up (UrpLibrary f) = printf "library %s" (up </> toFilePath (dropExtensions f))
  toUrpLine up (UrpDebug) = printf "debug"
  toUrpLine up (UrpInclude f) = printf "include %s" (up </> toFilePath f)
  toUrpLine up (UrpLink f) = printf "link %s" (up </> toFilePath f)
  toUrpLine up (UrpFFI s) = printf "ffi %s" (up </> toFilePath (dropExtensions s))
  toUrpLine up (UrpSafeGet s) = printf "safeGet %s" (dropExtensions s)
  toUrpLine up (UrpJSFunc s1 s2 s3) = printf "jsFunc %s.%s = %s" s1 s2 s3
  toUrpLine up e = error $ "toUrpLine: unhandled case " ++ (show e)

instance ToUrpLine UrpModToken where
  toUrpLine up (UrpModule1 f) = up </> toFilePath (dropExtensions f)
  toUrpLine up (UrpModule2 f _) = up </> toFilePath (dropExtensions f)
  toUrpLine up (UrpModuleSys s) = printf "$/%s" s

newtype UrpGen m a = UrpGen { unUrpGen :: StateT UrpState m a }
  deriving(Functor, Applicative, Monad, MonadState UrpState, MonadMake, MonadIO)

instance (Monad m) => MonadAction (UrpGen (A' m)) m where
  liftAction a = UrpGen (lift a)

toFile f wr = liftIO $ writeFile (toFilePath f) $ execWriter $ wr

line :: (MonadWriter String m) => String -> m ()
line s = tell (s++"\n")

uwlib :: File -> UrpGen (A' (Make' IO)) () -> Make UWLib
uwlib urpfile m = do
  (_,u) <- rule2 $ do
    ((),s) <- runStateT (unUrpGen m) (defState urpfile)
    let u@(Urp _ _ hdr mod) = urpst s
    let up = urpUp urpfile

    toFile urpfile $ do
      forM hdr (line . toUrpLine up)
      line ""
      forM mod (line . toUrpLine up)

    forM_ (urpObjs u) $ \o -> do
      let incl = makevar "URINCL" "$(shell urweb -print-cinclude)"
      let cc = makevar "URCC" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=gcc)"
      rule2 $ do
        shell [cmd| $cc -c -I $incl -o @o $(o .= "c") |]

    depend (urpDeps u)
    depend (urpLibs u)
    shell [cmd|touch @urpfile|]
    return u

  return $ UWLib u

uwapp :: String -> File -> UrpGen (A' (Make' IO)) () -> Make UWExe
uwapp opts urpfile m = do
  (UWLib u') <- uwlib urpfile m
  let u = u' { uexe = Just (urpfile .= "exe") }
  rule $ do
    depend urpfile
    produce (urpExe u)
    case urpSql' u of
      Nothing -> return ()
      Just sql -> produce sql
    depend (makevar "URVERSION" "$(shell urweb -version)")
    unsafeShell [cmd|urweb $(string opts) $((takeDirectory urpfile)</>(takeBaseName urpfile))|]
  return $ UWExe u

liftUrp m = m

addHdr h = modify $ \s -> let u = urpst s in s { urpst = u { uhdr = (uhdr u) ++ [h] } }
addMod m = modify $ \s -> let u = urpst s in s { urpst = u { umod = (umod u) ++ [m] } }

database :: (MonadMake m) => String -> UrpGen m ()
database dbs = addHdr $ UrpDatabase dbs
  
allow :: (MonadMake m) => UrpAllow -> String -> UrpGen m ()
allow a s = addHdr $ UrpAllow a s

rewrite :: (MonadMake m) => UrpRewrite -> String -> UrpGen m ()
rewrite a s = addHdr $ UrpRewrite a s

urpUp :: File -> FilePath
urpUp f = F.joinPath $ map (const "..") $ filter (/= ".") $ F.splitDirectories $ F.takeDirectory $ toFilePath f

newtype UrEmbed = Urembed File
  deriving (Show)

data UrpLibReference
  = UrpLibStandalone File 
  | UrpLibInternal UWLib
  | UrpLibEmbed UrEmbed
  deriving(Show)

library' :: (MonadMake m) => File -> UrpGen m ()
library' l = do
  when ((takeExtension l) /= ".urp") $ do
    fail "library declaration for %s should ends with '.urp'" (toFilePath l)
  addHdr $ UrpLibrary l

library :: (MonadMake m) => UrpLibReference -> UrpGen m ()
library (UrpLibStandalone l) = do
  library' l
  when ((toFilePath $ takeDirectory l) /= ".") $ do
    prebuild [cmd| $(make) -C $(takeDirectory l) |]
library (UrpLibInternal (UWLib u)) = library' (urp u)
library (UrpLibEmbed ue) = error "urembed is not defined"

standalone f = UrpLibStandalone f
internal u = UrpLibInternal u
embed e = UrpLibEmbed e

ur, module_ :: (MonadMake m) => UrpModToken -> UrpGen m ()
module_ = addMod
ur = addMod

pair f = UrpModule2 (f.="ur") (f.="urs")
single f = UrpModule1 f
sys s = UrpModuleSys s

debug :: (MonadMake m) => UrpGen m ()
debug = addHdr $ UrpDebug

include :: (MonadMake m) => File -> UrpGen m ()
include f = addHdr $ UrpInclude f

link :: (MonadMake m) => File -> UrpGen m ()
link f = addHdr $ UrpLink f

ffi :: (MonadMake m) => File -> UrpGen m ()
ffi s = addHdr $ UrpFFI s

sql :: (MonadMake m) => File -> UrpGen m ()
sql f = addHdr $ UrpSql f
  
jsFunc m u j = addHdr $ UrpJSFunc m u j

safeGet s = addHdr $ UrpSafeGet s

url = UrpUrl

mime = UrpMime

style = UrpStyle

guessMime inf = fixup $ BS.unpack (defaultMimeLookup (fromString inf)) where
  fixup "application/javascript" = "text/javascript"
  fixup m = m

data JSFunc = JSFunc {
    urdecl :: String -- ^ URS declaration for this function
  , urname :: String -- ^ UrWeb name of this function
  , jsname :: String -- ^ JavaScript name of this function
  } deriving(Show)

data JSType = JSType {
    urtdecl :: String
  } deriving(Show)

-- | Parse the JavaScript file, extract top-level functions, convert their
-- signatures into Ur/Web format, return them as the list of strings
parse_js :: BS.ByteString -> Make (Either String ([JSType],[JSFunc]))
parse_js contents = do
  runErrorT $ do
    c <- either fail return (parse (BS.unpack contents) "<urembed_input>")
    f <- concat <$> (forM (findTopLevelFunctions c) $ \f@(fn:_) -> (do
      ts <- mapM extractEmbeddedType (f`zip`(False:repeat True))
      let urdecl_ = urs_line ts
      let urname_ = (fst (head ts))
      let jsname_ = fn
      return [JSFunc urdecl_ urname_ jsname_]
      ) `catchError` (\(e::String) -> do
        err $ printf "ignoring function %s, reason:\n\t%s" fn e
        return []))
    t <- concat <$> (forM (findTopLevelVars c) $ \vn -> (do
      (n,t) <- extractEmbeddedType (vn,False)
      return [JSType $ printf "type %s" t]
      )`catchError`  (\(e::String) -> do
        err $ printf "ignoring variable %s, reason:\n\t%s" vn e
        return []))
    return (t,f)

  where
    urs_line :: [(String,String)] -> String
    urs_line [] = error "wrong function signature"
    urs_line ((n,nt):args) = printf "val %s : %s" n (fmtargs args) where
      fmtargs :: [(String,String)] -> String
      fmtargs ((an,at):as) = printf "%s -> %s" at (fmtargs as)
      fmtargs [] = let pf = L.stripPrefix "pure_" nt in
                   case pf of
                     Just p -> p
                     Nothing -> printf "transaction %s" nt

    extractEmbeddedType :: (Monad m) => (String,Bool) -> m (String,String)
    extractEmbeddedType ([],_) = error "BUG: empty identifier"
    extractEmbeddedType (name,fallback) = check (msum [span2  "__" name , span2 "_as_" name]) where
      check (Just (n,t)) = return (n,t)
      check _ | fallback == True = return (name,name)
              | fallback == False = fail $ printf "Can't extract the type from the identifier '%s'" name

    findTopLevelFunctions :: JSNode -> [[String]]
    findTopLevelFunctions top = map decls $ listify is_func top where
      is_func n@(JSFunction a b c d e f) = True
      is_func _ = False
      decls (JSFunction a b c d e f) = (identifiers b) ++ (identifiers d)

    findTopLevelVars :: JSNode -> [String]
    findTopLevelVars top = map decls $ listify is_var top where
      is_var n@(JSVarDecl a []) = True
      is_var _ = False
      decls (JSVarDecl a _) = (head $ identifiers a);
      
    identifiers x = map name $ listify ids x where
      ids i@(JSIdentifier s) = True
      ids _ = False
      name (JSIdentifier n) = n

    err,out :: (MonadIO m) => String -> m ()
    err = hio stderr
    out = hio stdout

    span2 :: String -> String -> Maybe (String,String)
    span2 inf s = span' [] s where
      span' _ [] = Nothing
      span' acc (c:cs)
        | L.isPrefixOf inf (c:cs) = Just (acc, drop (length inf) (c:cs))
        | otherwise = span' (acc++[c]) cs

    hio :: (MonadIO m) => Handle -> String -> m ()
    hio h = liftIO . hPutStrLn h

bin :: (MonadIO m, MonadMake m) => File -> File -> UrpGen m ()
bin dir src = do
  c <- readFileForMake src
  bin' dir (toFilePath src) c

bin' :: (MonadIO m, MonadMake m) => File -> FilePath -> BS.ByteString -> UrpGen m ()
bin' dir src_name src_contents = do

  let mime = guessMime src_name
  let mn = (mkname src_name)
  let wrapmod ext = (dir </> mn) .= ext
  let binmod ext = (dir </> (mn ++ "_c")) .= ext
  let jsmod ext = (dir </> (mn ++ "_js")) .= ext

  -- Binary module
  let binfunc = printf "uw_%s_binary" (modname binmod)
  let textfunc = printf "uw_%s_text" (modname binmod)
  toFile (binmod ".c") $ do
    line $ "/* Thanks, http://stupefydeveloper.blogspot.ru/2008/08/cc-embed-binary-data-into-elf.html */"
    line $ "#include <urweb.h>"
    line $ "#include <stdio.h>"
    line $ printf "#define BLOBSZ %d" (BS.length src_contents)
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

  let append f wr = liftIO $ BS.appendFile f $ execWriter $ wr
  append (toFilePath (binmod ".c")) $ do
    let line s = tell ((BS.pack s)`mappend`(BS.pack "\n"))
    line $ ""
    line $ "static char blob[BLOBSZ] = {"
    let buf = reverse $ BS.foldl (\a c -> (BS.pack (printf "0x%02X ," c)) : a) [] src_contents
    tell (BS.concat buf)
    line $ "};"
    line $ ""

  toFile (binmod ".h") $ do
    line $ "#include <urweb.h>"
    line $ "uw_Basis_blob " ++ binfunc ++ " (uw_context ctx, uw_unit unit);"
    line $ "uw_Basis_string " ++ textfunc ++ " (uw_context ctx, uw_unit unit);"

  toFile (binmod ".urs") $ do
    line $ "val binary : unit -> transaction blob"
    line $ "val text : unit -> transaction string"

  include (binmod ".h")
  link (binmod ".o")
  ffi (binmod ".urs")

  -- JavaScript FFI Module
  (jstypes,jsdecls) <- if ((takeExtension src_name) == ".js") then do
                          e <- liftMake $ parse_js src_contents
                          case e of
                            Left e -> do
                              fail $ printf "Error while parsing %s" src_name
                            Right decls -> do
                              return decls
                       else
                          return ([],[])

  toFile (jsmod ".urs") $ do
    forM_ jstypes $ \decl -> line (urtdecl decl)
    forM_ jsdecls $ \decl -> line (urdecl decl)
  
  -- Wrapper module
  toFile (wrapmod ".urs") $ do
    line $ "val binary : unit -> transaction blob"
    line $ "val text : unit -> transaction string"
    line $ "val blobpage : unit -> transaction page"
    line $ "val geturl : url"
    forM_ jstypes $ \decl -> line (urtdecl decl)
    forM_ jsdecls $ \d -> line (urdecl d)

  toFile (wrapmod ".ur") $ do
    line $ "val binary = " ++ modname binmod ++ ".binary"
    line $ "val text = " ++ modname binmod ++ ".text"
    forM_ jsdecls $ \d ->
      line $ printf "val %s = %s.%s" (urname d) (modname jsmod) (urname d)
    line $ printf "fun blobpage {} = b <- binary () ; returnBlob b (blessMime \"%s\")" mime
    line $ "val geturl = url(blobpage {})"

  forM_ jsdecls $ \decl -> do
    addHdr $ UrpJSFunc (modname jsmod) (urname decl) (jsname decl)
  ffi (jsmod ".urs")

  safeGet $ printf "%s/blobpage" (modname wrapmod)
  safeGet $ printf "%s/blob" (modname wrapmod)
  module_ (pair $ wrapmod ".ur")

  where

    mkname :: FilePath -> String
    mkname = upper1 . notnum . map under . takeFileName where
      under c | c`elem`"_-. /" = '_'
              | otherwise = c
      upper1 [] = []
      upper1 (x:xs) = (toUpper x) : xs
      notnum n@(x:xs) | isDigit x = "f" ++ n
                      | otherwise = n

    modname :: (String -> File) -> String
    modname f = upper1 . takeBaseName $ f ".urs" where
      upper1 [] = []
      upper1 (x:xs) = (toUpper x) : xs

