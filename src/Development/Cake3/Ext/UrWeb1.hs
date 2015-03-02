{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Language.JavaScript.Parser as JS
import Network.Mime (defaultMimeLookup)
import Text.Printf

import Text.Parsec as P hiding (string)
import Text.Parsec.Token as P hiding(lexeme, symbol)
import qualified Text.Parsec.Token as P
import Text.Parsec.ByteString as P

import qualified System.FilePath as F
import System.Directory
import System.IO as IO

import System.FilePath.Wrapper
import Development.Cake3.Monad
import Development.Cake3 hiding (many, (<|>))

data UrpAllow = UrpMime | UrpUrl | UrpResponseHeader | UrpEnvVar | UrpHeader
  deriving(Show,Data,Typeable)

data UrpRewrite = UrpStyle | UrpAll | UrpTable
  deriving(Show,Data,Typeable)

data UrpHdrToken = UrpDatabase String
                 | UrpSql File
                 | UrpAllow UrpAllow String
                 | UrpRewrite UrpRewrite String
                 | UrpLibrary File
                 | UrpDebug
                 | UrpInclude File
                 | UrpLink (Either File String)
                 | UrpSrc File String String
                 | UrpPkgConfig String
                 | UrpFFI File
                 | UrpJSFunc String String String -- ^ Module name, UrWeb name, JavaScript name
                 | UrpSafeGet String
                 | UrpScript String
                 | UrpClientOnly String
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
  tempfiles = (\x -> (urpObjs x) ++ maybeToList (urpSql' x) ++ maybeToList (urpExe' x)) . toUrp

instance UrpLike Urp where
  toUrp = id

instance UrpLike UWLib where
  toUrp (UWLib x) = x
instance UrpLike UWExe where
  toUrp (UWExe x) = x

urpDeps :: Urp -> [File]
urpDeps (Urp _ _ hdr mod) = foldl' scan2 (foldl' scan1 mempty hdr) mod where
  scan1 a (UrpLink (Left f)) = f:a
  scan1 a (UrpSrc f _ _) = (f.="o"):a
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

urpSrcs (Urp _ _ hdr _) = foldl' scan [] hdr where
  scan a (UrpSrc f cfl lfl) = (f,cfl):a
  scan a _ = a

urpObjs (Urp _ _ hdr _) = foldl' scan [] hdr where
  scan a (UrpSrc f _ lfl) = (f.="o"):a
  scan a (UrpLink (Left f)) = (f):a
  scan a _ = a

urpLibs (Urp _ _ hdr _) = foldl' scan [] hdr where
  scan a (UrpLibrary f) = f:a
  scan a _ = a

urpExe' = uexe
urpExe u = case uexe u of
  Nothing -> error "ur project defines no EXE file"
  Just exe -> exe

urpPkgCfg (Urp _ _ hdr _) = foldl' scan [] hdr where
  scan a (UrpPkgConfig s) = s:a
  scan a _ = a

data UrpState = UrpState {
    urpst :: Urp
  , urautogen :: File
  } deriving (Show)

defState urp = UrpState (Urp urp Nothing [] []) (fromFilePath "autogen")

class ToUrpWord a where
  toUrpWord :: a -> String

instance ToUrpWord UrpAllow where
  toUrpWord (UrpMime) = "mime"
  toUrpWord (UrpHeader) = "requestHeader"
  toUrpWord (UrpUrl) = "url"
  toUrpWord (UrpEnvVar) = "env"
  toUrpWord (UrpResponseHeader) = "responseHeader"

instance ToUrpWord UrpRewrite where
  toUrpWord (UrpStyle) = "style"
  toUrpWord (UrpAll) = "all"
  toUrpWord (UrpTable) = "table"

class ToUrpLine a where
  toUrpLine :: FilePath -> a -> String

maskPkgCfg s = "%" ++ (map toUpper s) ++ "%"

instance ToUrpLine UrpHdrToken where
  toUrpLine up (UrpDatabase dbs) = printf "database %s" dbs
  toUrpLine up (UrpSql f) = printf "sql %s" (up </> toFilePath f)
  toUrpLine up (UrpAllow a s) = printf "allow %s %s" (toUrpWord a) s
  toUrpLine up (UrpRewrite a s) = printf "rewrite %s %s" (toUrpWord a) s
  toUrpLine up (UrpLibrary f)
    | (takeFileName f) == "lib.urp" = printf "library %s" (up </> toFilePath (takeDirectory f))
    | otherwise = printf "library %s" (up </> toFilePath (dropExtension f))
  toUrpLine up (UrpDebug) = printf "debug"
  toUrpLine up (UrpInclude f) = printf "include %s" (up </> toFilePath f)
  toUrpLine up (UrpLink (Left f)) = printf "link %s" (up </> toFilePath f)
  toUrpLine up (UrpLink (Right lfl)) = printf "link %s" lfl
  toUrpLine up (UrpSrc f _ _) = printf "link %s" (up </> toFilePath (f.="o"))
  toUrpLine up (UrpPkgConfig s) = printf "link %s" (maskPkgCfg s)
  toUrpLine up (UrpFFI s) = printf "ffi %s" (up </> toFilePath (dropExtensions s))
  toUrpLine up (UrpSafeGet s) = printf "safeGet %s" (dropExtensions s)
  toUrpLine up (UrpJSFunc s1 s2 s3) = printf "jsFunc %s.%s = %s" s1 s2 s3
  toUrpLine up (UrpScript s) = printf "script %s" s
  toUrpLine up (UrpClientOnly s) = printf "clientOnly %s" s
  toUrpLine up e = error $ "toUrpLine: unhandled case " ++ (show e)

instance ToUrpLine UrpModToken where
  toUrpLine up (UrpModule1 f) = up </> toFilePath (dropExtensions f)
  toUrpLine up (UrpModule2 f _) = up </> toFilePath (dropExtensions f)
  toUrpLine up (UrpModuleSys s) = printf "$/%s" s

newtype UrpGen m a = UrpGen { unUrpGen :: StateT UrpState m a }
  deriving(Functor, Applicative, Monad, MonadState UrpState, MonadMake, MonadIO)

toFile f' wr = liftIO $ do
  let f = toFilePath f'
  createDirectoryIfMissing True (takeDirectory f)
  writeFile f $ execWriter $ wr

tempPrefix :: File -> String
tempPrefix f = concat $ map (map nodot) $ splitDirectories f where
  nodot '.' = '_'
  nodot '/' = '_'
  nodot a = a

mkFileRule pfx wr = genFile (tmp_file pfx) $ execWriter $ wr

line :: (MonadWriter String m) => String -> m ()
line s = tell (s++"\n")

uwlib :: File -> UrpGen (Make' IO) () -> Make UWLib
uwlib urpfile m = do
  ((),s) <- runStateT (unUrpGen m) (defState urpfile)
  let u@(Urp _ _ hdr mod) = urpst s
  let pkgcfg = (urpPkgCfg u)

  forM_ (urpSrcs u) $ \(c,fl) -> do
    let flags = concat $ fl : map (\p -> printf "$(shell pkg-config --cflags %s) " p) (urpPkgCfg u)
    let i = makevar "URINCL" "-I$(shell urweb -print-cinclude) " 
    let cc = makevar "URCC" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=gcc)"
    let cpp = makevar "URCPP" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=g++)"
    let incfl = extvar "UR_CFLAGS"
    rule' $ do
      case takeExtension c of
        ".cpp" -> shell [cmd| $cpp -c $incfl $i $(string flags) -o @(c .= "o") $(c) |]
        ".c" -> shell [cmd| $cc -c $i $incfl $(string flags) -o @(c .= "o") $(c) |]
        e -> error ("Unknown C-source extension " ++ e)

  inp_in <- mkFileRule (tempPrefix (urpfile .= "in")) $ do
      forM hdr (line . toUrpLine (urpUp urpfile))
      line ""
      forM mod (line . toUrpLine (urpUp urpfile))

  rule' $ do
    let cpy = [cmd|cat $inp_in|] :: CommandGen' (Make' IO)
    let l = foldl' (\a p -> do
                            let l = makevar (map toUpper $ printf "lib%s" p) (printf "$(shell pkg-config --libs %s)" p)
                            [cmd| $a | sed 's@@$(string $ maskPkgCfg p)@@$l@@'  |]
                            ) cpy pkgcfg
    shell [cmd| $l > @urpfile |]
    depend (urpDeps u)
    depend (urpLibs u)

  return $ UWLib u

uwapp :: String -> File -> UrpGen (Make' IO) () -> Make UWExe
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

setAutogenDir d = modify $ \s -> s { urautogen = d }

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

class LibraryLike x where
  library :: (MonadMake m) => x -> UrpGen m ()

instance LibraryLike [File] where
  library  ls = do
    forM_ ls $ \l -> do
      when ((takeExtension l) /= ".urp") $ do
        fail $ printf "library declaration '%s' should ends with '.urp'" (toFilePath l)
      addHdr $ UrpLibrary l

instance LibraryLike UWLib where
  library (UWLib u) = library [urp u]

instance LibraryLike x => LibraryLike (Make x) where
  library  ml = liftMake ml >>= library

-- | Build a file using external Makefile facility.
externalMake3 ::
     File -- ^ External Makefile
  -> File -- ^ External file to refer to
  -> String -- ^ The name of the target to run
  -> Make [File]
externalMake3 mk f tgt = do
  prebuildS [cmd|$(make) -C $(string $ toFilePath $ takeDirectory mk) -f $(string $ takeFileName mk) $(string tgt) |]
  return [f]

-- | Build a file using external Makefile facility.
externalMake' ::
     File -- ^ External Makefile
  -> File -- ^ External file to refer to
  -> Make [File]
externalMake' mk f = do
  prebuildS [cmd|$(make) -C $(string $ toFilePath $ takeDirectory mk) -f $(string $ takeFileName mk)|]
  return [f]

-- | Build a file from external project. It is expected, that this project has a
-- 'Makwfile' in it's root directory. Call Makefile with the default target
externalMake ::
     File -- ^ File from the external project to build
  -> Make [File]
externalMake f = externalMake3 (takeDirectory f </> "Makefile") f ""

-- | Build a file from external project. It is expected, that this project has a
-- 'Makwfile' in it's root directory
externalMakeTarget ::
     File -- ^ File from the external project to build
  -> String
  -> Make [File]
externalMakeTarget f tgt = externalMake3 (takeDirectory f </> "Makefile") f tgt

-- | Build a file from external project. It is expected, that this project has a
-- fiel.mk (a Makefile with an unusual name) in it's root directory
externalMake2 :: File -> Make [File]
externalMake2 f = externalMake' ((takeDirectory f </> takeFileName f) .= "mk") f

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

link' :: (MonadMake m) => File -> String -> UrpGen m ()
link' f fl = do
  addHdr $ UrpLink (Left f)
  when (fl /= "") $ do
    addHdr $ UrpLink (Right fl)

link :: (MonadMake m) => File -> UrpGen m ()
link f = link' f []

csrc'  :: (MonadMake m) => File -> String -> String -> UrpGen m ()
csrc' f cfl lfl = do
  addHdr $ UrpSrc f cfl lfl
  when (lfl /= "") $ do
    addHdr $ UrpLink (Right lfl)

csrc  :: (MonadMake m) => File -> UrpGen m ()
csrc f = csrc' f [] []

ffi :: (MonadMake m) => File -> UrpGen m ()
ffi s = addHdr $ UrpFFI s

sql :: (MonadMake m) => File -> UrpGen m ()
sql f = addHdr $ UrpSql f
  
jsFunc m u j = addHdr $ UrpJSFunc m u j

safeGet' :: (MonadMake m) => String -> UrpGen m ()
safeGet' uri
  | otherwise = addHdr $ UrpSafeGet uri

safeGet :: (MonadMake m) => File -> String -> UrpGen m ()
safeGet m fn
  | (takeExtension m) /= ".ur" = fail (printf "safeGet: not an Ur/Web module name specified (%s)" (toFilePath m))
  | otherwise = safeGet' (printf "%s/%s" (takeBaseName m) fn)

url = UrpUrl

mime = UrpMime

style = UrpStyle

all = UrpAll

table = UrpTable

env = UrpEnvVar

hdr = UrpHeader

requestHeader = UrpHeader

responseHeader = UrpResponseHeader

script :: (MonadMake m) => String -> UrpGen m ()
script s = addHdr $ UrpScript s

guessMime inf = fixup $ BS.unpack (defaultMimeLookup (fromString inf)) where
  fixup "application/javascript" = "text/javascript"
  fixup m = m

pkgconfig :: (MonadMake m) => String -> UrpGen m ()
pkgconfig l = addHdr $ UrpPkgConfig l


type BinOptions = [ BinOption ]
data BinOption = NoScan | UseUrembed deriving(Show, Eq)


bin :: (MonadIO m, MonadMake m) => File -> BinOptions -> UrpGen m ()
bin src bo = do
  let ds = if NoScan `elem` bo then "--dont-scan" else ""
  case UseUrembed `elem` bo of
    False -> do
      c <- readFileForMake src
      bin' (toFilePath src) c bo
    True -> do
      a <- urautogen `liftM` get
      library $ do
        rule $ shell [cmd|urembed -o @(a </> (takeFileName src .="urp")) $(string ds) $src|]

bin' :: (MonadIO m, MonadMake m) => FilePath -> BS.ByteString -> BinOptions -> UrpGen m ()
bin' src_name src_contents' bo = do

  dir <- urautogen `liftM` get

  let mm = guessMime src_name
  let mn = (mkname src_name)

  let wrapmod ext = (dir </> mn) .= ext
  let binmod ext = (dir </> (mn ++ "_c")) .= ext
  let jsmod ext = (dir </> (mn ++ "_js")) .= ext

  (src_contents, nurls) <-
    if not (NoScan `elem` bo) then
    if ((takeExtension src_name) == ".css") then do
        (e,urls) <- return $ runWriter $ parse_css src_contents' $ \x -> do
          let (url, query) = span (\c -> not $ elem c "?#") x
          let mn = modname (const (fromFilePath $ mkname url))
          tell [ mn ]
          return $ "/" ++ mn ++ "/blobpage" ++ query
        case e of
          Left e -> do
            fail $ printf "Error while parsing css %s: %s" src_name (show e)
          Right b -> do
            return (b, L.nub urls)
    else
      return (src_contents', [])
    else
      return (src_contents', [])

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
    line $ "      *write = '\\n';"
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
  csrc (binmod ".c")
  ffi (binmod ".urs")

  -- JavaScript FFI Module
  (jstypes,jsdecls) <- 
    if not (NoScan `elem` bo) then
    if ((takeExtension src_name) == ".js") then do
      e <- liftMake $ parse_js src_contents
      case e of
        Left e -> do
          fail $ printf "Error while parsing javascript %s: %s" src_name e
        Right decls -> do
          return decls
    else
      return ([],[])
    else
      return ([],[])

  forM_ jsdecls $ \decl -> do
    addHdr $ UrpJSFunc (modname jsmod) (urname decl) (jsname decl)
    addHdr $ UrpClientOnly $ (modname jsmod) ++ "." ++ (urname decl)
  toFile (jsmod ".urs") $ do
    forM_ jstypes $ \decl -> line (urtdecl decl)
    forM_ jsdecls $ \decl -> line (urdecl decl)
  ffi (jsmod ".urs")
  
  -- Wrapper module
  toFile (wrapmod ".urs") $ do
    line $ "val binary : unit -> transaction blob"
    line $ "val text : unit -> transaction string"
    line $ "val blobpage : unit -> transaction page"
    line $ "val geturl : url"
    forM_ jstypes $ \decl -> line (urtdecl decl)
    forM_ jsdecls $ \d -> line (urdecl d)
    line $ "val propagated_urls : list url"

  toFile (wrapmod ".ur") $ do
    line $ "val binary = " ++ modname binmod ++ ".binary"
    line $ "val text = " ++ modname binmod ++ ".text"
    forM_ jsdecls $ \d ->
      line $ printf "val %s = %s.%s" (urname d) (modname jsmod) (urname d)
    line $ printf "fun blobpage {} = b <- binary () ; returnBlob b (blessMime \"%s\")" mm
    line $ "val geturl = url(blobpage {})"
    line $ "val propagated_urls = "
    forM_ nurls $ \u -> do
      line $ "    " ++ u ++ ".geturl ::"
    line $ "    []"

  allow mime mm
  safeGet (wrapmod ".ur") "blobpage"
  safeGet (wrapmod ".ur") "blob"

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

{-
 - Content parsing helpers
 -}

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
    c <- either fail return (JS.parse (BS.unpack contents) "<urembed_input>")
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
      decls (JSFunction a b c d e f) = (identifiers b) ++ (identifiersC d)

    findTopLevelVars :: JSNode -> [String]
    findTopLevelVars top = map decls $ listify is_var top where
      is_var n@(JSVarDecl a []) = True
      is_var _ = False
      decls (JSVarDecl a _) = (head $ identifiers a);

    identifiersC x = map name $ listify ids x where
      ids i@(NT (JSIdentifier s) _ com) = True
      ids _ = False
      name (NT (JSIdentifier n) _ com)
        | not $ null $ comglue = n ++ "_as_" ++ comglue
        | otherwise = n
        where
          comglue = concat $ map
            (\c ->
              case c of
                CommentA _ c -> unwords $ filter (\c -> c /= "/*" && c /= "*/") $ words c
                _ -> "") com
      
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


transform_css :: (Stream s m Char) => ParsecT s u m [Either ByteString [Char]]
transform_css = do
  l1 <- map Left <$> blabla
  l2 <- map Right <$> funs
  e <- try (eof >> return True) <|> (return False)
  case e of
    True -> return (l1++l2)
    False -> do
      l <- transform_css
      return (l1 ++ l2 ++ l)

  where

    symbol = P.symbol l
    lexeme = P.lexeme l

    string  = lexeme (
      between (char '\'') (char '\'') (strchars '\'') <|>
      between (char '"') (char '"') (strchars '"')) <|>
      manyTill anyChar (try (char ')'))
      where
        strchars e = many $ satisfy (/=e)

    fun1 = lexeme $ do
      symbol "url"
      symbol "("
      s <- string
      symbol ")"
      return s

    blabla = do
      l <- manyTill anyChar (eof <|> (try (lookAhead fun1) >> return ()))
      case null l of
        True -> return []
        False -> return [BS.pack l]

    funs = many (try fun1)

    l =  P.makeTokenParser $ P.LanguageDef
        { P.commentStart	 = "/*"
        , P.commentEnd	 = "*/"
        , P.commentLine	 = "//"
        , P.nestedComments = True
        , P.identStart	 = P.letter
        , P.identLetter	 = P.alphaNum <|> oneOf "_@-"
        , P.reservedNames   = []
        , P.reservedOpNames = []
        , P.caseSensitive  = False
        , P.opStart        = l
        , P.opLetter       = l
        }
        where l = oneOf ":!#$%&*+./<=>?@\\^|-~"

parse_css :: (Monad m) => BS.ByteString -> (String -> m String) -> m (Either P.ParseError BS.ByteString)
parse_css inp f = do
  case P.runParser transform_css () "-" inp of
    Left e -> return $ Left e
    Right pr -> do
      b <- forM pr $ \i -> do
        case i of
          Left bs -> return bs
          Right u -> do
            u' <- f u
            return (BS.pack $ "url('" ++ u' ++ "')")
      return $ Right $ BS.concat b

