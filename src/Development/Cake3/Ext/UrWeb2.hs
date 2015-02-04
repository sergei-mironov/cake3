{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module Development.Cake3.Ext.UrWeb2 where

import Data.Data
import Data.Char
import Data.Maybe
import Data.Foldable (Foldable(..), foldl')
import qualified Data.Foldable as F
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Text.Printf

import qualified System.FilePath as F

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
                 | UrpLink File String -- ^ File.o to link, additional linker flags
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
  -- tempfiles :: x -> [File]
  -- tempfiles = (\x -> (urpObjs x) ++ maybeToList (urpSql' x) ++ maybeToList (urpExe' x)) . toUrp

instance UrpLike Urp where
  toUrp = id

instance UrpLike UWLib where
  toUrp (UWLib x) = x
instance UrpLike UWExe where
  toUrp (UWExe x) = x

urpDeps :: Urp -> [File]
urpDeps (Urp _ _ hdr mod) = foldl' scan2 (foldl' scan1 mempty hdr) mod where
  scan1 a (UrpLink f _) = f:a
  scan1 a (UrpInclude f) = f:a
  scan1 a (UrpLibrary f) = f:a
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
  scan a (UrpSrc f cfl lfl) = (f,cfl,lfl):a
  scan a _ = a

-- urpObjs (Urp _ _ hdr _) = foldl' scan [] hdr where
--   scan a (UrpSrc f _ _) = (f.="o"):a
--   scan a (UrpLink f _) = f:a
--   scan a _ = a

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
  toUrpLine up (UrpLink f []) = printf "link %s" (up </> toFilePath f)
  toUrpLine up (UrpLink f lfl) = printf "link %s\nlink %s" (up </> toFilePath f) lfl
  toUrpLine up (UrpSrc f _ _) = printf "link %s" (up </> toFilePath (f.="o"))
  toUrpLine up (UrpPkgConfig s) = printf "link %s" (maskPkgCfg s)
  toUrpLine up (UrpFFI s) = printf "ffi %s" (up </> toFilePath (dropExtensions s))
  toUrpLine up (UrpSafeGet s) = printf "safeGet %s" (dropExtensions s)
  toUrpLine up (UrpJSFunc s1 s2 s3) = printf "jsFunc %s.%s = %s" s1 s2 s3
  toUrpLine up (UrpScript s) = printf "script %s" s
  toUrpLine up (UrpClientOnly s) = printf "clientOnly %s" s

instance ToUrpLine UrpModToken where
  toUrpLine up (UrpModule1 f) = up </> toFilePath (dropExtensions f)
  toUrpLine up (UrpModule2 f f2)
    | (dropExtensions f) == (dropExtensions f2) = up </> toFilePath (dropExtensions f)
    | otherwise = error $ printf "ur: File names should match, got %s, %s" (toFilePath f) (toFilePath f2)
  toUrpLine up (UrpModuleSys s) = printf "$/%s" s

newtype UrpGen m a = UrpGen { unUrpGen :: StateT UrpState m a }
  deriving(Functor, Applicative, Monad, MonadState UrpState, MonadMake, MonadIO)

runUrpGen :: (Monad m) => File -> UrpGen m a -> m (a,UrpState)
runUrpGen f m = runStateT (unUrpGen m) (defState f)

tempPrefix :: File -> String
tempPrefix f = concat $ map (map nodot) $ splitDirectories f where
  nodot '.' = '_'
  nodot '/' = '_'
  nodot a = a

-- | Produce fixed-content rule using @f as a uniq name template, add additional
-- dependencies @ds
genIn :: File -> [File] -> Writer String a -> Make File
genIn f ds wr = genFile' (tmp_file (tempPrefix f)) (execWriter $ wr) (forM_ ds depend)

line :: (MonadWriter String m) => String -> m ()
line s = tell (s++"\n")

urincl = makevar "UR_INCL" "-I$(shell urweb -print-cinclude) " 
gcc = makevar "UR_CC" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=gcc)"
gxx = makevar "UR_CPP" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=g++)"
urcflags = extvar "UR_CFLAGS"

uwlib :: File -> UrpGen (Make' IO) () -> Make UWLib
uwlib urpfile m = do
  ((),s) <- runUrpGen urpfile m
  let u@(Urp _ _ hdr mod) = urpst s
  let pkgcfg = (urpPkgCfg u)

  os <- forM (urpSrcs u) $ \(src,cfl,lfl) -> do
    let cflags = string $ concat $ cfl : map (\p -> printf "$(shell pkg-config --cflags %s) " p) (urpPkgCfg u)
    o <- (case takeExtension src of
      ".cpp" -> do
        rule' $ shell1 [cmd| $gxx -c $urcflags $urincl $cflags -o @(src .= "o") $src |]
      ".c" -> do
        rule' $ shell1 [cmd| $gcc -c $urincl $urcflags $cflags -o @(src .= "o") $src |]
      e -> fail ("Source type not supported (by extension) " ++ (toFilePath src)))
    return $ UrpLink (snd o) lfl

  urpfile' <- genIn (urpfile .= "in") (urpDeps u) $ do
    forM hdr (line . toUrpLine (urpUp urpfile))
    forM os (line . toUrpLine (urpUp urpfile))
    line ""
    forM mod (line . toUrpLine (urpUp urpfile))

  rule' $ do
    let cpy = [cmd|cat $urpfile'|] :: CommandGen' (Make' IO)
    let l = foldl'
            (\a p -> do
              let l = makevar (map toUpper $ printf "lib%s" p) (printf "$(shell pkg-config --libs %s)" p)
              [cmd| $a | sed 's@@$(string $ maskPkgCfg p)@@$l@@'  |]
            ) cpy pkgcfg
    shell [cmd| $l > @urpfile |]

  return $ UWLib u

uwapp :: String -> File -> UrpGen (Make' IO) () -> Make UWExe
uwapp uwflags urpfile m = do
  (UWLib u') <- uwlib urpfile m
  let u = u' { uexe = Just (urpfile .= "exe") }
  rule' $ do
    depend urpfile
    produce (urpExe u)
    case urpSql' u of
      Nothing -> return ()
      Just sql -> produce sql
    depend (makevar "URVERSION" "$(shell urweb -version)")
    unsafeShell [cmd|urweb $(string uwflags) $((takeDirectory urpfile)</>(takeBaseName urpfile))|]
  return $ UWExe u

addHdr h = modify $ \s -> let u = urpst s in s { urpst = u { uhdr = (uhdr u) ++ [h] } }

database :: (MonadMake m) => String -> UrpGen m ()
database dbs = addHdr $ UrpDatabase dbs

allow :: (MonadMake m) => UrpAllow -> String -> UrpGen m ()
allow a s = addHdr $ UrpAllow a s

rewrite :: (MonadMake m) => UrpRewrite -> String -> UrpGen m ()
rewrite a s = addHdr $ UrpRewrite a s

urpUp :: File -> FilePath
urpUp f = F.joinPath $ map (const "..") $ filter (/= ".") $ F.splitDirectories $ F.takeDirectory $ toFilePath f

class LibraryDecl x where
  library :: (MonadMake m) => x -> UrpGen m ()

instance LibraryDecl [File] where
  library  ls = do
    forM_ ls $ \l -> do
      when ((takeExtension l) /= ".urp") $ do
        fail $ printf "library declaration '%s' should ends with '.urp'" (toFilePath l)
      addHdr $ UrpLibrary l

instance LibraryDecl UWLib where
  library (UWLib u) = library [urp u]

instance LibraryDecl x => LibraryDecl (Make x) where
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


addMod :: (Monad m) => UrpModToken -> UrpGen m ()
addMod m = modify $ \s -> let u = urpst s in s { urpst = u { umod = (umod u) ++ [m] } }

class ModuleDecl x where
  ur :: (Monad m) => x -> UrpGen m ()
instance ModuleDecl File where
  ur = addMod . UrpModule1
instance ModuleDecl (File,File) where
  ur (f1,f2) = addMod $ UrpModule2 f1 f2
instance ModuleDecl String where
  ur = addMod .UrpModuleSys


debug :: (MonadMake m) => UrpGen m ()
debug = addHdr UrpDebug

include :: (MonadMake m) => File -> UrpGen m ()
include = addHdr . UrpInclude


class LinkDecl x where
  link :: (Monad m) => x -> UrpGen m () 

instance LinkDecl (File,String) where
  link (f,fl) = addHdr $ UrpLink f fl

instance LinkDecl File where
  link f = link (f,"")


class SrcDecl x where
  src :: (Monad m) => x -> UrpGen m ()

instance SrcDecl (File,String,String) where
  src (f,cfl,lfl) = addHdr $ UrpSrc f cfl lfl

instance SrcDecl File where
  src f = src (f,"","")



ffi :: (MonadMake m) => File -> UrpGen m ()
ffi = addHdr . UrpFFI

sql :: (MonadMake m) => File -> UrpGen m ()
sql = addHdr . UrpSql
  
jsFunc m u j = addHdr $ UrpJSFunc m u j

safeGet :: (MonadMake m) => String -> UrpGen m ()
safeGet = addHdr . UrpSafeGet

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
script = addHdr . UrpScript

pkgconfig :: (MonadMake m) => String -> UrpGen m ()
pkgconfig = addHdr . UrpPkgConfig

-- t1 :: Make ((),UrpState)
-- t1 = runUrpGen (file "Script.urp") $ do
--     return ()

-- t2 = uwlib (file "Script.urp") $ do
--     ffi (file "Script.urs")
--     include (file "Script.h")
--     link (file "Script.o")
--     pkgconfig "jansson"

-- file = file' (ProjectLocation "." ".") 

