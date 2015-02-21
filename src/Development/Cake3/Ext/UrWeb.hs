{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE IncoherentInstances #-}
module Development.Cake3.Ext.UrWeb where

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
import Development.Cake3.Types
import Development.Cake3.Monad
import Development.Cake3 hiding (many, (<|>))
import Development.Cake3.Ext.UrEmbed.Types (uwModName,css_mangle_flag)

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

data SrcFile = SrcFile File String String
  deriving(Show,Data,Typeable)

data Urp = Urp {
    urp_ :: File
  , uexe :: Maybe File
  , uhdr :: [UrpHdrToken]
  , umod :: [UrpModToken]
  , srcs :: [SrcFile]
  , patches :: [File]
  } deriving(Show,Data,Typeable)

newtype UWLib = UWLib Urp
  deriving (Show,Data,Typeable)

newtype UWExe = UWExe Urp
  deriving (Show,Data,Typeable)

instance (MonadAction a m) => RefInput a m UWLib where
  refInput (UWLib u) = refInput (urp_ u)
 
instance (MonadAction a m) => RefInput a m UWExe where
  refInput (UWExe u) = refInput (urpExe u)
 
-- class UrpLike x where
--   toUrp :: x -> Urp

-- instance UrpLike Urp where
--   toUrp = id

-- instance UrpLike UWLib where
--   toUrp (UWLib x) = x
-- instance UrpLike UWExe where
--   toUrp (UWExe x) = x


class (Monad m) => AppLike m x where
  urp :: m x -> m File
  sqlf :: m x -> m File
  dbn :: m x -> m CakeString
  

instance (Monad m) => AppLike m UWExe where
  urp mu = mu >>= \(UWExe u) -> return (urp_ u)
  sqlf mu = mu >>= \(UWExe u) -> return (urpSql u)
  dbn mu = sqlf mu >>= \x -> return (string $ takeBaseName x)


urpDeps :: Urp -> [File]
urpDeps (Urp _ _ hdr mod srcs _) = foldl' scan2 (foldl' scan1 (foldl' scan3 mempty srcs) hdr) mod where
  scan1 a (UrpLink f _) = f:a
  scan1 a (UrpInclude f) = f:a
  scan1 a (UrpLibrary f) = f:a
  scan1 a _ = a
  scan2 a (UrpModule1 f) = f:a
  scan2 a (UrpModule2 f1 f2) = f1:f2:a
  scan2 a _ = a
  scan3 a (SrcFile f _ _) = (f.="o"):a

urpSql' :: Urp -> Maybe File
urpSql' (Urp _ _ hdr _ _ _) = find hdr where
  find [] = Nothing
  find ((UrpSql f):hs) = Just f
  find (h:hs) = find hs

urpSql :: Urp -> File
urpSql u = case urpSql' u of
  Nothing -> error "ur project defines no SQL file"
  Just sql -> sql

urpLibs (Urp _ _ hdr _ _ _) = foldl' scan [] hdr where
  scan a (UrpLibrary f) = f:a
  scan a _ = a

urpExe' = uexe
urpExe u = case uexe u of
  Nothing -> error "ur project defines no EXE file"
  Just exe -> exe

urpPkgCfg (Urp _ _ hdr _ _ _) = foldl' scan [] hdr where
  scan a (UrpPkgConfig s) = s:a
  scan a _ = a

data UrpState = UrpState {
    urpst :: Urp
  , urautogen :: String
  } deriving (Show)

defUrp f = Urp f Nothing [] [] [] []

defState f = UrpState (defUrp f) "autogen"

-- | Returns autogen dir for the current module's file
autogenDir :: (Monad m) => File -> UrpGen m File
autogenDir (FileT l@(ModuleLocation t2m m2t) path) = do
  ag <- (urautogen `liftM` get)
  return $ FileT l ag

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
  toUrpLine :: File -> a -> String

maskPkgCfg s = "%" ++ (map toUpper s) ++ "%"

instance ToUrpLine UrpHdrToken where
  toUrpLine t (UrpDatabase dbs) = printf "database %s" dbs
  toUrpLine t (UrpSql f) = printf "sql %s" (route t f)
  toUrpLine t (UrpAllow a s) = printf "allow %s %s" (toUrpWord a) s
  toUrpLine t (UrpRewrite a s) = printf "rewrite %s %s" (toUrpWord a) s
  toUrpLine t (UrpLibrary f)
    | (takeFileName f) == "lib.urp" = printf "library %s" (route t (takeDirectory f))
    | otherwise = printf "library %s" (route t (dropExtension f))
  toUrpLine t (UrpDebug) = printf "debug"
  toUrpLine t (UrpInclude f) = printf "include %s" (route t f)
  toUrpLine t (UrpLink f []) = printf "link %s" (route t f)
  toUrpLine t (UrpLink f lfl) = printf "link %s\nlink %s" (route t f) lfl
  toUrpLine t (UrpPkgConfig s) = printf "link %s" (maskPkgCfg s)
  toUrpLine t (UrpFFI s) = printf "ffi %s" (route t (dropExtensions s))
  toUrpLine t (UrpSafeGet s) = printf "safeGet %s" (dropExtensions s)
  toUrpLine t (UrpJSFunc s1 s2 s3) = printf "jsFunc %s.%s = %s" s1 s2 s3
  toUrpLine t (UrpScript s) = printf "script %s" s
  toUrpLine t (UrpClientOnly s) = printf "clientOnly %s" s

instance ToUrpLine UrpModToken where
  toUrpLine t (UrpModule1 f) = route t (dropExtensions f)
  toUrpLine t (UrpModule2 f f2)
    | (dropExtensions f) == (dropExtensions f2) = route t (dropExtensions f)
    | otherwise = error $ printf "ur: File names should match, got %s, %s" (route t f) (route t f2)
  toUrpLine t (UrpModuleSys s) = printf "$/%s" s

newtype UrpGen m a = UrpGen { unUrpGen :: StateT UrpState m a }
  deriving(Functor, Applicative, Monad, MonadState UrpState, MonadMake, MonadIO)


class (Monad m, Monad m1) => MonadUrpGen m1 m where
  liftUrpGen :: m1 a -> m a

instance (Monad m) => MonadUrpGen m (UrpGen m) where
  liftUrpGen m = UrpGen (lift m)


runUrpGen :: (Monad m) => File -> UrpGen m a -> m (a,UrpState)
runUrpGen f m = runStateT (unUrpGen m) (defState f)

tempPrefix :: File -> String
tempPrefix f = munglePath $ topRel f where

munglePath :: FilePath -> String
munglePath = map plain where
  plain a | (not $ isAlphaNum a) = '_'
          | otherwise = a

-- | Produce fixed-content rule using @f as a uniq name template, add additional
-- dependencies @ds
genIn :: File -> [File] -> Writer String a -> Make File
genIn f ds wr = genFile' (tmp_file (tempPrefix f)) (execWriter $ wr) (forM_ ds depend)

line :: (MonadWriter String m) => String -> m ()
line s = tell (s++"\n")

urincl = makevar "UR_INCL" "-I$(shell urweb -print-cinclude)" 
gcc = makevar "UR_CC" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=gcc)"
gxx = makevar "UR_CPP" "$(shell $(shell urweb -print-ccompiler) -print-prog-name=g++)"
urcflags = extvar "UR_CFLAGS"

uwlib :: File -> UrpGen (Make' IO) () -> Make UWLib
uwlib urpfile m = do
  ((),s) <- runUrpGen urpfile m
  let u@(Urp tgt _ hdr mod srcs ptch) = urpst s
  let pkgcfg = (urpPkgCfg u)

  os <- forM srcs $ \(SrcFile src cfl lfl) -> do
    let cflags = string $ concat $ cfl : map (\p -> printf "$(shell pkg-config --cflags %s) " p) (urpPkgCfg u)
    o <- (case takeExtension src of
      ".cpp" -> do
        rule' $ shell1 [cmd| $gxx -c $urcflags $urincl $cflags -o @(src .= "o") $src |]
      ".c" -> do
        rule' $ shell1 [cmd| $gcc -c $urincl $urcflags $cflags -o @(src .= "o") $src |]
      e -> fail ("Source type not supported (by extension) " ++ (topRel src)))
    return $ UrpLink (snd o) lfl

  urp1 <- genIn (urpfile .= "in.1") (urpDeps u) $ do
    forM hdr (line . toUrpLine tgt)
    forM os (line . toUrpLine tgt)

  urp2 <- genIn (urpfile .= "in.2") (urpDeps u) $ do
    line ""
    forM mod (line . toUrpLine tgt)

  rule' $ do
    let cpy = [cmd|cat $urp1 |] :: CommandGen' (Make' IO)
    let l = foldl'
            (\a p -> do
              let l = makevar (map toUpper $ printf "lib%s" p) (printf "$(shell pkg-config --libs %s)" p)
              [cmd| $a | sed 's@@$(string $ maskPkgCfg p)@@$l@@'  |]
            ) cpy pkgcfg
    shell [cmd| $l > @urpfile |]
    when (not $ null ptch) $ do
      shell [cmd| cat $ptch >> @urpfile |] >> return ()
    shell [cmd| cat $urp2 >> @urpfile |]

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

addHdr :: (Monad m) => UrpHdrToken -> UrpGen m ()
addHdr h = modify $ \s -> let u = urpst s in s { urpst = u { uhdr = (uhdr u) ++ [h] } }

addSrc :: (Monad m) => SrcFile -> UrpGen m ()
addSrc f = modify $ \s -> let u = urpst s in s { urpst = u { srcs = f : (srcs u) } }

addPatch :: (Monad m) => File -> UrpGen m ()
addPatch f = modify $ \s -> let u = urpst s in s { urpst = u { patches = f : (patches u) } }

database :: (Monad m) => String -> UrpGen m ()
database dbs = addHdr $ UrpDatabase dbs

allow :: (Monad m) => UrpAllow -> String -> UrpGen m ()
allow a s = addHdr $ UrpAllow a s

rewrite :: (Monad m) => UrpRewrite -> String -> UrpGen m ()
rewrite a s = addHdr $ UrpRewrite a s

-- addDot f = addDot' $ F.normalise f where
--   addDot' "." = "."
--   addDot' p = "."</>p

-- routeM :: (MonadMake m) => File -> UrpGen m File
-- routeM his = do
--   my <- urp `liftM` urpst `liftM` get
--   return $ route my his

-- Down from module to file
-- urpDown :: File -> FilePath
-- urpDown (FileT hint path) = F.makeRelative hint path

-- wayback x = F.joinPath $ map (const "..") $ filter (/= ".") $ F.splitDirectories $ F.takeDirectory x

-- -- Up to the Cake module's top dir
-- urpUp :: File -> FilePath
-- urpUp f = wayback $ mkrel f where
--   mkrel (FileT hint path) = F.makeRelative hint path

class LibraryDecl m x where
  library :: x -> UrpGen m ()
  
instance (Monad m) => LibraryDecl m File where
  library l = do
      when ((takeExtension l) /= ".urp") $ do
        fail $ printf "library declaration '%s' should ends with '.urp'" (topRel l)
      addHdr $ UrpLibrary l

instance (Monad m) => LibraryDecl m [File] where
  library  ls = forM_ ls library

instance (Monad m) => LibraryDecl m UWLib where
  library (UWLib u) = library (urp_ u)

instance (Monad m) => LibraryDecl m UWExe where
  library (UWExe u) = library (urp_ u)

instance (Monad m) => LibraryDecl m (m File) where
  library ml = (liftUrpGen ml) >>= library

instance (Monad m, LibraryDecl m x) => LibraryDecl m (m x) where
  library ml = (liftUrpGen ml) >>= library

-- | Build a file using external Makefile facility.
externalMake3 ::
     File -- ^ External Makefile
  -> File -- ^ External file to refer to
  -> String -- ^ The name of the target to run
  -> Make [File]
externalMake3 mk f tgt = do
  prebuildS [cmd|$(make) -C $(string $ topRel $ takeDirectory mk) -f $(string $ takeFileName mk) $(string tgt) |]
  return [f]

-- | Build a file using external Makefile facility.
externalMake' ::
     File -- ^ External Makefile
  -> File -- ^ External file to refer to
  -> Make [File]
externalMake' mk f = do
  prebuildS [cmd|$(make) -C $(string $ topRel $ takeDirectory mk) -f $(string $ takeFileName mk)|]
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

instance ModuleDecl UrpModToken where
  ur = addMod

instance ModuleDecl (File,File) where
  ur (f1,f2) = addMod $ UrpModule2 f1 f2

sys = UrpModuleSys

pair f = UrpModule2 (f.="ur") (f.="urs")

debug :: (Monad m) => UrpGen m ()
debug = addHdr UrpDebug

include :: (Monad m) => File -> UrpGen m ()
include = addHdr . UrpInclude


class LinkDecl x where
  link :: (MonadMake m) => x -> UrpGen m () 

instance LinkDecl (File,String) where
  link (f,fl) = addHdr $ UrpLink f fl

instance LinkDecl File where
  link f = addHdr $ UrpLink f ""

instance (LinkDecl x) => LinkDecl (Make' IO x) where
  link  ml = liftMake ml >>= link


class SrcDecl x where
  src :: (MonadMake m) => x -> UrpGen m ()

instance SrcDecl (File,String,String) where
  src (f,cfl,lfl) = addSrc $ SrcFile f cfl lfl

instance SrcDecl File where
  src f = src (f,"","")

instance SrcDecl x => SrcDecl (Make x) where
  src  ml = liftMake ml >>= src



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

urembed = tool "urembed"

embed' :: (MonadMake m) => [String] -> Bool -> File -> UrpGen m ()
embed' ueo' js_ffi f = do
  let ueo = unwords $ map ("--" ++) ueo'
  a <- autogenDir f
  let intermed f suffix ext = (a </> ((munglePath (takeFileName f)) ++ suffix)) .= ext
  let c = intermed f "_c" "c"
  let h = intermed f "_c" "h"
  let s = intermed f "_c" "urs"
  let w = intermed f "" "ur"
  j <- (if js_ffi then do
         let j = intermed f "_js" "urs"
         ffi j
         return ([cmd| -j @j |] :: CommandGen' (Make' IO))
       else
         return [cmd||])

  let p = intermed f "" "urp.in"
  addPatch p
  rule' $ do
    shell [cmd|mkdir -p $(string $ topRel a) 2>/dev/null|]
    shell [cmd|$urembed $(string ueo) -c @c -H @h -s @s -w @w $j $f > @p|]
  o <- snd `liftM` (rule' $ shell1 [cmd| $gcc -c $urincl -o @(c .= "o") $c |])
  ffi s
  include h
  link o
  ur w

  -- u <- urp `liftM` urpst `liftM` get
  -- liftMake $ liftIO $ do
  --   putStrLn $ show a
  --   putStrLn $ show u
  --   putStrLn $ show w
  --   putStrLn $ route u w

class EmbedDecl x where
  embed :: (MonadMake m) => x -> UrpGen m ()

instance EmbedDecl File where
  embed = embed' [] False

data Mangled_File = CSS_File File | JS_File File

mangled :: File -> Make Mangled_File
mangled f
  | (takeExtension f) == ".css" = return $ CSS_File f
  | (takeExtension f) == ".js" = return $ JS_File f
  | otherwise = fail $ "mangled: Mangling is defined for .css and .js files only (got " ++ topRel f ++ ")"

instance EmbedDecl Mangled_File where
  embed (CSS_File f) = embed' [css_mangle_flag] False f
  embed (JS_File f) = embed' [] True f

instance EmbedDecl x => EmbedDecl (Make x) where
  embed ml = liftMake ml >>= embed

-- TESTS

-- t1 :: Make ((),UrpState)
-- t1 = runUrpGen (file "Script.urp") $ do
--     return ()

-- t2 = uwlib (file "Script.urp") $ do
--     ffi (file "Script.urs")
--     include (file "Script.h")
--     link (file "Script.o")
--     pkgconfig "jansson"

-- file = file' (ProjectLocation "." ".") 

