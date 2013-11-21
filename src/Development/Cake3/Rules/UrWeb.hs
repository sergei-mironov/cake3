{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Development.Cake3.Rules.UrWeb(
    withUrp
  , Config(..)
  , Urp(..)
  , defaultConfig
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

urincl = makevar "URINCL" "$(shell urweb -print-cinclude)"

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

-- | Helper function, parses dependencies of an *urp
urdeps :: Config -> File -> A ()
urdeps cfg f = do
  let check = msum [ lookup (unpack (takeBaseName f)) (urEmbed cfg)
                   , lookup (unpack (takeFileName f)) (urEmbed cfg)
                   ]
  case check of
    Just embeddable -> do
      r <- liftMake (urembed f embeddable)
      depend r
    Nothing -> do
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
        let cc = makevar "URCC" "$(shell urweb -print-ccompiler)"
        (r,_) <- liftMake $ rule' $ do
          shell [cmd|$(cc) -c -I $urincl -o %(relative x) @((relative x) .= "c")|]
        depend r
      | otherwise = return ()

    src d@(c:_)
      | (c/='$') = do
        depend ((relative d) .= "ur" :: File)
        let urs = (relative d) .= "urs"
        e <- liftIO $ doesFileExist (toFilePath urs)
        when e $ do
          depend ((relative d) .= "urs" :: File)
      | otherwise = return ()
    src _ = return ()

-- | Search for @sect@ in the urp file's header.
urpline :: String -> File -> String -> File
urpline sect f c = flip execState mempty $ urpparse c lib (const $ return ()) where
  relative x = takeDirectory f </> (fromFilePath x)
  lib (n,x) | n == sect = put (relative x)
            | otherwise = return ()

-- | Search for section @sect@ in the urp file's database line
-- FIXME: actually supports only 'databse dbname=XXX' format
urpdb :: String -> File -> String -> File
urpdb dbsect f c = flip execState mempty $ urpparse c lib (const $ return ()) where
  relative x = takeDirectory f </> (fromFilePath x)
  lib (n,x) | n == "database" = put (relative $ snd (splitWhen '=' x))
            | otherwise = return ()

-- | Get executable name of an URP project
urpexe :: File -> File
urpexe f = f .= "exe"

data Config = Config {
    urEmbed :: [(String, [File])]
  , urFlags :: CommandGen
  }

defaultConfig = Config {
    urEmbed = []
  , urFlags = commandGen (return [])
  }


data Urp = Urp {
    urweb :: Config -> Make (File,File)
  , urdb :: File
  } |
  UrpWithoutDB {
    urwebCmd :: CommandGen -> Make Recipe
  }

-- | Take the URP file and the build action. Provide three aliases: one for
-- executable, one for SQL-file and one for database file
withUrp :: File -> (Urp -> Make a) -> Make a
withUrp f h = do
  c <- liftIO (IO.readFile $ unpack f)
  h Urp{
      urweb = \cfg -> do
        let exe = urpexe f
        let sql = urpline "sql" f c
        snd <$> (rule' $ do
          produce exe
          produce sql
          urdeps cfg f
          shell [cmd|urweb $(urFlags cfg) $(string $ takeBaseName f)|]
          return (exe,sql))
    , urdb = urpdb "name" f c
    }

-- | Generate Ur/Web project file @urp@ providing embedded files @files@
-- FIXME: Generate unique variable name instead of URGCC
-- FIXME: implement variable dependency
urembed :: File -> [File] -> Make Recipe
urembed urp files =
  let
  dir = takeDirectory urp
  mf = (dir </> takeBaseName urp) .= "mk"
  dir_ = string dir
  urp_ = string urp
  mf_ = string (takeFileName mf)
  in do
    (mf, _) <- rule' $ do
      shell [cmd|mkdir -pv $(dir_)|]
      shell [cmd|urembed -o $(urp_) @files|]
      produce mf

    (r, _) <- rule' $ do
      let urcc = makevar "URCC" "$(shell urweb -print-ccompiler)"
      let gcc = makevar "CC" "$(shell $(URCC) -print-prog-name=gcc)"
      let ld = makevar "LD" "$(shell $(URCC) -print-prog-name=ld)"
      merge urcc
      depend mf
      shell [cmd|$(extvar "MAKE") -C $(dir_) -f $(mf_) CC=$gcc LD=$ld UR_INCLUDE_DIR=$(urincl) urp|]
      produce urp
    return r

