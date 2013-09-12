{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module System.FilePath.Wrapper where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import qualified System.FilePath as F hiding ((</>))
import qualified Data.Map as M
import Data.Monoid
import Text.Printf

newtype FileT a = FileT a
  deriving(Show,Eq,Ord)

type Tag = String

data ReactFile m = ReactFile (m FilePath)

instance Show (ReactFile m) where
  show (ReactFile _) = "ReactFile { IO_ACTION }"

class FileLike a where
  fromFilePath :: FilePath -> a
  combine :: a -> a -> a
  takeBaseName :: a -> a
  makeRelative :: a -> a -> a
  replaceExtension :: a -> String -> a
  takeDirectory :: a -> a

(</>) :: (FileLike a) => a -> a -> a
(</>) = combine

(.=) :: (FileLike a) => a -> String -> a
(.=) = replaceExtension

instance FileLike a => FileLike (FileT a) where
  fromFilePath fp = FileT (fromFilePath fp)
  combine (FileT a) (FileT b) = FileT (combine a b)
  takeBaseName (FileT a) = FileT (takeBaseName a)
  makeRelative (FileT a) (FileT b) = FileT (makeRelative a b)
  replaceExtension (FileT a) ext = FileT (replaceExtension a ext)
  takeDirectory (FileT a) = FileT (takeDirectory a)

instance FileLike FilePath where
  fromFilePath = id
  combine = F.combine
  takeBaseName = F.takeBaseName
  makeRelative = F.makeRelative
  replaceExtension = F.replaceExtension
  takeDirectory = F.takeDirectory

unpack :: (FileT FilePath) -> FilePath
unpack (FileT f) = f

-- tag1 fn a = printf "%s(%s)" fn (label a)
-- tag2 fn a b = printf "%s(%s,%s)" fn (label a) (label b)

instance (Applicative m) => FileLike (ReactFile m) where
  fromFilePath fp = ReactFile (pure fp)
  combine a@(ReactFile ma) b@(ReactFile mb) = ReactFile (F.combine<$> ma <*> mb)
  takeBaseName a@(ReactFile ma) = ReactFile (takeBaseName <$> ma)
  makeRelative a@(ReactFile ma) b@(ReactFile mb) = ReactFile (F.makeRelative <$> ma <*> mb)
  replaceExtension a@(ReactFile ma) ext = ReactFile (F.replaceExtension <$> ma <*> pure ext)
  takeDirectory a@(ReactFile ma) = ReactFile (F.takeDirectory <$> ma)

type FileCache = M.Map String (FileT FilePath)

emptyFileCache :: FileCache
emptyFileCache = M.empty

class (Applicative m, Applicative m1, Monad m, Monad m1) => FileCacheMonad m m1 where
  cache :: FileT (ReactFile m1) -> m (FileT FilePath)
  readCachedFile :: FileT (ReactFile m1) -> m (Maybe String)

accessContents :: (FileCacheMonad m m1) => (String -> m a) -> a -> FileT (ReactFile m1) -> m a
accessContents act def f = readCachedFile f >>= maybe (return def) act

accessContents_ :: (FileCacheMonad m m1) => FileT (ReactFile m1) -> (String -> m ()) -> m ()
accessContents_ f act = accessContents act () f

reactive :: (Applicative m) => FileT (ReactFile m) -> (FilePath -> FileT (ReactFile m) -> m FilePath) -> FileT (ReactFile m)
reactive f q = FileT (ReactFile (q [] f))


