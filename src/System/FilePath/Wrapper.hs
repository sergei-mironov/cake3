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

data ReactFile m = ReactFile Tag (m FilePath)

instance Show (ReactFile m) where
  show (ReactFile t _) = printf "ReactFile { %s , IO_ACTION }" (show t)

instance Eq (ReactFile m) where
  (ReactFile a _) == (ReactFile b _) = a == b

instance Ord (ReactFile m) where
  (ReactFile a _) `compare` (ReactFile b _) = a `compare` b

class FileLike a where
  fromFilePath :: FilePath -> a
  label :: a -> String
  combine :: a -> a -> a
  takeBaseName :: a -> a
  makeRelative :: a -> a -> a
  replaceExtension :: a -> String -> a

(</>) :: (FileLike a) => a -> a -> a
(</>) = combine

(.=) :: (FileLike a) => a -> String -> a
(.=) = replaceExtension

instance FileLike a => FileLike (FileT a) where
  fromFilePath fp = FileT (fromFilePath fp)
  label (FileT a) = label a
  combine (FileT a) (FileT b) = FileT (combine a b)
  takeBaseName (FileT a) = FileT (takeBaseName a)
  makeRelative (FileT a) (FileT b) = FileT (makeRelative a b)
  replaceExtension (FileT a) ext = FileT (replaceExtension a ext)

instance FileLike FilePath where
  fromFilePath = id
  label = id
  combine = F.combine
  takeBaseName = F.takeBaseName
  makeRelative = F.makeRelative
  replaceExtension = F.replaceExtension

unpack :: (FileT FilePath) -> FilePath
unpack (FileT f) = f

tag1 fn a = printf "%s(%s)" fn (label a)
tag2 fn a b = printf "%s(%s,%s)" fn (label a) (label b)

instance (Applicative m) => FileLike (ReactFile m) where
  fromFilePath fp = ReactFile fp (pure fp)
  label (ReactFile t _) = t
  combine a@(ReactFile ta ma) b@(ReactFile tb mb) = ReactFile (tag2 "combine" a b) (F.combine<$> ma <*> mb)
  takeBaseName a@(ReactFile ta ma) = ReactFile (tag1 "basename" a) (takeBaseName <$> ma)
  makeRelative a@(ReactFile ta ma) b@(ReactFile tb mb) = ReactFile (tag2 "rel" a b) (F.makeRelative <$> ma <*> mb)
  replaceExtension a@(ReactFile ta ma) ext = ReactFile (tag2 "newext" a ext) (F.replaceExtension <$> ma <*> pure ext)

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

reactive :: (Applicative m) => String -> FileT (ReactFile m) -> (FilePath -> FileT (ReactFile m) -> m FilePath) -> FileT (ReactFile m)
reactive opname f q = FileT (ReactFile (tag1 opname f) (q [] f))
