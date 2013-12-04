{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module System.FilePath.Wrapper where

import Data.Data
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import qualified System.FilePath as F hiding ((</>))
import qualified Data.Map as M
import Data.Monoid
import Text.Printf

newtype FileT a = FileT a
  deriving(Show,Eq,Ord,Data,Typeable)

-- | Simple wrapper for FilePath.
type File = FileT FilePath

-- | Convert File back to FilePath
toFilePath :: (FileT FilePath) -> FilePath
toFilePath (FileT f) = f

fromFilePath :: FilePath -> FileT FilePath
fromFilePath f = FileT f

instance (Monoid a) => Monoid (FileT a) where
  mempty = FileT mempty
  mappend (FileT a) (FileT b) = FileT (a`mappend`b)

class FileLike a where
  -- fromFilePath :: FilePath -> a
  combine :: a -> String -> a
  takeDirectory :: a -> a
  takeBaseName :: a -> String
  takeFileName :: a -> String
  makeRelative :: a -> a -> a
  replaceExtension :: a -> String -> a
  takeExtension :: a -> String
  takeExtensions :: a -> String
  dropExtensions :: a -> a

-- | Redefine standard @</>@ operator to work with Files
(</>) :: (FileLike a) => a -> String -> a
(</>) = combine

-- | Alias for replaceExtension
(.=) :: (FileLike a) => a -> String -> a
(.=) = replaceExtension

instance FileLike a => FileLike (FileT a) where
  -- fromFilePath fp = FileT (fromFilePath fp)
  combine (FileT a) b = FileT (combine a b)
  takeBaseName (FileT a) = takeBaseName a
  takeFileName (FileT a) = takeFileName a
  takeExtension (FileT a) = takeExtension a
  takeExtensions (FileT a) = takeExtensions a
  makeRelative (FileT a) (FileT b) = FileT (makeRelative a b)
  replaceExtension (FileT a) ext = FileT (replaceExtension a ext)
  takeDirectory (FileT a) = FileT (takeDirectory a)
  dropExtensions (FileT a) = FileT (dropExtensions a)

instance FileLike FilePath where
  -- fromFilePath = id
  combine = F.combine
  takeBaseName = F.takeBaseName
  takeFileName = F.takeFileName
  makeRelative = F.makeRelative
  replaceExtension = F.replaceExtension
  takeDirectory = F.takeDirectory
  takeExtension = F.takeExtension
  takeExtensions = F.takeExtensions
  dropExtensions = F.dropExtensions

