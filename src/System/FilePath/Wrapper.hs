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

instance (Monoid a) => Monoid (FileT a) where
  mempty = FileT mempty
  mappend (FileT a) (FileT b) = FileT (a`mappend`b)

type File = FileT FilePath

class FileLike a where
  fromFilePath :: FilePath -> a
  combine :: a -> a -> a
  takeBaseName :: a -> a
  takeFileName :: a -> a
  makeRelative :: a -> a -> a
  replaceExtension :: a -> String -> a
  takeExtension :: a -> String
  takeExtensions :: a -> String
  takeDirectory :: a -> a
  dropExtensions :: a -> a

(</>) :: (FileLike a) => a -> a -> a
(</>) = combine

(.=) :: (FileLike a) => a -> String -> a
(.=) = replaceExtension

instance FileLike a => FileLike (FileT a) where
  fromFilePath fp = FileT (fromFilePath fp)
  combine (FileT a) (FileT b) = FileT (combine a b)
  takeBaseName (FileT a) = FileT (takeBaseName a)
  takeFileName (FileT a) = FileT (takeFileName a)
  takeExtension (FileT a) = takeExtension a
  takeExtensions (FileT a) = takeExtensions a
  makeRelative (FileT a) (FileT b) = FileT (makeRelative a b)
  replaceExtension (FileT a) ext = FileT (replaceExtension a ext)
  takeDirectory (FileT a) = FileT (takeDirectory a)
  dropExtensions (FileT a) = FileT (dropExtensions a)

instance FileLike FilePath where
  fromFilePath = id
  combine = F.combine
  takeBaseName = F.takeBaseName
  takeFileName = F.takeFileName
  makeRelative = F.makeRelative
  replaceExtension = F.replaceExtension
  takeDirectory = F.takeDirectory
  takeExtension = F.takeExtension
  takeExtensions = F.takeExtensions
  dropExtensions = F.dropExtensions

unpack :: (FileT FilePath) -> FilePath
unpack (FileT f) = f

toFilePath = unpack

