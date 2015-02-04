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

data FileT h a = FileT h a
  deriving(Show,Eq,Ord,Data,Typeable)

-- | Convert File back to FilePath
toFilePath :: (FileT h FilePath) -> FilePath
toFilePath (FileT _ f) = f

fromFilePath :: h -> FilePath -> FileT h FilePath
fromFilePath h f = FileT h f

-- | Convert File back to FilePath with escaped spaces
escapeFile :: FileT h FilePath -> FilePath
escapeFile f = escapeFile' (toFilePath f) where
  escapeFile' [] = []
  escapeFile' (' ':xs) = "\\ " ++ escapeFile' xs
  escapeFile' (x:xs) = (x:(escapeFile' xs))

-- instance (Monoid a, Monoid h) => Monoid (FileT h a) where
--   mempty = FileT mempty mempty
--   mappend (FileT h1 a) (FileT h2 b) = FileT (a`mappend`b)

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
  dropExtension :: a -> a
  splitDirectories :: a -> [String]

-- | Redefine standard @</>@ operator to work with Files
(</>) :: (FileLike a) => a -> String -> a
(</>) = combine

-- | Alias for replaceExtension
(.=) :: (FileLike a) => a -> String -> a
(.=) = replaceExtension

instance (Eq h, Show h, FileLike a) => FileLike (FileT h a) where
  -- fromFilePath fp = FileT (fromFilePath fp)
  combine (FileT h a) b = FileT h (combine a b)
  takeBaseName (FileT _ a) = takeBaseName a
  takeFileName (FileT _ a) = takeFileName a
  takeExtension (FileT _ a) = takeExtension a
  takeExtensions (FileT _ a) = takeExtensions a
  makeRelative (FileT h1 a) (FileT h2 b)
    | h1 == h2 = FileT h1 (makeRelative a b)
    | otherwise = error $ "makeRelative: FileT, hints are different: " ++ (show h1) ++ " <> " ++ (show h2)
  replaceExtension (FileT h a) ext = FileT h (replaceExtension a ext)
  takeDirectory (FileT h a) = FileT h (takeDirectory a)
  dropExtensions (FileT h a) = FileT h (dropExtensions a)
  dropExtension (FileT h a) = FileT h (dropExtension a)
  splitDirectories (FileT _ a) = splitDirectories a

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
  dropExtension = F.dropExtension
  splitDirectories = F.splitDirectories

