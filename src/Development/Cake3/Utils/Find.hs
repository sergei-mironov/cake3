module Development.Cake3.Utils.Find where

import Control.Applicative
import Control.Monad.Trans
import System.Directory
import System.IO.Unsafe (unsafeInterleaveIO)

import System.FilePath.Wrapper
import Development.Cake3
import Development.Cake3.Types
import Development.Cake3.Monad


filterExts :: [String] -> [File] -> [File]
filterExts exts files = filter (\f -> or $ map (isExt f) exts) files where
  isExt f e = takeExtension f == (sureDot e)
  sureDot [] = error "filterExt: empty extantion passed"
  sureDot e@('.':cs) = e
  sureDot e@(c:cs) = '.':e

-- FIMXE: Find a way to record dependencies like
-- FIXME: Figure out how to add ./relative notation (./file instead of file)
-- Makefile : contents_of(directory)
getDirectoryContentsRecursive :: (MonadIO m) => File -> m [File]
getDirectoryContentsRecursive (FileT topdir) = map fromFilePath `liftM` (liftIO $ recurseDirectories [""])
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files, reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore _          = False
