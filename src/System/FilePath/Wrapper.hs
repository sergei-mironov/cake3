module System.FilePath.Wrapper where

import qualified System.FilePath as F hiding ((</>))

newtype File = File FilePath
  deriving(Show,Eq,Ord)

unFile (File f) = f

class AsFile x where
  toFile :: x -> File

instance AsFile File where
  toFile = id

toFilePath :: (AsFile x) => x -> FilePath
toFilePath = unFile . toFile

(</>) :: (AsFile x, AsFile y) => x -> y -> File
(</>) a b = File $ (toFilePath a) `F.combine` (toFilePath b)

replaceExtension :: (AsFile x) => x -> String -> File
replaceExtension f ext = File (F.replaceExtension (toFilePath f) ext)

(.=) :: (AsFile x) => x -> String -> File
(.=) = replaceExtension

makeRelative :: (AsFile x, AsFile y)=> x -> y -> File
makeRelative x y = File (F.makeRelative (toFilePath x) (toFilePath y))

takeDirectory :: (AsFile x) => x -> File
takeDirectory x = File $ F.takeDirectory (toFilePath x)
