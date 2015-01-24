
module Development.Cake3.Utils.Slice
where

import Control.Applicative
import Control.Monad.Trans
import qualified Data.Set as S
import Data.Set (Set)

import System.Directory
import System.Environment
import System.Process
import System.Exit
import System.IO
import Text.Printf

import System.FilePath.Wrapper
import Development.Cake3
import Development.Cake3.Types
import Development.Cake3.Monad
import Development.Cake3.Writer

-- | Build the full Makefile named @fo@ and a set of 'sliced' versions.
-- 'Slicing' here means filtering out all rules which depends on certain tools
-- (second element of @sls@) and all upstream rules.
writeSliced :: File -> [(File,[Tool])] -> Make a -> IO ()
writeSliced fo sls mk = do
  cwd <- currentDirLocation
  ms <- evalMake fo mk
  runMakeH ms (writeFile (toFilePath fo))

  ecs <- forM sls $ \(fs,ts) -> do

    rs <- return $ filterRecipesByToolsDeep ts (recipes ms)
    ts <- return $ S.toList $ queryTargets rs

    putStrLn  $ printf "Executing: make -f %s %s" (escapeFile fo) (unwords $ map escapeFile ts)
    ec <- system $ printf "make -f %s %s" (escapeFile fo) (unwords $ map escapeFile ts)

    putStrLn  $ printf "Writing %s" (escapeFile fs)
    runMakeH ms{ recipes = (recipes ms) `S.difference` rs } (writeFile (toFilePath fs))

    return (ec,fs)

  forM_ ecs $ \(ec,sln) ->
    case ec of
      ExitFailure i -> fail $ printf "Non-zero exit code (%d) while building %s\n" (show ec) (escapeFile sln)
      _ -> return ()

