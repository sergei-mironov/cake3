module Main where

import System.Environment
import System.Process
import System.Exit
import Paths_cake3

main = do
  args <- getArgs
  src <- getDataFileName "CakeScript.sh"
  (_,_,_,ph) <- createProcess $ CreateProcess
    { cmdspec = RawCommand "sh" (src : args)
    , cwd = Nothing
    , env = Nothing
    , std_in = Inherit
    , std_out = Inherit
    , std_err = Inherit
    , close_fds = True
    , create_group = False
    }
  ret <- waitForProcess ph
  exitWith ret
