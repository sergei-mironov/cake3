module Main where

import System.Process
import Paths_thirdcake

main = getDataFileName "CakeScript.sh" >>= system . ("sh " ++ )

